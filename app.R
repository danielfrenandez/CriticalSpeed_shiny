###load libraries

library(shiny)
library(bslib)
library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(data.table)
library(formattable)
library(gghighlight)
library(hrbrthemes)
library(gghighlight)
library(gt)
library(DT)
library(tidyr)
library(purrr)
library(viridis)
library(stringr)
library(RcppRoll)
library(tibble)
library(stringr)
library(scales)
library(hrbrthemes)
library(patchwork)
library(RcppRoll)

load("output/database_maximums_var.Rdata")
df_final_maximums$date <- as.Date(df_final_maximums$date)
#df_final_maximums <- df_final_maximums %>%
  #mutate(player_short = substr(as.character(player_name), 1, 9))  # primeres 8 lletres

options(shiny.maxRequestSize = 1024 * 1024^2)

###Define UI
ui <- fluidPage(
  titlePanel("Critical Speed Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput( 
        "select", 
        "Select MIP date range", 
        choices = c("last month" = "last_month",
          "last 3 months" = "last_3_months",
          "last 6 months" = "last_6_months",
          "all data" = "all_data",
          "personalized range" = "personalized"),
        selected = "all_data"
      ),
      conditionalPanel(
        condition = "input.select == 'personalized'",
        sliderInput(
          inputId =  "slider",
          label = "Date start and end",
          min = min(df_final_maximums$date),
          max = max(df_final_maximums$date), 
          value = c(min(df_final_maximums$date), max(df_final_maximums$date))
        )
      ),
      selectInput( 
        "athlete", 
        "Select Athlete:", 
        choices = unique(df_final_maximums$player_name)
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'MIPs vs. session'",
        selectInput( 
          "training", 
          "Select session", 
          choices = NULL
        )
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Session analysis'",
        fileInput("file1", "Choose a File")
      ),
      width = 3
    ),
    mainPanel(
      navset_tab(
        id = "tabs_main",
        nav_panel("MIPs vs. team",
                  div(style = "text-align:center;",
                    plotOutput("plot_mipsteam", width = "100%"),
                    br())
                  ),
        nav_panel("MIPs vs. session",
                  div(style = "text-align:center;",
                      plotOutput("plot", width = "100%"),
                      br(),
                      h4("% respect most intense periods selected"),
                      div(
                        style = "display: flex;
                      justify-content: center;
                      align-items: center;
                      width: 100%;",
                        tableOutput("summary_table")
            )
          )
        ),
        nav_panel("Session analysis",
                  div(style = "text-align:center;",
                      plotOutput("plot_session", height = "100vh", width = "100%"),
                      br())
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Quan canvia l'athlete, actualitza les sessions disponibles
  observeEvent(input$athlete, {
    available_sessions <- df_final_maximums %>%
      dplyr::filter(player_name == input$athlete) %>%
      dplyr::pull(date) %>%
      unique() %>%
      sort()
    
    updateSelectInput(
      session,
      inputId = "training",
      choices = available_sessions,
      selected = tail(available_sessions, 1)
    )
  })
  
  output$plot_mipsteam <- renderPlot({
    
    max_date <- max(df_final_maximums$date)
    min_date <- min(df_final_maximums$date)
    
    selected_range <- switch(
      input$select,
      "last_month" = c(max_date - 30, max_date),
      "last_3_months" = c(max_date - 90, max_date),
      "last_6_months" = c(max_date - 180, max_date),
      "all_data" = c(min_date, max_date),
      "personalized" = input$slider
    )
    
    df_hist_maximums <- df_final_maximums %>%
      filter(MM_3 < 39) %>% #there is 14 sessions where GPS was in a car LOL
      filter(date >= selected_range[[1]] & date <= selected_range[[2]]) %>%
      filter(if_all(MM_1:MM_1250, ~ !is.na(.)))
    
    df_hist_maximums <- df_hist_maximums %>%
      group_by(player_name) %>%
      summarise(across(starts_with("MM_"), ~{
        if(all(is.na(.x))) NA else max(.x, na.rm = TRUE)
      }))  %>%
      pivot_longer(!player_name, names_to = "code_window", values_to = "speed") %>%
      mutate(time_window = as.numeric(str_extract(code_window, "(?<=MM_)\\d+")))
    
    df_hist_maximums_indv <- df_final_maximums %>%
      filter(MM_3 < 39) %>% #there is 14 sessions where GPS was in a car LOL
      filter(date >= selected_range[[1]] & date <= selected_range[[2]]) %>%
      filter(player_name == input$athlete)
    
    df_hist_maximums_indv <- df_hist_maximums_indv %>%
      group_by(player_name) %>%
      summarise(across(starts_with("MM_"), ~{
        if(all(is.na(.x))) NA else max(.x, na.rm = TRUE)
      }))  %>%
      pivot_longer(!player_name, names_to = "code_window", values_to = "speed") %>%
      mutate(time_window = as.numeric(str_extract(code_window, "(?<=MM_)\\d+")))
    
    width_px <- session$clientData$output_plot_width
    
    # defineix angle segons amplada
    label_angle <- ifelse(width_px < 600, 60, 0)
    n_dodge_val <- ifelse(width_px < 600, 2, 1)
    
    # Defineix les zones (temps en segons)
    zones <- tibble::tibble(
      xmin = c(1, 7, 9, 21, 60, 481),      # 1 s, 7 s, 9 s, 21 s, 60 s, 9 min (540 s)
      xmax = c(6.999, 8.999, 20.999, 60.999, 480.999, 7200.999),   # 6 s, 8 s, 20 s, 60 s, 8 min (480 s), 120 min (7200 s)
      zone = c(
        "AnaAla-P",  # Anaerobic alactic potencia
        "AnaAla-C",  # Anaerobic alactic capacitat
        "AnaLa-P",  # Anaerobic lactic potencia
        "AnaLa-C",  # Anaerobic lactic capacitat
        "VO2max-P", # Aerobic VO2max potencia
        "Aerobic-C" # Aerobic capacitat
      ),
      fill_color = c(
        "#FF9999", "#FFCCCC", "#FFCC66", "#FFEE99", "#99CCFF", "#66AAFF"
      )
    ) %>%
      arrange(xmin) %>%                       # ordena segons xmin
      mutate(zone = factor(zone, levels = zone))  # converteix a factor ordenat
    
    # Al renderPlot(), abans dels geom_line(), afegeix les zones:
    ggplot(df_hist_maximums, aes(x = time_window, y = speed, group = player_name)) +
      # Zones de fons
      geom_rect(
        data = zones,
        mapping = aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = zone),
        inherit.aes = FALSE,
        alpha = 0.2
      ) +
      # Línies de dades
      geom_line(color = "black", size = 1, alpha = 0.2, na.rm = TRUE) +
      geom_line(data = df_hist_maximums_indv, aes(y = speed), color = "orange", size = 1.5) +
      # Escala X logarítmica i etiquetes
      scale_x_log10(
        breaks = c(1, 10, 30, 60, 120, 300, 600, 1200, 3600),
        labels = function(x) {
          sapply(x, function(val) {
            if (val < 60) paste0(val, " s") else paste0(round(val / 60, 1), " min")
          })
        }
      ) +
      # Escala de colors per les zones
      scale_fill_manual(values = setNames(zones$fill_color, zones$zone)) +
      theme_ipsum() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.key.width = unit(2, "lines")
      ) +
      labs(
        x = "Time (seconds/minutes)",
        y = "Speed (km/h)",
        fill = "Zone"
      )
    
  })
  
  zones_tbl <- tibble::tibble(
    xmin = c(1, 7, 9, 21, 60, 481),
    xmax = c(6.999, 8.999, 20.999, 60.999, 480.999, 7200.999),
    zone = c("AnaAla-P", "AnaAla-C", "AnaLa-P", "AnaLa-C", "VO2max-P", "Aerobic-C"),
    fill_color = c("#FF9999", "#FFCCCC", "#FFCC66", "#FFEE99", "#99CCFF", "#66AAFF")
  ) %>%
    arrange(xmin) %>%
    mutate(zone = factor(zone, levels = zone))
  
  
  output$plot <- renderPlot({
    
    max_date <- max(df_final_maximums$date)
    min_date <- min(df_final_maximums$date)
    
    selected_range <- switch(
      input$select,
      "last_month" = c(max_date - 30, max_date),
      "last_3_months" = c(max_date - 90, max_date),
      "last_6_months" = c(max_date - 180, max_date),
      "all_data" = c(min_date, max_date),
      "personalized" = input$slider
    )
    
    df_hist_maximums <- df_final_maximums %>%
      filter(MM_3 < 39) %>% #there is 14 sessions where GPS was in a car LOL
      filter(date >= selected_range[[1]] & date <= selected_range[[2]]) %>%
      filter(player_name == input$athlete)
    
    validate(
      need(nrow(df_hist_maximums) > 0, "No data for the selected time window")
    )
    
    df_hist_maximums <- df_hist_maximums %>%
      group_by(player_name) %>%
      summarise(across(starts_with("MM_"), ~{
        if(all(is.na(.x))) NA else max(.x, na.rm = TRUE)
      }))  %>%
      pivot_longer(!player_name, names_to = "code_window", values_to = "speed") %>%
      mutate(time_window = as.numeric(str_extract(code_window, "(?<=MM_)\\d+")))
    
    df_hist_maximums_session <- df_final_maximums %>%
      filter(MM_3 < 39) %>% #there is 14 sessions where GPS was in a car LOL
      filter(date == input$training) %>%
      filter(player_name == input$athlete)
    
    df_hist_maximums_session <- df_hist_maximums_session %>%
      group_by(player_name) %>%
      summarise(across(starts_with("MM_"), ~{
        if(all(is.na(.x))) NA else max(.x, na.rm = TRUE)
      }))  %>%
      pivot_longer(!player_name, names_to = "code_window", values_to = "speed") %>%
      mutate(time_window = as.numeric(str_extract(code_window, "(?<=MM_)\\d+")))
    
    width_px <- session$clientData$output_plot_width
    
    # defineix angle segons amplada
    label_angle <- ifelse(width_px < 600, 60, 0)
    n_dodge_val <- ifelse(width_px < 600, 2, 1)
    
    # Al renderPlot(), abans dels geom_line(), afegeix les zones:
    ggplot(df_hist_maximums, aes(x = time_window, y = speed)) +
      # Zones de fons
      geom_rect(
        data = zones_tbl,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = zone),
        inherit.aes = FALSE,
        alpha = 0.2
      ) +
      # Línies de dades
      geom_line(aes(y = speed), size = 1) +
      geom_line(data = df_hist_maximums_session, aes(y = speed), size = 0.5) +
      # Escala X logarítmica i etiquetes
      scale_x_log10(
        breaks = c(1, 10, 30, 60, 120, 300, 600, 1200, 3600),
        labels = function(x) {
          sapply(x, function(val) {
            if (val < 60) paste0(val, " s") else paste0(round(val / 60, 1), " min")
          })
        }
      ) +
      # Escala de colors per les zones
      scale_fill_manual(values = setNames(zones_tbl$fill_color, zones_tbl$zone)) +
      theme_ipsum() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.key.width = unit(2, "lines")
      ) +
      labs(
        x = "Time (seconds/minutes)",
        y = "Speed (km/h)",
        fill = "Zone"
      )
  })
  
  output$summary_table <- renderTable({
    req(input$athlete, input$training)
    
    # Defineix el mateix rang temporal que al gràfic
    max_date <- max(df_final_maximums$date)
    min_date <- min(df_final_maximums$date)
    
    selected_range <- switch(
      input$select,
      "last_month" = c(max_date - 30, max_date),
      "last_3_months" = c(max_date - 90, max_date),
      "last_6_months" = c(max_date - 180, max_date),
      "all_data" = c(min_date, max_date),
      "personalized" = input$slider
    )
    
    # Filtra dins el rang de dates
    df_hist_maximums <- df_final_maximums %>%
      filter(MM_3 < 39) %>% # elimina sessions estranyes
      filter(date >= selected_range[[1]] & date <= selected_range[[2]]) %>%
      filter(player_name == input$athlete)
    
    # Calcula el màxim per finestra dins del rang
    df_hist_maximums <- df_hist_maximums %>%
      group_by(player_name) %>%
      summarise(across(starts_with("MM_"), ~{
        if(all(is.na(.x))) NA else max(.x, na.rm = TRUE)
      })) %>%
      pivot_longer(!player_name, names_to = "code_window", values_to = "max_speed") %>%
      mutate(time_window = as.numeric(str_extract(code_window, "(?<=MM_)\\d+")))
    
    # Sessió seleccionada
    df_session <- df_final_maximums %>%
      filter(MM_3 < 39) %>%
      filter(player_name == input$athlete, date == input$training) %>%
      group_by(player_name) %>%
      summarise(across(starts_with("MM_"), ~{
        if(all(is.na(.x))) NA else max(.x, na.rm = TRUE)
      })) %>%
      pivot_longer(!player_name, names_to = "code_window", values_to = "session_speed") %>%
      mutate(time_window = as.numeric(str_extract(code_window, "(?<=MM_)\\d+")))
    
    # Combina i calcula el %
    df_compare <- df_hist_maximums %>%
      left_join(df_session, by = c("player_name", "code_window", "time_window")) %>%
      mutate(percent_of_max = round((session_speed / max_speed) * 100, 1)) %>%
      select(time_window, session_speed, max_speed, percent_of_max)
    
    # Filtra només les finestres desitjades
    desired_windows <- c(1, 10, 30, 60, 120, 300, 600, 1200, 3600)
    df_compare <- df_compare %>% filter(time_window %in% desired_windows)
    
    # Format final
    df_compare %>%
      arrange(time_window) %>%
      mutate(
        time_window = ifelse(time_window < 60,
                             paste0(time_window, " s"),
                             paste0(round(time_window / 60, 1), " min"))
      ) %>%
      rename(
        "Time window" = time_window,
        "Session speed (km/h)" = session_speed,
        "Max (filtered range, km/h)" = max_speed,
        "% of max" = percent_of_max
      )
  })
  
  output$plot_session <- renderPlot({
    
    name <- input$file1$name
    file1 <- readRDS(input$file1$datapath)
    
    # --- Extreiem la data del nom ---
    data <- sub("^([0-9]{4}-[0-9]{2}-[0-9]{2}).*$", "\\1", name)
    date_session <- as.Date(data)
    
    file1 <- file1 %>%
      distinct(time, .keep_all = TRUE) %>%
      arrange(time) %>%
      mutate(speed_kmh = speed * 3.6)
    
    if (nrow(file1) < 10) return(NULL)
    
    # --- Finestres (en segons) ---
    window_seconds <- c(
      seq(1, 120, by = 2),
      seq(125, 600, by = 10),
      seq(630, 900, by = 60)
    )
    
    # --- Rolling means ---
    v <- file1$speed_kmh
    n <- length(v)
    
    all_windows <- lapply(window_seconds, function(w) {
      win <- w * 20
      if (win >= n) return(NULL)
      
      means <- RcppRoll::roll_mean(v, n = win, align = "right")
      if (length(means) < length(file1$time)) {
        means <- c(rep(NA, length(file1$time) - length(means)), means)
      }
      
      tibble(
        window_seconds = w,
        timestamp = file1$time,
        mean_speed = means
      )
    })
    
    result_long <- bind_rows(all_windows)
    
    player_name_file <- if ("player_name" %in% names(file1)) file1$player_name[1] else NA
    
    result_long <- result_long %>%
      mutate(
        date = date_session,
        player_name = player_name_file
      ) %>%
      select(date, player_name, window_seconds, timestamp, mean_speed)
    
    # --- Històric ---
    max_date <- max(df_final_maximums$date)
    min_date <- min(df_final_maximums$date)
    
    selected_range <- switch(
      input$select,
      "last_month" = c(date_session - 30, date_session),
      "last_3_months" = c(date_session - 90, date_session),
      "last_6_months" = c(date_session - 180, date_session),
      "all_data" = c(min_date, date_session),
      "personalized" = input$slider
    )
    
    df_hist_maximums_indv <- df_final_maximums %>%
      filter(MM_3 < 39) %>%  # exclou sessions amb GPS al cotxe
      filter(date >= selected_range[[1]] & date <= selected_range[[2]]) %>%
      filter(player_name == player_name_file) %>%
      group_by(player_name) %>%
      summarise(
        across(
          starts_with("MM_"),
          function(x) {
            if (all(is.na(x))) {
              NA_real_
            } else {
              max(x, na.rm = TRUE)
            }
          }
        ),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = !player_name,
        names_to = "code_window",
        values_to = "speed"
      ) %>%
      mutate(
        time_window = as.numeric(str_extract(code_window, "(?<=MM_)\\d+"))
      )
    
    # --- Normalització respecte màxim històric ---
    result_norm <- result_long %>%
      left_join(
        df_hist_maximums_indv %>% select(player_name, time_window, max_speed = speed),
        by = c("player_name" = "player_name", "window_seconds" = "time_window")
      ) %>%
      mutate(mean_speed_rel = (mean_speed / max_speed) * 100)
    
    # --- Màxim per timestamp ---
    result_max_per_timestamp <- result_norm %>%
      group_by(player_name, timestamp) %>%
      summarise(
        max_rel_speed = if (all(is.na(mean_speed_rel))) NA_real_ else max(mean_speed_rel, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(max_rel_speed)) %>%
      mutate(timestamp = as.POSIXct(paste(date_session, timestamp), format = "%Y-%m-%d %H:%M:%OS"))
    
    # --- GRÀFIC 1: evolució temporal ---
    plot_evolucio <- ggplot() +
      geom_line(
        data = result_norm,
        aes(x = as.POSIXct(paste(date_session, timestamp), format = "%Y-%m-%d %H:%M:%OS"),
            y = mean_speed_rel,
            group = interaction(player_name, window_seconds)),
        color = "grey40",
        alpha = 0.2,
        linewidth = 0.4
      ) +
      geom_line(
        data = result_max_per_timestamp,
        aes(x = timestamp, y = max_rel_speed, group = player_name),
        color = "red",
        linewidth = 1
      ) +
      labs(
        title = "Evolução das médias móveis normalizadas e valor máximo por instante",
        x = "Tempo",
        y = "Velocidade média normalizada (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    # --- GRÀFIC 2: línia històrica + sessió amb gruix variable ---
    
    # 1️⃣ Dades històriques (ja les tens en df_hist_maximums)
    df_hist_maximums <- df_final_maximums %>%
      filter(MM_3 < 39) %>% #there is 14 sessions where GPS was in a car LOL
      filter(date >= selected_range[[1]] & date <= selected_range[[2]]) %>%
      filter(player_name == player_name_file)
    
    validate(
      need(nrow(df_hist_maximums) > 0, "No data for the selected time window")
    )
    
    df_hist_maximums <- df_hist_maximums %>%
      group_by(player_name) %>%
      summarise(across(starts_with("MM_"), ~{
        if(all(is.na(.x))) NA else max(.x, na.rm = TRUE)
      }))  %>%
      pivot_longer(!player_name, names_to = "code_window", values_to = "speed") %>%
      mutate(time_window = as.numeric(str_extract(code_window, "(?<=MM_)\\d+")))
    
    # 2️⃣ Màxims per finestra i % temps > 80% del màxim de la sessió
    df_session_windows <- result_norm %>%
      group_by(player_name, window_seconds) %>%
      summarise(
        max_speed_window_session = if (all(is.na(mean_speed))) NA_real_ else max(mean_speed, na.rm = TRUE),
        prop_time_over_80 = mean(mean_speed > 0.7 * max(mean_speed, na.rm = TRUE), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(linewidth_scaled = scales::rescale(prop_time_over_80, to = c(1, 5)))
    
    df_session_ribbon <- df_session_windows %>%
      mutate(
        ymin = max_speed_window_session - linewidth_scaled / 2,
        ymax = max_speed_window_session + linewidth_scaled / 2
      )
 
    
    # 3️⃣ Gràfic
    plot_zones_session <- ggplot() +
      # Zones de fons
      geom_rect(
        data = zones_tbl,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = zone),
        inherit.aes = FALSE,
        alpha = 0.2
      ) +
      # Línia històrica
      geom_line(
        data = df_hist_maximums,
        aes(x = time_window, y = speed),
        color = "grey10",
        linewidth = 1
      ) +
      geom_ribbon(
        data = df_session_ribbon,
        aes(x = window_seconds, ymin = ymin, ymax = ymax),
        fill = "grey",
        alpha = 0.5
      ) +
      geom_line(
        data = df_session_windows,
        aes(x = window_seconds, y = max_speed_window_session),
        color = "grey40",
        linewidth = 1
      ) +
      scale_x_log10(
        breaks = c(1, 10, 30, 60, 120, 300, 600, 1200, 3600),
        labels = function(x) {
          sapply(x, function(val) {
            if (val < 60) paste0(val, " s") else paste0(round(val / 60, 1), " min")
          })
        }
      ) +
      scale_fill_manual(values = setNames(zones_tbl$fill_color, zones_tbl$zone)) +
      scale_linewidth(range = c(0.5, 3), guide = "none") +
      guides(fill = "none") +
      theme_ipsum() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) +
      labs(
        title = "Velocidade máxima por janela: histórico vs sessão atual",
        x = "Tempo (segundos/minutos)",
        y = "Velocidade (km/h)"
      )
    
    # --- COMBINA ELS DOS GRÀFICS ---
    plot_evolucio / plot_zones_session + plot_layout(heights = c(2, 1))
  })
  
  
}

shinyApp(ui = ui, server = server)
