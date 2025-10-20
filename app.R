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

load("output/database_maximums_var.Rdata")
df_final_maximums$date <- as.Date(df_final_maximums$date)
#df_final_maximums <- df_final_maximums %>%
  #mutate(player_short = substr(as.character(player_name), 1, 9))  # primeres 8 lletres

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
      selectInput( 
        "training", 
        "Select session", 
        choices = NULL
      ),
      width = 3
    ),
    mainPanel(
      navset_tab(
        nav_panel("General",
                  div(
                    style = "text-align:center;",
                    plotOutput("plot", width = "100%"),
                    br(),
                    h4("% respect most intense periods selected"),
                    div(
                      style = "display: flex;
            justify-content: center;
            align-items: center;
            width: 100%;",tableOutput("summary_table"))
          )
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
    
    #--- 1. Ajustar el model hiperbòlic per jugador ---
    df_models <- df_hist_maximums %>%
      group_by(player_name) %>%
      nest() %>%
      mutate(
        fit = map(data, ~{
          df_valid <- drop_na(.x, speed)  # eliminar files amb NA
          if(nrow(df_valid) < 2) return(NULL)  # si no hi ha prou dades, no ajustar
          nls(speed ~ D_prime / time_window + CS,
              data = df_valid,
              start = list(D_prime = 100, CS = 5),
              control = nls.control(maxiter = 100))
        })
      )
    
    # --- 2. Afegir prediccions del model ---
    df_preds <- df_models %>%
      mutate(pred = map2(data, fit, ~{
        if(is.null(.y)) {
          # si no hi ha model, només retornem NA
          .x %>% mutate(speed_pred = NA_real_)
        } else {
          # només predim sobre les files amb valors vàlids utilitzats pel model
          df_valid <- drop_na(.x, speed)
          df_pred <- df_valid %>% mutate(speed_pred = predict(.y))
          
          # unir amb les files originals per conservar tota la informació
          .x %>%
            left_join(df_pred %>% select(time_window, speed_pred), by = "time_window")
        }
      })) %>%
      select(player_name, pred) %>%
      unnest(pred)
    
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
    ggplot(df_preds, aes(x = time_window, y = speed)) +
      # Zones de fons
      geom_rect(
        data = zones,
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
}

shinyApp(ui = ui, server = server)
