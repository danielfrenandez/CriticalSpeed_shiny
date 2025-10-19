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
          "personalized range" = "personalized")
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
        "training", 
        "Select session", 
        choices = unique(df_final_maximums$date)
      ),
      selectInput( 
        "athlete", 
        "Select Athlete:", 
        choices = unique(df_final_maximums$player_name)
      ),
      width = 3
    ),
    mainPanel(
      navset_tab(
        nav_panel("General", plotOutput("plot"))
      )
    )
  )
)

server <- function(input, output) {
  
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
    
    ggplot(df_preds, aes(x = time_window, y = speed)) +
      geom_line(aes(y = speed)) + 
      geom_line(data = df_hist_maximums_session, aes(y = speed), size = 1) +
      scale_color_viridis(discrete = TRUE)
  })
}

shinyApp(ui = ui, server = server)
