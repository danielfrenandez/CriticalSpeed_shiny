library(tidyr)
library(viridis)
library(gghighlight)
library(stringr)
library(purrr)
library(broom)
library(dplyr)

df_historic_ind_max <- function(datein = "2020-06-01", dateout = "2021-11-16", output_file = "output/df_hist_maximums.Rdata"){
  
  load("output/database_maximums_var.Rdata")
  df_final_maximums$player_name <- as.factor(df_final_maximums$player_name)
  
  df_hist_maximums <- df_final_maximums %>%
    filter(MM_3 < 39) %>% #there is 14 sessions where GPS was in a car LOL
    filter(date >= datein & date <= dateout) %>%
    group_by(player_name) %>%
    summarise(across(starts_with("MM_"), ~{
      if(all(is.na(.x))) NA else max(.x, na.rm = TRUE)
    }))  %>%
    pivot_longer(!player_name, names_to = "code_window", values_to = "speed") %>%
    mutate(time_window = as.numeric(str_extract(code_window, "(?<=MM_)\\d+")))
  
  
  save(df_hist_maximums, file = output_file)
  
}

df_historic_ind_max()
load("output/df_hist_maximums.Rdata")

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

# --- 3. Crear identificador curt del jugador ---
df_preds <- df_preds %>%
  mutate(player_short = substr(as.character(player_name), 1, 9))


# --- 3. Gràfic amb punts i corba ajustada ---
ggplot(df_preds, aes(x = time_window, y = speed, colour = player_short)) +
  geom_line(aes(y = speed_pred), size = 1) +             # corba ajustada
  geom_line(aes(y = speed, colour = player_short)) +                                 # punts observats
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~player_short) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey85", 1))) +
  theme_minimal() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.2, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  labs(
    title = "Critical Speed - 2020",
    x = "Time Window (s)",
    y = "Velocity (km/h)"
  )

