library(ggplot2)
library(dplyr)
library(arrow)
library(lubridate)
library(data.table)
library(furrr)
library(purrr)


#####load_data(): function to change from .PARQUET to RDS####
# Only need to run once #

load_data <- function(year_path_folder){
  
  folders <- list.files(year_path_folder)
  
  for (month in folders){
    
    days <- list.files(paste(year_path_folder,"/", month, sep = ""))
    
    for (day in days){
      
      players_files <- list.files(paste(year_path_folder,"/", month, "/", day, sep = ""))
      
      for (player in players_files){
        
        file <- paste(year_path_folder,"/", month, "/", day, "/", player, sep = "")
        df <- arrow::read_parquet(file)
        saveRDS(df, file = sub("\\.parquet$", ".rds", file))
        file.remove(file)
        
      }
      
    }
    
  }
  rm(df1)
  rm(days)
  rm(folders)
  rm(month)
}



#####function to create a resume database of trainings####
# Only need to run once, it demores 1 houre minimum #

create_database_fast <- function(year_path_folder, output_file = "output/database.Rdata"){
  
  #####function to create a resume database from files####
  # Auxiliar function for the function create_database_fast() #
  
  resume_file <- function(file, year_path_folder) {
    e <- new.env()
    df <- readRDS(file)
    #load(file, envir = e)
    #df <- get(ls(e)[1], envir = e)   # agafa l'objecte carregat
    
    # --- treure data del path ---
    rel_path <- gsub(paste0("^", year_path_folder, "/"), "", file)
    parts <- strsplit(rel_path, "/")[[1]]
    day <- parts[2]  # [mes, dia, fitxer]
    date_session <- as.Date(day)
    
    # --- preproces GPS ---
    df_gps <- df %>%
      distinct(time, .keep_all = TRUE) %>%
      arrange(time) %>%
      mutate(speed_kmh = speed * 3.6)
    
    # duration
    df <- df %>% mutate(time = hms::as_hms(time))
    duration <- as.numeric(max(df$time) - min(df$time), units = "secs")
    
    # player
    player_name <- df$player_name[1]   # més ràpid que unique()
    
    # distance
    dist_total <- sum(df_gps$speed * 0.1, na.rm = TRUE)
    dist_21 <- sum((df_gps$speed * 0.1) * (df_gps$speed_kmh > 21), na.rm = TRUE)
    
    # sprints
    n_sprints <- sum(df_gps$speed_kmh > 21 & lag(df_gps$speed_kmh, default = 0) <= 21, na.rm = TRUE)
    
    # playerload
    df <- df %>%
      mutate(
        d_acc_x = c(0, diff(accl_x)),
        d_acc_y = c(0, diff(accl_y)),
        d_acc_z = c(0, diff(accl_z)),
        pl_inst = sqrt(d_acc_x^2 + d_acc_y^2 + d_acc_z^2)
      )
    playerload <- sum(df$pl_inst, na.rm = TRUE) / 100
    
    # heart rate
    hr_mean <- mean(df_gps$heart_rate[df_gps$heart_rate > 0], na.rm = TRUE)
    hr_max <- max(df_gps$heart_rate, na.rm = TRUE)
    
    tibble(
      date = date_session,
      player = player_name,
      duration = duration,
      distance = dist_total,
      distance_21 = dist_21,
      sprints_21 = n_sprints,
      playerload = playerload,
      hr_mean = hr_mean,
      hr_max = hr_max
    )
  }
  
  # agafa tots els arxius dins de mes/dia/jugadora
  files <- list.files(year_path_folder, recursive = TRUE, full.names = TRUE)
  
  # aplica resume_file a cada arxiu i els uneix
  df_final <- map_dfr(files, resume_file, year_path_folder = year_path_folder)
  
  # guarda en Rdata
  save(df_final, file = output_file)
  
  df_final
}

# Example of command #
create_database_fast("data/2020")
