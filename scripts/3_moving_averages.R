# Desactivar notació científica
options(scipen = 999)

library(dplyr)
library(purrr)
library(zoo)
library(tibble)


### Principal function to run moving average function through all data ###
moving_averages_fast <- function(year_path_folder, output_file = "output/database_maximums.Rdata") {
  
  ### Auxiliar function to run moving averages ###
  moving_average_ind <- function(file, year_path_folder) {
    # read data
    df <- readRDS(file)
    
    # extract date from path
    rel_path <- gsub(paste0("^", year_path_folder, "/"), "", file)
    parts <- strsplit(rel_path, "/")[[1]]
    
    # control path format
    if (length(parts) < 2) {
      warning("Format de path inesperat: ", rel_path)
      return(NULL)
    }
    
    day <- parts[2]  # [month, day, file]
    date_session <- as.Date(day)
    
    # GPS preprocess
    df_gps <- df %>%
      distinct(time, .keep_all = TRUE) %>%
      arrange(time) %>%
      mutate(speed_kmh = speed * 3.6)
    
    # Control point if data no exist
    if (nrow(df_gps) < 10) {
      warning("Fitxer buit o massa curt: ", file)
      return(NULL)
    }
    
    # parameters
    freq <- 20                # Hz
    window_seconds <- c(3, 5, 10, 30, 60, 120, 240, 480, 600)
    
    # max for each window
    max_values <- sapply(window_seconds, function(w) {
      rollmean(df_gps$speed_kmh, k = freq * w, fill = NA, align = "right") |>
        max(na.rm = TRUE)
    })
    
    # results
    player_name <- if ("player_name" %in% names(df_gps)) df_gps$player_name[1] else NA
    
    result <- tibble(
      date = date_session,
      player_name = player_name
    )
    
    result <- bind_cols(result, as_tibble_row(setNames(max_values, paste0("MM_", window_seconds))))
    
    return(result)
  }
  
  # List of files
  files <- list.files(year_path_folder, recursive = TRUE, full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No files in directory: ", year_path_folder)
  }
  
  message("Processing ", length(files), " files...")
  
  # Progress
  df_final_maximums <- map_dfr(seq_along(files), function(i) {
    if (i %% 10 == 0) cat(round(i / length(files) * 100), "% complet\n")
    moving_average_ind(files[i], year_path_folder)
  })
  
  # Save all results
  save(df_final_maximums, file = output_file)
  
  message("File saved as: ", output_file)
  return(df_final_maximums)
}

moving_averages_fast("data/2020")


### Principal function to run moving average function through all data ###
moving_averages_ind_fast <- function(year_path_folder, output_file = "output/database_maximums_var.Rdata") {
  
  library(dplyr)
  library(tibble)
  
  moving_average_ind_variable <- function(file, year_path_folder, freq = 20) {
    # --- Llegim el fitxer ---
    df <- readRDS(file)
    
    # --- Extraiem la data del path ---
    rel_path <- gsub(paste0("^", year_path_folder, "/"), "", file)
    parts <- strsplit(rel_path, "/")[[1]]
    if (length(parts) < 2) return(NULL)
    
    day <- parts[2]
    date_session <- as.Date(day)
    
    # --- Preprocessament GPS ---
    df_gps <- df %>%
      distinct(time, .keep_all = TRUE) %>%
      arrange(time) %>%
      mutate(speed_kmh = speed * 3.6)
    
    if (nrow(df_gps) < 10) return(NULL)
    
    # --- Definim les finestres (en segons) ---
    window_seconds <- c(
      seq(1, 300, by = 1),        # 0-5 min cada 1s
      seq(305, 1800, by = 5),     # 5-30 min cada 5s
      seq(1830, 7200, by = 30)    # 30min-2h cada 30s
    )
    
    # --- Vector i suma acumulada ---
    v <- df_gps$speed_kmh
    S <- c(0, cumsum(v))
    n <- length(v)
    
    # --- Càlcul ràpid per a cada finestra ---
    roll_max_by_window <- sapply(window_seconds, function(w) {
      win <- w * freq
      if (win >= n) return(NA_real_)
      means <- (S[(win + 1):(n + 1)] - S[1:(n + 1 - win)]) / win
      max(means, na.rm = TRUE)
    })
    
    player_name <- if ("player_name" %in% names(df_gps)) df_gps$player_name[1] else NA
    
    result <- tibble(
      date = date_session,
      player_name = player_name
    ) |> bind_cols(
      tibble(!!!setNames(as.list(roll_max_by_window), paste0("MM_", window_seconds)))
    )
    
    return(result)
  }
  
  # List of files
  files <- list.files(year_path_folder, recursive = TRUE, full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No files in directory: ", year_path_folder)
  }
  
  message("Processing ", length(files), " files...")
  
  # Progress
  df_final_maximums <- map_dfr(seq_along(files), function(i) {
    if (i %% 10 == 0) cat(round(i / length(files) * 100), "% complet\n")
    moving_average_ind_variable(files[i], year_path_folder)
  })
  
  # Save all results
  save(df_final_maximums, file = output_file)
  
  message("File saved as: ", output_file)
  return(df_final_maximums)
}

moving_averages_ind_fast("data/2020")
