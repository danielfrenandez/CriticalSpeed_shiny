###install.packages('zen4R')
library(zen4R)

download2020data <- function(team, year) {
  
  # Defineix el directori fix
  path <- "data/2020/"
  
  # Crea el directori si no existeix
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  
  # Defineix el nom del fitxer
  zip_file <- paste0(path, "objective-", team, "-", year, ".zip")
  
  # Descarrega
  zen4R::download_zenodo(
    "10.5281/zenodo.10033832",
    path = path,
    files = list(paste0("objective-", team, "-", year, ".zip")),
    timeout = 3600
  )
  
  # Descomprimeix
  unzip(zip_file, exdir = path)
  
  # Elimina el fitxer zip
  file.remove(zip_file)
}


#Example
download2020data(team = "TeamA", year = "2020")
download2020data(team = "TeamA", year = "2021")
