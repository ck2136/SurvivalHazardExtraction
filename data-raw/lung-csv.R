## code to prepare `lung.csv` dataset goes here

lung.csv <- read.csv(paste0(getwd(),"/data-raw/lung.csv"))
usethis::use_data(lung.csv, overwrite = TRUE)
