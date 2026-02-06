# =============================================================================
# Master script
#
# Description:
#   Running this script reproduces all results in this repository.
#
# Usage:
#   - From an interactive R session:
#       source("01_code/00_rundirectory.R")
#   - From the command line:
#       R CMD BATCH 01_code/00_rundirectory.R
#
# Authors:
#   - Sany León Curasi
#   - Andrés Camilo Suárez
#   - Juan Rueda
#
# =============================================================================


# Step 1: Download and construct the raw dataset
rm(list = ls())
if (require("pacman") == F){install.packages("pacman")}else{require("pacman")}

pacman::p_load(tidyverse)
# run code
list = c(# 01_importar
  "01_code/01_data_scraper.R",
  #cleaning
  "01_code/02_clean_data.R"
)

run = walk(.x = list, .f = function(x){
  print(paste0("Running: ", x))
  Sys.sleep(3)
  source(x)}
)


#source("01_code/02_clean_data.R")