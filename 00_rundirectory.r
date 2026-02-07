# =============================================================================
# Project:        PS Prediction Income
#
# Script:         Master Script - Reproduce All Results
#
# Description:    Runs all analyses and generates all results
#                 contained in the repository.
#
# Authors:        Sany León Curasi, Andrés Camilo Suárez, Juan Rueda
# Affiliation:    Universidad de Los Andes
#
# Created:        2026-02-06
# Last updated:   2026-02-06
#
# Script type:    Reproducibility / Full Analysis
#
# Reproducibility:
#   - R version:      ≥ 4.2.0
#   - Seed:           set.seed(12345)
#
# Notes:
#   - This script should be run before any estimation or inference scripts.
# =============================================================================


# Step 1: Download and construct the raw dataset
rm(list = ls())
if (require("pacman") == F){install.packages("pacman")}else{require("pacman")}

pacman::p_load(tidyverse)
# run code
list = c(# 01_importar
  "01_code/01_data_scraper.R",
  
  #Cleaning data
  "01_code/02_clean_data.R"
)

run = walk(.x = list, .f = function(x){
  print(paste0("Running: ", x))
  Sys.sleep(3)
  source(x)}
)
