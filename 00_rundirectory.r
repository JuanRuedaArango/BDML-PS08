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

install.packages("rlang")
install.packages("dplyr")
install.packages("tidyverse")

#-----------------------------------------------------------

# Step 1: Download and construct the raw dataset
rm(list = ls())
if (require("pacman") == F){install.packages("pacman")}else{require("pacman")}



pacman::p_load(tidyverse)
# run code
list = c(# 01_importar
  # Data scraper
  "01_data_scraper/01_data_scraper.R",
  
  # Cleaning data
  "02_clean_data/02_clean_data.R",
  
  # Descriptive
  "00_descriptive/00_descriptive.R",
  
  # Wage-age models
  "03_reg_wage_age/03_reg_wage_age.R",
  "04_reg_wage_age_gap/04_reg_wage_age_gap.R",
  "05_reg_peak_age_gap_graph/05_reg_peak_age_gap_graph.R",
  
  # Prediction
  "06_prediction/06_prediction.R",
  "07_prediction_tables_graph/07_prediction_tables_graph.R"
  
)

run = walk(.x = list, .f = function(x){
  print(paste0("Running: ", x))
  Sys.sleep(3)
  source(x)}
)
