# =============================================================================
# Project:        PS  Prediction Income
#
# Description:    Data scraping and preprocessing from Ignacio's public repository
#                 for propensity score estimation in income-related outcomes.
#
# Authors:        Sany, Andrés, and Juan
# Affiliation:    Universidad de Los Andes
#
# Created:        2026-02-02
# Last updated:   2026-02-06
#
# Data source:    Ignacio's repository (see README for access details)
# Script type:    Data acquisition and preparation
#
# Reproducibility:
#   - R version:      ≥ 4.2.0
#   - Required pkgs:  tidyverse, data.table, httr, jsonlite
#   - Seed:           set.seed(12345)
#
# Output:
#   - Raw datasets saved in /data/
#
# Notes:
#   - Run this script before estimation and inference scripts.#
# =============================================================================

# Setup 
rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, data.table)

#=====================#
# Import
#=====================#
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

#=====================#
# scrapping
#=====================#

#--- Data
page <- read_html(url)

# Get links from main page 

links <- page %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  c() %>% 
  .[str_detect(string = ., pattern = "page")] %>% 
  paste0(url,.)

#get in page and scrapping

data = map2(.x = links,
            .y = seq_along(links),
            .f = function(x, chunk_id){
              
              Sys.sleep(5)
              
              link = read_html(x) %>% 
                as.character() %>% 
                str_extract_all(pattern = "pages/geih_page.+\\.html") %>% 
                unlist() %>% 
                paste0(url, .)
              
             
              data = read_html(link) %>% 
                html_table()
              
              df = data[[1]]
              
              
              df$chunk = chunk_id
              
              return(df)
              
            })

data = rbindlist(data)

#--- dictionaries
dict = read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html") %>% 
  html_table() %>% 
  .[[1]]

labels = read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html") %>% 
  html_table() %>% 
  .[[1]]

dictionary = list("dictionarie" = dict, 
                  "labels" = labels)

#=====================#
# export
#=====================#

export(data, "00_data/01_data_scrapping_web_page.rds")
export(dictionary, "99_additional/dictionary.xlsx")
