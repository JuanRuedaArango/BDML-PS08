# =============================================================================
# Project:        Problem Set – Prediction Income
#
# Description:    Data scraping and preprocessing from Ignacio's public repository
#                 for the income prediction problem set.
#
# Authors:        Sany, Andrés, and Juan
# Affiliation:    Universidad de Los Andes
#
# Created:        2026-02-02
# Last updated:   2026-02-02
# Reproducibility:
#   - R version:      ≥ 4.2.0
#   - Seed:           set.seed(12345)
#
# Output:
#   - Clean datasets saved in /data/
#
# Notes:
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
data = map(.x = links, .f = function(x){
  
  Sys.sleep(5)
  
  link = read_html(x) %>% 
    as.character(page) %>% 
    str_extract_all(pattern = "pages/geih_page.+\\.html") %>% 
    unlist() %>% 
    paste0(url, .)
  
  # import data
  data = read_html(link) %>% 
    html_table()
  
  return(data[[1]])
  
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
