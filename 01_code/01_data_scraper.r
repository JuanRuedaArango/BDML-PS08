#========================================================#
# PS Prediction Income
# Sany, Andres and Juan
# Fisrt Created:  02/01/2026
# Last update:  02/01/2026
# Script:       Scrapping data from Ignacio repository
#======================================================#

# Setup 
rm(list = ls())

rm(list = ls())
pacman::p_load(rio, rvet, tidyverse, data.table)

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

export(data, "BDML-PS08/00_data/01_data_scrapping_web_page.rds")
export(dictionary, "BDML-PS08/99_additional/dictionary.xlsx")
