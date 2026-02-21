# PROBLEM SET PREDICTION INCOME
# Authors:
# - Sany León 
# - Andrés Camilo Suárez
# - Juan Rueda
# BIG DATA AND MACHINE LEARNING - UNIVERSIDAD DE LOS ANDES - 2026 

This repository contains the complete workflow developed to solve Problem Set 1 for the course Big Data and Machine Learning (2026–10). The project is organized into modular directories that cover each stage of the process, from data scraping and preprocessing to analysis, modeling, and results documentation.

Please run the code contained in the script <i>00_rundirectory.R to replicate the results.

<!---------------------------->

## Descripción de carpetas:
-   00_data: funciones de scraping para extraer la información. Saving intermediate data.
-   01_code: Code building for clean, regress and prediccion.
-   02_output: These files extract the results from the scripts, such as figures and tables.
-   03_regression: estimación de modelos econométricos.
-   04_slides: Presentaciones en formato md y pdf.
-   99_other: Files usefull for the results.

<!---------------------------->

To reproduce all results, run:

source("01_code/00_rundirectory.R")

<!---------------------------->

00_rundirectory.R: master script, run to reproduce all results

00_descriptive.R:
01_data_scraper.r: scrapes the data from https://ignaciomsarmiento.github.io/GEIH2018_sample/
02_clean_data.r:
03_reg_wage_age.r:
04_reg_wage_age_gap.R:
05_reg_peak_age_gap_graph.R:
05_reg_section3.R:
