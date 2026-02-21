# PROBLEM SET PREDICTION INCOME
# Authors:
# - Sany León 
# - Andrés Camilo Suárez
# - Juan Rueda
# BIG DATA AND MACHINE LEARNING - UNIVERSIDAD DE LOS ANDES - 2026 

This repository contains the complete workflow developed to solve Problem Set 1 for the course Big Data and Machine Learning (2026–10). The project is organized into modular directories that cover each stage of the process, from data scraping and preprocessing to analysis, modeling, and results documentation.

Please run the code contained in the script <i>00_rundirectory.R to replicate the results.

<!---------------------------->
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

01_data_scraper.r: Data scraping and preprocessing from Ignacio's public repository for propensity score estimation in income-related outcomes.

02_clean_data.r: Data cleaning, variable construction, and preprocessing of the raw dataset obtained from Ignacio's public repository.

03_reg_wage_age.r: Baseline estimation of the age–labor income profile using OLS and bootstrap methods. 

04_reg_wage_age_gap.R: Estimation of gender wage gaps across the life cycle using age–income regressions.

05_reg_peak_age_gap_graph.R: Visualization of the age–labor income profile and estimated peak age (OLS and bootstrap).

06_prediction.R: Out-of-sample prediction exercise and model comparison for income outcomes. 

07_prediction_tables_graph.R: Generation of prediction tables, residual diagnostics, and evaluation graphs.


<!---------------------------->

## Estructura de Carpetas:


📂 00_data
│   └── Raw and processed datasets
│
📂 01_code
│   ├── Data scraping scripts
│   ├── Data cleaning scripts
│   └── Auxiliary scripts
│
📂 02_outputs
│   ├── 📂 tables
│   │   └── LaTeX and summary tables
│   └── 📂 figures
│       └── Generated plots and graphs
│
📂 03_regression
│   ├── Wage–age profile estimations
│   ├── Wage gap regressions
│   └── Peak age analysis
│
📂 04_slides
│   └── Presentation materials
│
📂 99_additional
│   └── Supplementary material
│
📄 00_rundirectory.R  
📄 BDML-PS08.Rproj  
📄 README.md  
📄 LICENSE