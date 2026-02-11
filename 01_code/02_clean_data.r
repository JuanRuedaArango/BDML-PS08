# =============================================================================
# Project:        PS  Prediction Income
#
# Description:    Data cleaning and preprocessing from Ignacio's public repository
#
# Authors:        Sany, Andrés, and Juan
# Affiliation:    Universidad de Los Andes
#
# Created:        2026-02-05
# Last updated:   2026-02-06
#
# Data source:    Ignacio's repository (see README for access details)
# Script type:    Data acquisition and preparation
#
# Reproducibility:
#   - R version:      ≥ 4.0
#   - Seed:           set.seed(369)
#
# Output:
#   - Clean datasets saved in 00_data file
#
# Notes:
#   - Run this script before estimation and inference scripts.#
# =============================================================================


## ---------------------------------------------------------------
## Load previously downloaded data
## ---------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse, rvest, data.table)

base <- readRDS("00_data/01_data_scrapping_web_page.rds")

colnames(base)

## ---------------------------------------------------------------
## Selecting Sample
## ---------------------------------------------------------------

db <- base %>%
  filter(age >= 18, ocu == 1) %>%
  select(
    directorio, secuencia_p, orden,
    sex, age, college, maxEducLevel,
    ocu, informal, relab, oficio,
    totalHoursWorked, y_total_m, y_total_m_ha,
    p6050, pet, mes, clase, fex_c, inac, sizeFirm, clase, chunk
  ) %>%
  rename(
    max_educ_level = maxEducLevel,
    ocupado        = ocu,
    total_hours    = totalHoursWorked,
    parent_hh      = p6050,
    PET = pet,
    f_weights      = fex_c,
    inactivo       = inac,
    size_firm      = sizeFirm,
    Urbano=clase
  )


## Create indicator variables
db = db %>% 
  mutate(menores_edad = ifelse(age < 18,1,0),
         seniors_inactivos = ifelse(age > 65 & inactivo==1,1,0)) %>% 
  group_by(directorio,secuencia_p) %>% 
  mutate(total_menores = sum(menores_edad),
         total_seniors_inactivos = sum(seniors_inactivos)) %>% 
  ungroup()

## Impute missing values of covariates
mode_max_educ <- as.numeric(
  names(which.max(table(db$max_educ_level)))
)

db <- db %>%
  mutate(
    max_educ_level = ifelse(
      is.na(max_educ_level),
      mode_max_educ,
      max_educ_level
    )
  )

## Relabel covariates

### Educacion
db = db %>%
  mutate(max_educ_level = case_when(
    max_educ_level == 1 ~ "None",
    max_educ_level == 2 ~ "Preschool",
    max_educ_level == 3 ~ "Incomplete primary",
    max_educ_level == 4 ~ "Complete primary",
    max_educ_level == 5 ~ "Incomplete secondary",
    max_educ_level == 6 ~ "Complete secondary",
    max_educ_level == 7 ~ "Tertiary education",
    max_educ_level == 999 ~ NA,   # Replace 999 with NA
    TRUE ~ NA
  ),
  max_educ_level = as.factor(max_educ_level))

### Sex
db = db %>% 
  mutate(sex = ifelse(sex == 0, "Female", "Male"),
         sex = factor(sex, levels = c("Female", "Male")),
         sex = relevel(sex, ref = "Male"))

### Relab
db = db %>%
  mutate(relab = case_when(
    relab == 1 ~ "Private sector employee",
    relab == 2 ~ "Public sector employee",
    relab == 3 ~ "Domestic worker",
    relab == 4 ~ "Self-employed",
    relab == 5 ~ "Employer",
    relab == 6 ~ "Unpaid family worker",
    relab == 7 ~ "Unpaid worker in other households' businesses",
    relab == 8 ~ "Day laborer",
    relab == 9 ~ "Other",
    TRUE ~ NA
  ),
  relab = as.factor(relab))

### Formal
db = db %>% 
  mutate(formalidad = ifelse(informal == 1,'Informal','Formal'),
         formalidad = factor(formalidad,levels = c('Informal','Formal')),
         formalidad = relevel(formalidad,ref = 'Formal')) %>% 
  select(-informal)

### Urbanidad
b <- db %>% 
  mutate(Urbano = factor(Urbano,
                         levels = c(1,0),
                         labels = c("Si","No")))

### Size of firm
db = db %>%
  mutate(size_firm = case_when(
    size_firm == 1 ~ "Self-employed",
    size_firm == 2 ~ "2–5 employees",
    size_firm == 3 ~ "6–10 employees",
    size_firm == 4 ~ "11–50 employees",
    size_firm == 5 ~ "More than 50 employees"
  ),
  
  size_firm = factor(size_firm,
                     levels = c("Self-employed",
                                "2–5 employees",
                                "6–10 employees",
                                "11–50 employees",
                                "More than 50 employees")))

### Jobs 
db = db %>%
  mutate(oficio = case_when(
    oficio %in% c(1,2,3,4,5,6,7,8,9,11,12) ~ "Scientific and technical professionals",
    oficio %in% c(13,14,15,16,17,19) ~ "Education, religion, and culture",
    oficio %in% c(18) ~ "Arts, sports, and media",
    oficio %in% c(20,21,30,31,32,33,34,35,36,37,38,39,40,50,51,60) ~ "Administration and management",
    oficio %in% c(41,42,43,44,45,49) ~ "Commerce and sales",
    oficio %in% c(52,53,54,55,56,57,58,59) ~ "Personal and security services",
    oficio %in% c(61,62,63,64,99) ~ "Agriculture, fishing, and rural occupations",
    oficio %in% c(70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,93,94,95) ~ "Industry and construction",
    oficio %in% c(88,89,90,91,92) ~ "Textiles and artisanal manufacturing",
    oficio %in% c(96,97,98) ~ "Operatives and unskilled workers"
  ),
  oficio = factor(oficio))

## Remove NA from outcome variable
db = db %>% 
  drop_na(y_total_m_ha)

## Remove observations that by definition do not recieve any labor income
db = db %>% 
  filter(!relab %in% c("Unpaid family worker",
                       "Unpaid worker in other households' businesses"))

db = db |> 
  mutate(relab = factor(x = relab, levels = unique(db$relab)), 
         max_educ_level = factor(x = max_educ_level, levels = unique(db$max_educ_level)))

## Select covariates
db = db %>% 
  select(directorio,secuencia_p,orden,
         age,sex,max_educ_level,
         formalidad,y_total_m, relab,size_firm,oficio,
         total_menores,total_seniors_inactivos,
         y_total_m_ha,total_hours,
         f_weights, mes,Urbano,chunk)

# Reference values (Colombia, 2018). Minimum monthly wage: COP 781,242
# Poverty line in Bogotá (household level): COP 434,630


## ---------------------------------------------------------------
## Exploring potential seasonality in monthly labor income
## ---------------------------------------------------------------
## No clear evidence of seasonality is observed across months
db %>%
  group_by(mes) %>%
  summarise(
    median_income = median(y_total_m, na.rm = TRUE),
    mean_income   = mean(y_total_m, na.rm = TRUE),
    max_income    = max(y_total_m, na.rm = TRUE),
    min_income    = min(y_total_m, na.rm = TRUE)
  )


## ---------------------------------------------------------------
## Distributional analysis: income quantiles
## ---------------------------------------------------------------
## Extreme values are observed in the upper tail of the distribution
 quantile(
  db$y_total_m,
  probs = c(.001, .9, .95, .99, .995, .999),
  na.rm = TRUE
 )


## ---------------------------------------------------------------
## Gender composition at the extremes of the income distribution
## ---------------------------------------------------------------
## Excluding extreme values would remove a similar proportion
## of men and women from the sample


# db %>%
#  filter(y_total_m >= quantile(y_total_m, .99, na.rm = TRUE)) %>%
#  group_by(sex) %>%
#  summarise(n = n())

# db %>%
#  filter(y_total_m <= quantile(y_total_m, .001, na.rm = TRUE)) %>%
#  group_by(sex) %>%
#  summarise(n = n())


## ---------------------------------------------------------------
## Construction of the trimmed analytical sample
## ---------------------------------------------------------------
## Sample trimmed at the 0.1th and 99th percentiles of monthly income
# db_clean <- db %>%
#  filter(
#    y_total_m <= quantile(y_total_m, .99, na.rm = TRUE),
#    y_total_m >= quantile(y_total_m, .001, na.rm = TRUE) )
 
#colSums(is.na(db))

  saveRDS(db, file = "00_data/01_main_data.rds")

## ---------------------------------------------------------------
## Descriptive checks of the final sample
## ---------------------------------------------------------------
hist(db$y_total_m)
summary(db)
