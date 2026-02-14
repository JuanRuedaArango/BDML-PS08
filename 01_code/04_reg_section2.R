# =============================================================================
# Project:        PS – Prediction Income
#
# Description:    Estimation of the age–labor income profile and peak age
#                 using OLS and bootstrap methods
#
# Authors:        Sany, Andrés, and Juan
# Affiliation:    Universidad de los Andes
#
# Created:        2026-02-07
# Last updated:   2026-02-08
#
# Data source:    Ignacio's repository (see README for access details)
# Script type:    Reg
#
# Reproducibility:
#   - R version:      ≥ 4.1
#
# Output:
# =============================================================================


rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, janitor,data.table,caret,stargazer,boot,fixest)

#---------------------
# 1. import 
#---------------------

db <- readRDS('00_data/01_main_data.rds')

colnames(db)

## ---------------------------------------------------------------
## Variable construction
## ---------------------------------------------------------------


# Generate log wages and quadratic age term

db <- db %>%
  mutate(
    log_w = log(y_total_m),
    age2  = age^2
  )

#------------------------------
# 2. model regression 
#------------------------------

model1 = feols(log_w ~ factor(sex), data = db) 
model_sum = summary(model1)
model_sum

export(model1, "02_outputs/02_model1_reg_gap_female.rds")


#----------------------------------
# 3. FWL model regression 
#----------------------------------

model2 = lm(log_w ~ sex + age + I(age2) + max_educ_level + relab + oficio + formalidad + size_firm , data = db) 
summary(model2)

export(model2, "02_outputs/02_model2_reg_gap_female.rds")


model3 = feols(log_w ~ sex + age + I(age2)  + max_educ_level + relab + oficio + formalidad + size_firm + total_menores + total_seniors_inactivos, data = db) 
summary(model3)

export(model3, "02_outputs/02_model3_reg_gap_female.rds")

## 3.1 first model 

m_y <- lm(log_w ~ age + I(age2) + max_educ_level + relab + oficio + formalidad + size_firm, data = db)
m_x <- lm(sex ~  age + I(age2) + max_educ_level + relab + oficio + formalidad + size_firm , data = db)

db$YResid      <- resid(m_y)
db$FemaleResid <- resid(m_x)

model4 <- feols(YResid ~ FemaleResid, data = db)
summary(model4)
export(model4, "02_outputs/02_model4_reg_gap_female.rds")


## 3.2 second model 

m_y <- lm(log_w ~ age + I(age2)  + max_educ_level + relab + oficio + formalidad + size_firm + total_menores + total_seniors_inactivos, data = db)
m_x <- lm(sex ~   age + I(age2)  + max_educ_level + relab + oficio + formalidad + size_firm + total_menores + total_seniors_inactivos, data = db)

db$YResid      <- resid(m_y)
db$FemaleResid <- resid(m_x)

model5 <- feols(YResid ~ 0 + FemaleResid, data=db)
summary(model5)
export(model5, "02_outputs/02_model5_reg_gap_female.rds")


#=======================================
# FWL: regresión de residuos 
#=======================================

# Dummy
db$female <- as.integer(db$sex %in% c("Femenino"))

# new df

db_boot <- data.frame(
  y_total_m_ha = db$y_total_m_ha,
  female = db$female,
  age = db$age,
  max_educ_level = db$max_educ_level,
  relab = db$relab,
  oficio = db$oficio,
  formalidad = db$formalidad,
  size_firm = db$size_firm,
  total_menores = db$total_menores,
  total_seniors_inactivos = db$total_seniors_inactivos
)

fwl_fn <- function(data, indices) {
  d <- data[indices, , drop = FALSE]  
  
  y_res <- resid(lm(log(y_total_m_ha) ~ age + I(age^2) + max_educ_level + relab +
                      oficio + formalidad + size_firm + total_menores +
                      total_seniors_inactivos, data = d))
  
  x_res <- resid(lm(female ~ age + I(age^2) + max_educ_level + relab +
                      oficio + formalidad + size_firm + total_menores +
                      total_seniors_inactivos, data = d))
  
  unname(coef(lm(y_res ~ x_res))[2])  
}

set.seed(12345)
boot_fwl <- boot(db_boot, statistic = fwl_fn, R = 3000)
boot_fwl$t0          #estimacion
sd(boot_fwl$t)        # EE bootstrap
quantile(boot_fwl$t, c(.025,.975))  # IC 95%

# =============================================================================
# Tables
# =============================================================================

etable(model1, model4, model5,
       dict = c("log(y_total_m_ha)" = "ln(salario por hora)",
                "(Intercept)" = "Constante",
                "factor(sex)Femenino" = "Mujer",
                "FemaleResid" = "Residuales Mujer",
                "YResid"      = "Residuales de salario", 
                "r2"        = "R²",
                "ar2"       = "R² ajustado",
                "rmse"      = "Error cuadrático medio (RMSE)",
                "n"         = "Número de observaciones",
                "FE"        = "Efectos fijos",
                "Std. Errors" = "Errores estándar"),  
       extralines = list("Controles laborales" = c("NO", "SI", "SI"),
                         "Controles de cuidado" = c("NO", "NO", "SI") ),
       depvar = TRUE,
       digits = 3,
       title = "Resultados de la estimación", 
       fitstat = ~  n + r2 + ar2 + rmse,  
       notes = c(
         "Controles laborales: edad, edad$^2$, nivel educativo, relación laboral, oficio y tamaño de la firma.",
         "Controles de cuidado: número de menores en el hogar y número de mayores inactivos.",
         "La columna (1) es el modelo base; (2) añade controles laborales; (3) añade controles de cuidado."
       ),                 
       file = "02_outputs/tables/03_model_gender_gap.tex", replace = TRUE)

#for Markdown table

library(modelsummary)

modelsummary(
  list(
    "Linear (Unconditional)" = model1,
    "Quadratic (Unconditional)" = model4,
    "Quadratic (Conditional)" = model5
  ),
  output = "02_outputs/tables/03_model_gender_gap.md"
)


