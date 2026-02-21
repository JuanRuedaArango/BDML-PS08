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
# Script type:    Estimation and inference
#
# Reproducibility:
#   - R version:      ≥ 4.2.0
#
# Output:
#   - Unconditional age–income regression
#
# Notes:
#   - This script must be run after the data cleaning step
# =============================================================================



### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse, rio, tidymodels, yardstick, fixest)
options(scipen =  999)

##==: 1. Import data
data = import("00_data/01_main_data.rds")

##==: 2. split model     

# separar por grupos 
set.seed(1369)
split = initial_split(data, prop = 0.7)
train = training(split)
test = testing(split)

##==: 3. get metrics for previus models 
M1 = lm(log(y_total_m_ha) ~ age + I(age^2), data = train ) 
M2 = lm(log(y_total_m_ha) ~ sex, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
M3 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + relab + size_firm, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
M4 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + relab + size_firm + formalidad, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 


M1_prediction = predict(M1, newdata = test) 
M2_prediction = predict(M2, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
M3_prediction = predict(M3, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
M4_prediction = predict(M4, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 


##==: 4. Create new models
formulas = list(
  M6 = log(y_total_m_ha) ~ sex + poly(age, 3) + max_educ_level + oficio + relab + size_firm + formalidad,
  M7 = log(y_total_m_ha) ~ sex + poly(age, 2) + max_educ_level * formalidad + oficio + relab + size_firm,
  M8 = log(y_total_m_ha) ~ sex + poly(age, 2) + max_educ_level + oficio + relab*size_firm + formalidad,
  M9 = log(y_total_m_ha) ~ sex + poly(age, 2) + oficio + formalidad + max_educ_level*formalidad + relab*size_firm,
  M10 = log(y_total_m_ha) ~ sex + poly(age, 2) + oficio + formalidad + max_educ_level*formalidad*age + relab*size_firm*age
)

# entrenar modelos
models = lapply(formulas, function(f) lm(f, data = train))
models = list(M1, M2, M3, M4, models$M6, models$M7, models$M8, models$M9, models$M10)

# predicciones
preds = lapply(models[-c(1:4)], function(m) predict(m, newdata = test))
preds = list(M1_prediction, M2_prediction, M3_prediction, M4_prediction, preds[[1]], preds[[2]], preds[[3]], preds[[4]], preds[[5]])

rm(M1, M2, M3, M4)
rm(M1_prediction, M2_prediction, M3_prediction, M4_prediction)
rm(split, formulas)

##==: 5. extact metrics for new models
output = lapply(X = 1:length(preds), FUN = function(x) {
  
  pred_vs_truth = tibble(prediction = preds[[x]], 
                         truth = log(test$y_total_m_ha),  
                         mse = (truth-prediction)^2)
  
  metirc = metrics(pred_vs_truth, 
                   truth = truth, 
                   estimate  = prediction) |> 
    mutate(model_parameters = length(models[[x]]$coefficients))
  
  output =  list(metric = metirc,
                 result = pred_vs_truth, 
                 modelo = models[[x]], 
                 train_size = length(models[[x]]$residuals), 
                 test_size = length(test$y_total_m_ha)
  )
  return(output)
  
}) 

#=====================#
# 6. Export results   #
#=====================#

modelo <- output
saveRDS(modelo, "02_outputs/01_test_metrics.rds")



#------------------------#
# Predicting without hight leverage  
#------------------------#

##==: 2. split model     


# separar por grupos 
set.seed(1369)
split = initial_split(data, prop = 0.7)
train = training(split)
test = testing(split)

# observaciones a rem over
hatvalue = hatvalues(modelo[[9]]$modelo)
hatvalue = which(hatvalue>mean(hatvalue)*3) |> unique()

# remover en el train
train = train[-hatvalue,]

train = train %>%
  mutate(relab = factor(relab, levels = unique(train$relab)), 
         oficio = factor(oficio, levels = unique(train$oficio)))

test <- test %>%
  mutate(
    relab = factor(relab, levels = levels(train$relab)),
    oficio = factor(oficio, levels = levels(train$oficio)),
  )


##==: 3. get metrics for previus models 
M1 = lm(log(y_total_m_ha) ~ age + I(age^2), data = train ) 
M2 = lm(log(y_total_m_ha) ~ sex, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
M3 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + relab + size_firm, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
M4 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + relab + size_firm + formalidad, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 


M1_prediction = predict(M1, newdata = test) 
M2_prediction = predict(M2, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
M3_prediction = predict(M3, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
M4_prediction = predict(M4, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 


##==: 4. Create new models
formulas = list(
  M6 = log(y_total_m_ha) ~ sex + poly(age, 3) + max_educ_level + oficio + relab + size_firm + formalidad,
  M7 = log(y_total_m_ha) ~ sex + poly(age, 2) + max_educ_level * formalidad + oficio + relab + size_firm,
  M8 = log(y_total_m_ha) ~ sex + poly(age, 2) + max_educ_level + oficio + relab*size_firm + formalidad,
  M9 = log(y_total_m_ha) ~ sex + poly(age, 2) + oficio + formalidad + max_educ_level*formalidad + relab*size_firm,
  M10 = log(y_total_m_ha) ~ sex + poly(age, 2) + oficio + formalidad + max_educ_level*formalidad*age + relab*size_firm*age
)

# entrenar modelos
models = lapply(formulas, function(f) lm(f, data = train))
models = list(M1, M2, M3, M4, models$M6, models$M7, models$M8, models$M9, models$M10)

# predicciones
preds = lapply(models[-c(1:4)], function(m) predict(m, newdata = test))
preds = list(M1_prediction, M2_prediction, M3_prediction, M4_prediction, preds[[1]], preds[[2]], preds[[3]], preds[[4]], preds[[5]])

rm(M1, M2, M3, M4)
rm(M1_prediction, M2_prediction, M3_prediction, M4_prediction)
rm(split, formulas)

##==: 5. extact metrics for new models
output = lapply(X = 1:length(preds), FUN = function(x) {
  
  pred_vs_truth = tibble(prediction = preds[[x]], 
                         truth = log(test$y_total_m_ha),  
                         error = (truth-prediction))
  
  metirc = metrics(pred_vs_truth, 
                   truth = truth, 
                   estimate  = prediction) |> 
    mutate(model_parameters = length(models[[x]]$coefficients))
  
  output = list(metric = metirc,
                result = pred_vs_truth, 
                modelo = models[[x]], 
                train_size = length(models[[x]]$residuals), 
                test_size = length(test$y_total_m_ha)
  )
  return(output)
  
}) 

# Residual vs. leverage

influence_df <- tibble(
  residuals = resid(modelo),
  leverage = lev,
  cooks = cook
)

ggplot(influence_df, aes(x = leverage, y = residuals)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE) +
  theme_minimal()


#=====================#
# 6. Export results   #
#=====================#

saveRDS(modelo, "02_outputs/02_test_metrics_without_high_leverage.rds")

