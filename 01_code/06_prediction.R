# =============================================================================
# Project:        PS – Income Prediction
#
# Description:    Estimates alternative income models using OLS, evaluates
#                 out-of-sample performance, and analyzes observation-level
#                 influence using LOOCV diagnostics.
#
# Authors:        Sany, Andrés, Juan
# Affiliation:    Universidad de los Andes
#
# Created:        2026-02-07
# Last updated:   2026-02-21
#
# Data input:     00_data/01_main_data.rds
# Data output:    02_outputs/
#
# Requirements:
#   - R version ≥ 4.2.0
#   - Packages: tidyverse, tidymodels, yardstick, fixest, rio
#
# Reproducibility:
#   - Set seed for all random operations
#   - Run after data cleaning script
#
# Notes:
#   - Model M9 is treated as the benchmark specification
#   - Influence diagnostics use LOOCV residual approximation
# =============================================================================

  
  
  ### setup
  cat("\f")
  rm(list = ls())
  library(pacman)
  p_load(tidyverse, rio, tidymodels, yardstick, fixest)
  options(scipen =  999)
  
  ## 1. Import data
  data = import("00_data/01_main_data.rds")
  
  ## 2. split model     
  
  # separar por grupos 
  set.seed(1369)
  split = initial_split(data, prop = 0.7)
  train = training(split)
  test = testing(split)
  
  ## 3. get metrics for previus models 
  M1 = lm(log(y_total_m_ha) ~ age + I(age^2), data = train ) 
  M2 = lm(log(y_total_m_ha) ~ sex, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  M3 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + relab + size_firm, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  M4 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + relab + size_firm + formalidad, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  
  
  M1_prediction = predict(M1, newdata = test) 
  M2_prediction = predict(M2, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  M3_prediction = predict(M3, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  M4_prediction = predict(M4, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  
  
  ## 4. Create new models
  formulas = list(
    M6 = log(y_total_m_ha) ~ sex + poly(age, 3) + max_educ_level + oficio + relab + size_firm + formalidad,
    M7 = log(y_total_m_ha) ~ sex + poly(age, 2) + max_educ_level * formalidad + oficio + relab + size_firm,
    M8 = log(y_total_m_ha) ~ sex + poly(age, 2) + max_educ_level + oficio + relab*size_firm + formalidad,
    M9 = log(y_total_m_ha) ~ sex + poly(age, 2) + oficio + formalidad + max_educ_level*formalidad + relab*size_firm,
    M10 = log(y_total_m_ha) ~ sex + poly(age, 2) + oficio + formalidad + max_educ_level*formalidad*age + relab*size_firm*age
  )
  
  # training models
  models = lapply(formulas, function(f) lm(f, data = train))
  models = list(M1, M2, M3, M4, models$M6, models$M7, models$M8, models$M9, models$M10)
  
  # predicciones
  preds = lapply(models[-c(1:4)], function(m) predict(m, newdata = test))
  preds = list(M1_prediction, M2_prediction, M3_prediction, M4_prediction, preds[[1]], preds[[2]], preds[[3]], preds[[4]], preds[[5]])
  
  rm(M1, M2, M3, M4)
  rm(M1_prediction, M2_prediction, M3_prediction, M4_prediction)
  rm(split, formulas)
  
  ## 5. extact metrics for new models
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
  
  
  ## get metrics for previus models 
  M1 = lm(log(y_total_m_ha) ~ age + I(age^2), data = train ) 
  M2 = lm(log(y_total_m_ha) ~ sex, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  M3 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + relab + size_firm, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  M4 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + relab + size_firm + formalidad, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  
  
  M1_prediction = predict(M1, newdata = test) 
  M2_prediction = predict(M2, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  M3_prediction = predict(M3, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  M4_prediction = predict(M4, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
  
  
  ## Create new models
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
  
  ## extact metrics for new models
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
  
  
  # ============================================================
  # 6. Residual distribution analysis
  # ============================================================
  
  
  # Extraer el mejor modelo
  best_model <- modelo[[9]]$modelo
  
  # Calcular leverage y residuos
  h <- hatvalues(best_model)
  e <- residuals(best_model)
  loo_resid <- e / (1 - h)
  
  # Crear tabla de influencia
  influence_df <- tibble(
    obs = 1:length(loo_resid),
    loo_resid = loo_resid,
    leverage = h
  )

  summary_influence <- influence_df %>%
    summarise(
      mean_leverage = mean(leverage),
      max_leverage = max(leverage),
      mean_loo_resid = mean(loo_resid),
      max_abs_loo_resid = max(abs(loo_resid))
    )
  
  summary_influence

  # Definir umbrales (p. ej., leverage > 2*promedio, residuo LOOCV > 3*sd)
  high_influence <- influence_df %>%
    filter(leverage > 2*mean(leverage) | abs(loo_resid) > 3*sd(loo_resid)) %>%
    arrange(desc(leverage))
  
  high_influence

  #-----------------
  #graph
  #-----------------
  
####################
# Leverage Graph (clean & professional)
####################

# Extraer el mejor modelo (M9)
best_model <- modelo[[9]]$modelo

# Calcular leverage y residuos LOOCV
h <- hatvalues(best_model)
e <- residuals(best_model)
loo_resid <- e / (1 - h)

# Crear tabla de influencia
influence_df <- tibble(
  obs = 1:length(loo_resid),
  loo_resid = loo_resid,
  leverage = h
)

# 🔥 LIMPIEZA CLAVE: remover infinitos y NaN
influence_df <- influence_df %>%
  mutate(
    loo_resid = ifelse(is.finite(loo_resid), loo_resid, NA_real_)
  )

# Crear indicador de alta influencia (robusto)
influence_df <- influence_df %>%
  mutate(
    high_influence =
      leverage > 2 * mean(leverage, na.rm = TRUE) |
      abs(loo_resid) > 3 * sd(loo_resid, na.rm = TRUE)
  )

# Resumen para reporte
summary_influence <- influence_df %>%
  summarise(
    mean_leverage = mean(leverage, na.rm = TRUE),
    max_leverage = max(leverage, na.rm = TRUE),
    mean_loo_resid = mean(loo_resid, na.rm = TRUE),
    max_abs_loo_resid = max(abs(loo_resid), na.rm = TRUE),
    pct_high_influence = mean(high_influence, na.rm = TRUE)
  )

summary_influence

# Tabla de observaciones influyentes
high_influence <- influence_df %>%
  filter(high_influence) %>%
  arrange(desc(leverage))

high_influence

# =================
# Gráfico limpio
# =================

####################
# Leverage Graph (clean & professional)
####################

# Extraer el mejor modelo (M9)
best_model <- modelo[[9]]$modelo

# Calcular leverage y residuos LOOCV
h <- hatvalues(best_model)
e <- residuals(best_model)
loo_resid <- e / (1 - h)

# Crear tabla de influencia
influence_df <- tibble(
  obs = 1:length(loo_resid),
  loo_resid = loo_resid,
  leverage = h
)

# 🔥 LIMPIEZA CLAVE: remover infinitos y NaN
influence_df <- influence_df %>%
  mutate(
    loo_resid = ifelse(is.finite(loo_resid), loo_resid, NA_real_)
  )

# Crear indicador de alta influencia (robusto)
influence_df <- influence_df %>%
  mutate(
    high_influence =
      leverage > 2 * mean(leverage, na.rm = TRUE) |
      abs(loo_resid) > 3 * sd(loo_resid, na.rm = TRUE)
  )

# Resumen para reporte
summary_influence <- influence_df %>%
  summarise(
    mean_leverage = mean(leverage, na.rm = TRUE),
    max_leverage = max(leverage, na.rm = TRUE),
    mean_loo_resid = mean(loo_resid, na.rm = TRUE),
    max_abs_loo_resid = max(abs(loo_resid), na.rm = TRUE),
    pct_high_influence = mean(high_influence, na.rm = TRUE)
  )

summary_influence

# Tabla de observaciones influyentes
high_influence <- influence_df %>%
  filter(high_influence) %>%
  arrange(desc(leverage))

high_influence

# =================
# Gráfico limpio
# =================

plot_df <- influence_df %>%
  filter(!is.na(loo_resid))

leverage_plot <- ggplot(plot_df, aes(x = leverage, y = loo_resid)) +
  geom_point(aes(color = high_influence), alpha = 0.6, size = 1.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("grey", "red")) +
  labs(
    title = "LOOCV Residuals vs Leverage (Best Model M9)",
    x = "Leverage",
    y = "LOOCV Residual",
    color = "High Influence"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Guardar figura
ggsave(
  "02_outputs/figures/leverage_outliers.png",
  plot = leverage_plot,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)



#=====================#
  # 6. Export results   #
  #=====================#
  
  saveRDS(modelo, "02_outputs/02_test_metrics_without_high_leverage.rds")
  
