# =============================================================================
# Project:        PS – Income Prediction
#
# Script:         04_prediction_evaluation.R
#
# Description:    Compares model predictive performance using test RMSE and
#                 LOOCV RMSE, with and without high-leverage observations.
#                 Also analyzes residual distribution and skewness.
#
# Authors:        Sany, Andrés, Juan
# Affiliation:    Universidad de los Andes
#
# Created:        2026-02-07
# Last updated:   2026-02-21
#
# Script type:    Estimation and inference
#
# Reproducibility:
#   - R version ≥ 4.2.0
#   - Seed set for reproducibility: 1369
#
# Inputs:
#   - 00_data/01_main_data.rds
#   - 02_outputs/01_test_metrics.rds
#   - 02_outputs/02_test_metrics_without_high_leverage.rds
#
# Outputs:
#   - 02_outputs/tables/04_prediction_table.tex
#   - 02_outputs/figures/histograma_residuales.png
# =============================================================================

### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse, rio, tidymodels, yardstick, fixest, kableExtra, moments)
options(scipen =  999)


## 1. Import data
data = import("00_data/01_main_data.rds")
resultados_modelos = import("02_outputs/01_test_metrics.rds")
resultados_modelos_sin_outlieres = import("02_outputs/02_test_metrics_without_high_leverage.rds")

## 2. test vs train set     

# grouping
set.seed(1369)
split = initial_split(data, prop = 0.7)
train = training(split)
test = testing(split)

## 3. error table   
tabla = lapply(X = 1:length(resultados_modelos), FUN = function(x) {
  metric = resultados_modelos[[x]]$metric  
  modelo = resultados_modelos[[x]]$modelo
  
  h <- hatvalues(modelo)       
  h[h>0.9999999] = NA
  e <- residuals(modelo)          
  loo_resid <- e / (1 - h)     
  loo_err   <- mean(loo_resid^2, na.rm = T)  
  
  metric = metric |> 
    mutate(loocv = loo_err, )
  
  
}) |> bind_rows() |> 
  filter(.metric == "rmse")  |> 
  mutate(.metric = paste0("Modelo ", row_number())) |> 
  select(-.estimator)

tabla_sin_outliers = lapply(X = 1:length(resultados_modelos_sin_outlieres), FUN = function(x) {
  metric = resultados_modelos_sin_outlieres[[x]]$metric  
  modelo = resultados_modelos_sin_outlieres[[x]]$modelo
  
  h <- hatvalues(modelo)       
  h[h>0.9999999] = NA
  e <- residuals(modelo)          
  loo_resid <- e / (1 - h)     
  loo_err   <- mean(loo_resid^2, na.rm = T)  
  
  metric = metric |> 
    mutate(loocv = loo_err)
  
  
}) |> bind_rows() |> 
  filter(.metric == "rmse")  |> 
  mutate(.metric = paste0("Modelo ", row_number())) |> 
  select(-.estimator)

tabla = left_join(tabla, tabla_sin_outliers, by =c(".metric") )

# pasar a latex
tabla = tabla |> 
  mutate(.estimate.x = formatC(.estimate.x, format = "f", digits = 3),
         loocv.x = formatC(loocv.x, format = "f", digits = 3),
         .estimate.y = formatC(.estimate.y, format = "f", digits = 3), 
         loocv.y = formatC(loocv.y, format = "f", digits = 3)) 

tabla = tabla |> 
  select(.metric, loocv.x, loocv.y, .estimate.x, .estimate.y )   |>     
  kable(format = "latex", 
        col.names = c("", "With outliers", "Without outliers", "With outliers", "Without outliers"),
        align = c("lccccc"),
        booktabs = TRUE) |> 
  add_header_above(header = c("", "LOOCV RMSE" = 2, "Test RMSE" = 2)) |> 
  kable_styling(latex_options = "hold_position", full_width = F) |> 
  footnote(
    general = c(
      paste0("Train Obs. (Outliers-No outliers): ", resultados_modelos[[9]]$train_size, "-", resultados_modelos_sin_outlieres[[9]]$train_size), 
      "Dos observaciones fueron eliminadas en LOOCV para evitar leverage de 1 en el test set"
    ),
    general_title = "", 
    escape = FALSE
  )

## 4. resid grafico 
writeLines(text = tabla,con = "02_outputs/tables/04_prediction_table.tex")
tabla = readLines("02_outputs/tables/04_prediction_table.tex")
tabla = tabla[-c(1,length(tabla))]
writeLines(text = tabla, con = "02_outputs/tables/04_prediction_table.tex")


#============================================================
#GRAPH 
#============================================================

##: 2.prepare data
m_completa = tibble(proviene = "Muestra Completa", 
                    residuales = resultados_modelos[[9]]$modelo$residuals)

out_completa = tibble(proviene = "Muestra sin High Leverage", 
                      residuales = resultados_modelos_sin_outlieres[[9]]$modelo$residuals)

output = bind_rows(m_completa, out_completa) 


skew_data <- output %>%
  group_by(proviene) %>%
  summarise(skew = round(skewness(residuales), 3))

# graficar histograma + texto con skewness
plot = ggplot(output, aes(x = residuales)) +
  geom_histogram(binwidth = 0.01, fill = "hotpink", alpha = 0.5) +
  labs(title = "",
       x = "Residuales", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~proviene, scales = "free_x") +
  geom_text(data = skew_data,
            aes(x = 0, y = Inf,
                label = paste0("Skewness = ", skew)),
            vjust = 1.5, inherit.aes = FALSE)

ggsave(plot, file = "02_outputs/figures/histograma_residuales.png", width = 10, height = 8, units = "in", dpi = 300)
