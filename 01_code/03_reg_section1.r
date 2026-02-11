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
#   - Seed:           set.seed(12345)
#
# Output:
#   - Unconditional age–income regression
#   - Peak age estimate
#   - Bootstrap confidence interval for peak age
#
# Notes:
#   - This script must be run after the data cleaning step
# =============================================================================

## ---------------------------------------------------------------
## Load previously downloaded data
## ---------------------------------------------------------------

rm(list = ls())
pacman::p_load(
  tidyverse, rvest, data.table, dplyr,
  fixest, modelsummary, boot
)

db <- readRDS("00_data/01_main_data.rds")

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

## ===============================================================
## 1. Age–income profile estimation
## ===============================================================

# Linear estimation
model <- lm(
  log_w ~ age,
  data = db
)

model_fe<- feols(
  log_w ~ age,
  data = db
)

# OLS estimation
model1 <- lm(log_w ~ age + age2, data = db)

modelsummary(
  model1,
  coef_map = c(age = "Age", age2 = "Age squared"),
  digits = 3
)

# OLS with robust inference (fixest)
model1_fe <- feols(log_w ~ age + age2, data = db)

etable(
  model1_fe,
  digits = 3,
  dict = c(age = "Age", age2 = "Age squared")
)

# -----------------------------------------------------------------------------
# Peak age calculation
# -----------------------------------------------------------------------------

coefs <- coef(model1)

b_age  <- coefs["age"]
b_age2 <- coefs["age2"]

# Point estimate of peak age
peak_hat <- - b_age / (2 * b_age2)
peak_hat

# -----------------------------------------------------------------------------
# Bootstrap inference for peak age (XY bootstrap)
# -----------------------------------------------------------------------------

# Bootstrap statistic: peak age
peak_fn <- function(data, index) {
  f <- lm(log_w ~ age + age2, data = data, subset = index)
  coefs <- coef(f)
  b_age  <- coefs["age"]
  b_age2 <- coefs["age2"]
  peak <- - b_age / (2 * b_age2)
  return(peak)
}

# Sanity check
peak_fn(db, 1:nrow(db))


# -----------------------------------------------------------------------------
# Bootstrap implementation
# -----------------------------------------------------------------------------

set.seed(12345)

results_boot <- boot(
  data = db,
  statistic = peak_fn,
  R = 10000
)

results_boot


# -----------------------------------------------------------------------------
# Bootstrap confidence interval and distribution
# -----------------------------------------------------------------------------

boot.ci(results_boot, type = "perc")

hist(
  results_boot$t,
  main = "Bootstrap distribution of peak age",
  xlab = "Peak age"
)


## ===============================================================
## 2. Conditional age–income profile estimation
##    (Controls: labor supply and employment type)
## ===============================================================

model2 <- lm(
  log_w ~ age + age2 + max_educ_level + sex + formalidad + total_hours + factor(relab),
  data = db
)

modelsummary(
  model2,
  coef_map = c(
    age = "Age",
    age2 = "Age squared",
    total_hours = "Total hours worked"
  ),
  digits = 3
)


## ---------------------------------------------------------------
## Conditional model with fixed effects (relab)
## ---------------------------------------------------------------

model2_fe <- feols(
  log_w ~ age + age2 + total_hours + i(relab),
  data = db
)

etable(
  model2_fe,
  digits = 3,
  dict = c(
    age = "Age",
    age2 = "Age squared",
    total_hours = "Total hours worked"
  )
)


## ---------------------------------------------------------------
## Peak age estimation (conditional profile)
## ---------------------------------------------------------------

coefs_c <- coef(model2)

b_age_c  <- coefs_c["age"]
b_age2_c <- coefs_c["age2"]

peak_hat_cond <- - b_age_c / (2 * b_age2_c)
peak_hat_cond


## ---------------------------------------------------------------
## Bootstrap confidence interval for conditional peak
## ---------------------------------------------------------------

peak_fn_cond <- function(data, index) {
  
  f <- lm(
    log_w ~ age + age2 + total_hours + factor(relab),
    data = data,
    subset = index
  )
  
  coefs <- coef(f)
  
  b_age  <- coefs["age"]
  b_age2 <- coefs["age2"]
  
  peak <- - b_age / (2 * b_age2)
  return(peak)
}

set.seed(12345)

boot_peak_cond <- boot(
  data = db,
  statistic = peak_fn_cond,
  R = 10000
)

boot.ci(boot_peak_cond, type = "perc")


## ===============================================================
## 3. Comparison: unconditional vs. conditional age–income peak
## ===============================================================

c(
  Peak_unconditional = peak_hat,
  Peak_conditional   = peak_hat_cond
)


## ===============================================================
## 4. Summary regression table
## ===============================================================

## ---------------------------------------------------------------
## Confidence intervals
## --------------------------------------------------------------- 

# Unconditional quadratic
ci_uncond <- boot.ci(results_boot, type = "perc")$percent[4:5]

# Conditional quadratic
ci_cond <- boot.ci(boot_peak_cond, type = "perc")$percent[4:5]

# Rows with information for table
extra_rows <- list(
  "Implied peak age" = c(
    "",                                   
    round(peak_hat, 2),
    round(peak_hat_cond, 2)
  ),
  "95% CI (peak age)" = c(
    "",
    paste0("[", round(ci_uncond[1], 2), ", ", round(ci_uncond[2], 2), "]"),
    paste0("[", round(ci_cond[1], 2), ", ", round(ci_cond[2], 2), "]")
  )
)

# Table

etable(
  model_fe,
  model1_fe,
  model2_fe,
  
  dict = c(
    age         = "Age",
    age2        = "Age squared",
    total_hours = "Total hours worked"
  ),
  
  digits = 3,
  fitstat = ~ n + r2,
  headers = c(
    "Linear (Unconditional)",
    "Quadratic (Unconditional)",
    "Quadratic (Conditional)"
  ),
  extralines = extra_rows,
  
  file = "02_outputs/tables/age_income_peak.tex"
)




## ===============================================================
## 5. Visualization: age–labor income profiles
## ===============================================================

age_grid <- tibble(
  age  = seq(min(db$age, na.rm = TRUE),
             max(db$age, na.rm = TRUE),
             by = 1)
) %>%
  mutate(age2 = age^2)

## ---------------------------------------------------------------
## Prediction values (unconditional)
## ---------------------------------------------------------------

age_grid$pred_uncond <- predict(
  model1,
  newdata = age_grid
)

ggplot(age_grid, aes(x = age, y = pred_uncond)) +
  geom_line(size = 1.2, color = "steelblue") +
  labs(
    title = "Unconditional age–labor income profile",
    x = "Age",
    y = "Predicted log monthly labor income"
  ) +
  theme_minimal()

## ---------------------------------------------------------------
## Prediction values (conditional)
## ---------------------------------------------------------------

# Reference values for controls
hours_mean <- mean(db$total_hours, na.rm = TRUE)

age_grid_cond <- age_grid %>%
  mutate(
    total_hours = hours_mean,
    relab = levels(factor(db$relab))[1]
  )

# Predicted values (conditional)
age_grid_cond$pred_cond <- predict(
  model2,
  newdata = age_grid_cond
)

ggplot(age_grid_cond, aes(x = age, y = pred_cond)) +
  geom_line(size = 1.2, color = "darkred") +
  labs(
    title = "Conditional age–labor income profile",
    x = "Age",
    y = "Predicted log monthly labor income"
  ) +
  theme_minimal()

## ---------------------------------------------------------------
## Comparison: unconditional vs conditional
## ---------------------------------------------------------------

plot_df <- age_grid %>%
  select(age) %>%
  mutate(
    Unconditional = age_grid$pred_uncond,
    Conditional   = age_grid_cond$pred_cond
  ) %>%
  pivot_longer(-age, names_to = "Profile", values_to = "log_income")

p_age <- ggplot(plot_df, aes(x = age, y = log_income, color = Profile)) +
  geom_line(size = 1.2) +
  labs(
    title = "Age–labor income profiles",
    x = "Age",
    y = "Predicted log monthly labor income",
    color = "Specification"
  ) +
  theme_minimal()

ggsave(
  filename = "02_outputs/figures/age_income_profiles.png",
  plot = p_age,
  width = 7,
  height = 5,
  dpi = 300
)


