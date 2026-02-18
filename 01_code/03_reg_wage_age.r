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
# Reproducibility:
#   - R version:      ≥ 4.0
#   - Seed:           set.seed(369)
#
# Output:
#   - Unconditional and conditional age–income regression
#   - Bootstrap confidence interval for peak age and wage profile
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
  fixest, ggplot2, modelsummary, boot
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

set.seed(1369)

results_boot <- boot(
  data = db,
  statistic = peak_fn,
  R = 3000
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
  log_w ~ age + age2 + total_hours + factor(relab),
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

set.seed(1369)

boot_peak_cond <- boot(
  data = db,
  statistic = peak_fn_cond,
  R = 3000
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

## 4.1 Confidence intervals

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

# =============================================================================
# Tables
# =============================================================================

etable(
  model_fe,
  model1_fe,
  model2_fe,
  
  dict = c(
    "log_w"        = "Log Monthly Labor Income",
    "(Intercept)" = "Constant",
    age           = "Age",
    age2          = "Age squared",
    total_hours   = "Total hours worked",
    "relab "    ="",      # 🔹 elimina el prefijo relab
    sexFemale     = "Female",
    r2            = "R²",
    ar2           = "Adjusted R²",
    rmse          = "Root Mean Squared Error",
    n             = "Number of observations"
  ),
  
  headers = c(
    "Linear",
    "Quadratic (Unconditional)",
    "Quadratic (Conditional)"
  ),
  
  depvar = TRUE,
  digits = 3,
  fitstat = ~ n + r2 + ar2 + rmse,
  style.tex = style.tex("qje"),
  file = "02_outputs/tables/02_model_age_income_peak.tex",
  replace = TRUE
)

#Additional another format table

modelsummary(
  list(
    "Linear<br>(Unconditional)"    = model_fe,
    "Quadratic<br>(Unconditional)" = model1_fe,
    "Quadratic<br>(Conditional)"   = model2_fe
    ),
  output = "02_outputs/tables/02_model_age_income_peak.md",
  coef_map = c(
    "(Intercept)" = "Constante",
    age           = "Age",
    age2          = "Age square",
    total_hours   = "Total hours worked",
    
    # Labels mapping for relab
    "relabObreroempleadodeempresaparticular" = "Private firm employee",
    "relabTrabajadorporcuentapropia"        = "Self-employed",
    "relabEmpleadodoméstico"                 = "Domestic worker",
    "relabPatrónoempleador"                  = "Employer",
    "relabOtro"                              = "Other",
    "relabJornaleroopeón"                    = "Day laborer",
    
    # Gender
    sexFemenino   = "Female",
    sexMasculino  = "Male"
  ),
  stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
  fmt = 3
)


## ===============================================================
## 5. Visualization: age–labor income profiles (WITH CI)
## ===============================================================

age_grid <- tibble(
  age = seq(min(db$age, na.rm = TRUE),
            max(db$age, na.rm = TRUE),
            by = 1)
) %>%
  mutate(age2 = age^2)

## ---------------------------------------------------------------
## Unconditional predictions + SE
## ---------------------------------------------------------------

pred1 <- predict(model1, newdata = age_grid, se.fit = TRUE)

age_grid <- age_grid %>%
  mutate(
    pred_uncond = pred1$fit,
    se_uncond   = pred1$se.fit,
    lo_uncond   = pred_uncond - 1.96 * se_uncond,
    hi_uncond   = pred_uncond + 1.96 * se_uncond
  )


## ---------------------------------------------------------------
## Conditional predictions + SE
## ---------------------------------------------------------------

hours_mean <- mean(db$total_hours, na.rm = TRUE)

age_grid_cond <- age_grid %>%
  mutate(
    total_hours = hours_mean,
    relab = levels(factor(db$relab))[1]
  )

pred2 <- predict(model2, newdata = age_grid_cond, se.fit = TRUE)

age_grid_cond <- age_grid_cond %>%
  mutate(
    pred_cond = pred2$fit,
    se_cond   = pred2$se.fit,
    lo_cond   = pred_cond - 1.96 * se_cond,
    hi_cond   = pred_cond + 1.96 * se_cond
  )


## ---------------------------------------------------------------
## Combine for plotting
## ---------------------------------------------------------------

plot_df <- bind_rows(
  
  age_grid %>%
    transmute(
      age,
      Profile = "Unconditional",
      fit = pred_uncond,
      ci_lower = lo_uncond,
      ci_upper = hi_uncond
    ),
  
  age_grid_cond %>%
    transmute(
      age,
      Profile = "Conditional",
      fit = pred_cond,
      ci_lower = lo_cond,
      ci_upper = hi_cond
    )
)



## ---------------------------------------------------------------
## Plot with shaded CI bands + dashed borders
## ---------------------------------------------------------------

## Peak age (máximo por perfil)

peak_df <- plot_df %>%
  group_by(Profile) %>%
  filter(fit == max(fit, na.rm = TRUE)) %>%
  slice(1) %>%  
  ungroup()

p_age <- ggplot(plot_df,
                aes(x = age,
                    y = fit,
                    color = Profile,
                    fill  = Profile)) +
  
  # banda CI
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              alpha = 0.18,
              color = NA) +
  
  # límites punteados
  geom_line(aes(y = ci_lower),
            linetype = "dashed",
            linewidth = 0.6,
            show.legend = FALSE) +
  geom_line(aes(y = ci_upper),
            linetype = "dashed",
            linewidth = 0.6,
            show.legend = FALSE) +
  
  # línea principal
  geom_line(linewidth = 1.0) +
  
  # -------- PEAK --------
geom_vline(data = peak_df,
           aes(xintercept = age, color = Profile),
           linetype = "longdash",
           linewidth = 0.3,
           show.legend = FALSE) +
  
  geom_point(data = peak_df,
             aes(x = age, y = fit),
             size = 3,
             show.legend = FALSE) +
  
  geom_text(data = peak_df,
            aes(x = age,
                y = fit,
                label = paste0("", round(age,1))),
            vjust = -1.1,
            fontface = "italic",
            size = 4,
            show.legend = FALSE) +
  
  # Colores rosa y celeste
  scale_color_manual(values = c("Unconditional" = "hotpink", "Conditional" = "blue"
  )) +
  scale_fill_manual(values = c("Unconditional" = "hotpink", "Conditional" = "blue"
  )) +
  
  labs(
    title = "",
    x = "Age",
    y = "Predicted log monthly labor income",
    color = "Specification",
    fill  = "Specification"
  ) +
  
  theme_classic()  

ggsave("02_outputs/figures/age_income_profiles.png", 
       p_age, width = 7, height = 5, dpi = 300)

