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

rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, janitor, data.table,
               caret, stargazer, boot, fixest)

#=========================================================
# 1. DATA IMPORT AND VARIABLE CONSTRUCTION
#=========================================================

## Import main dataset
db <- readRDS('00_data/01_main_data.rds')

## Generate log wages and quadratic age term
db <- db %>%
  mutate(
    log_w = log(y_total_m),
    age2  = age^2
  )

#=========================================================
# 2. TRAIN–TEST SPLIT
#=========================================================

## Training sample: chunks 1–7
train_db <- db %>% filter(chunk %in% 1:7)

## Validation sample: chunks 8–10
test_db  <- db %>% filter(chunk %in% 8:10)

#=========================================================
# 3. MODEL ESTIMATION (TRAINING SAMPLE)
#=========================================================

## Benchmark model (mean only)
m0_train <- feols(log_w ~ 1, data = train_db)

#---------------------------------------------------------
# Age–earnings profiles
#---------------------------------------------------------

m1_train <- feols(log_w ~ age, data = train_db)

m2_train <- feols(log_w ~ age + age2, data = train_db)

m3_train <- feols(log_w ~ age + age2 + total_hours + factor(relab),
                  data = train_db)

#---------------------------------------------------------
# Gender wage gap specifications
#---------------------------------------------------------

## Raw gender gap
m4_train <- feols(log_w ~ factor(sex),
                  data = train_db)

## Gender gap with controls
m5_train <- feols(
  log_w ~ sex + age + age2 +
    max_educ_level + relab + oficio +
    formalidad + size_firm,
  data = train_db
)

## Alternative specification
m6_train <- feols(
  log_w ~ sex + age + age2 +
    max_educ_level + relab +
    oficio + formalidad + size_firm,
  data = train_db
)

#---------------------------------------------------------
# Extended structural specifications
#---------------------------------------------------------

m7_train <- feols(log_w ~
                    sex +
                    poly(age, 2) +
                    max_educ_level +
                    formalidad +
                    relab +
                    oficio +
                    size_firm,
                  data = train_db)

m8_train <- feols(log_w ~
                    sex +
                    poly(age, 2) +
                    max_educ_level * formalidad +
                    relab +
                    oficio +
                    size_firm,
                  data = train_db)

m9_train <- feols(log_w ~
                    sex * formalidad +
                    poly(age, 2) +
                    max_educ_level +
                    relab +
                    oficio +
                    size_firm,
                  data = train_db)

m10_train <- feols(log_w ~
                     sex +
                     max_educ_level * poly(age, 2) +
                     formalidad +
                     relab +
                     oficio +
                     size_firm,
                   data = train_db)

## Selected model: triple interaction
m11_train <- feols(log_w ~
                     sex * max_educ_level * formalidad +
                     poly(age, 2) +
                     relab +
                     oficio +
                     size_firm +
                     total_hours,
                   data = train_db)

#=========================================================
# 4. OUT-OF-SAMPLE PREDICTION (VALIDATION SAMPLE)
#=========================================================

## Generate predictions for each model
test_db$m0_pred  <- predict(m0_train,  newdata = test_db)
test_db$m1_pred  <- predict(m1_train,  newdata = test_db)
test_db$m2_pred  <- predict(m2_train,  newdata = test_db)
test_db$m3_pred  <- predict(m3_train,  newdata = test_db)
test_db$m4_pred  <- predict(m4_train,  newdata = test_db)
test_db$m5_pred  <- predict(m5_train,  newdata = test_db)
test_db$m6_pred  <- predict(m6_train,  newdata = test_db)
test_db$m7_pred  <- predict(m7_train,  newdata = test_db)
test_db$m8_pred  <- predict(m8_train,  newdata = test_db)
test_db$m9_pred  <- predict(m9_train,  newdata = test_db)
test_db$m10_pred <- predict(m10_train, newdata = test_db)
test_db$m11_pred <- predict(m11_train, newdata = test_db)

#---------------------------------------------------------
# Compute validation RMSE
#---------------------------------------------------------

rmse0  <- with(test_db, round(sqrt(mean((log_w - m0_pred)^2)), 3))
rmse1  <- with(test_db, round(sqrt(mean((log_w - m1_pred)^2)), 3))
rmse2  <- with(test_db, round(sqrt(mean((log_w - m2_pred)^2)), 3))
rmse3  <- with(test_db, round(sqrt(mean((log_w - m3_pred)^2)), 3))
rmse4  <- with(test_db, round(sqrt(mean((log_w - m4_pred)^2)), 3))
rmse5  <- with(test_db, round(sqrt(mean((log_w - m5_pred)^2)), 3))
rmse6  <- with(test_db, round(sqrt(mean((log_w - m6_pred)^2)), 3))
rmse7  <- with(test_db, round(sqrt(mean((log_w - m7_pred)^2)), 3))
rmse8  <- with(test_db, round(sqrt(mean((log_w - m8_pred)^2)), 3))
rmse9  <- with(test_db, round(sqrt(mean((log_w - m9_pred)^2)), 3))
rmse10 <- with(test_db, round(sqrt(mean((log_w - m10_pred)^2)), 3))
rmse11 <- with(test_db, round(sqrt(mean((log_w - m11_pred)^2)), 3))

#=========================================================
# 5. MODEL SELECTION
#=========================================================

## Compare validation RMSE across models
rmse <- c(rmse0, rmse1, rmse2, rmse3, rmse4, rmse5,
          rmse6, rmse7, rmse8, rmse9, rmse10, rmse11)

results <- data.frame(
  model = factor(c(
    "M0: Mean benchmark",
    "M1: Age profile (linear)",
    "M2: Age profile (quadratic)",
    "M3: Age + hours + labor controls",
    "M4: Raw gender gap",
    "M5: Gender gap (FWL controls)",
    "M6: Gender gap + household controls (FWL)",
    "M7: Baseline structural model",
    "M8: Education × Formality",
    "M9: Gender × Formality",
    "M10: Education returns vary by age",
    "M11: Gender × Education × Formality"
  ), ordered = TRUE),
  RMSE = rmse
)

results

## Selected model based on lowest validation RMSE:
## M11: Gender × Education × Formality

#=========================================================
# 6. LOOCV PREDICTION ERROR (TRAINING SAMPLE)
#=========================================================

## Compute leave-one-out residuals using OLS identity
e <- residuals(m11_train)
h <- hatvalues(m11_train)

h[h > 0.999999] <- NA

e_loo <- e / (1 - h)

## LOOCV RMSE
loocv_rmse <- sqrt(mean(e_loo^2, na.rm = TRUE))
loocv_rmse

#=========================================================
# 7. DIAGNOSTIC ANALYSIS
#=========================================================

#---------------------------------------------------------
# (A) Observations hardest to predict
#---------------------------------------------------------

train_db$abs_error <- abs(residuals(m11_train))

train_db |>
  arrange(desc(abs_error)) |>
  head(10)

#---------------------------------------------------------
# (B) Observation-level influence on coefficients
#     || β(-i) − β ||^2
#---------------------------------------------------------

X <- model.matrix(m11_train)
beta_hat <- coef(m11_train)
XtX_inv  <- solve(t(X) %*% X)

e <- residuals(m11_train)
h <- hatvalues(m11_train)

n <- nrow(X)
influence_measure <- numeric(n)

for(i in 1:n){
  xi <- matrix(X[i, ], ncol = 1)
  delta_beta <- -(XtX_inv %*% xi * e[i]) / (1 - h[i])
  influence_measure[i] <- sum(delta_beta^2)
}

train_db$beta_influence <- influence_measure

train_db |>
  arrange(desc(beta_influence)) |>
  head(10)

#---------------------------------------------------------
# (C) Compare leverage and coefficient influence
#---------------------------------------------------------

train_db$id_row  <- 1:nrow(train_db)
train_db$leverage <- h

high_lev <- train_db$id_row[train_db$leverage > 0.2]

high_inf <- train_db %>%
  arrange(desc(beta_influence)) %>%
  slice(1:10) %>%
  pull(id_row)

intersect(high_lev, high_inf)

#---------------------------------------------------------
# STEP 1: Define controls (W) and residualize log wages
#---------------------------------------------------------

m_y <- lm(log_w ~ poly(age, 2) +
            relab +
            oficio +
            size_firm +
            total_hours,
          data = train_db)

Y_resid <- resid(m_y)

#---------------------------------------------------------
# STEP 2: Construct matrix of variables of interest (D)
#         Triple interaction: sex × education × formalidad
#---------------------------------------------------------

D <- model.matrix(~ sex * max_educ_level * formalidad,
                  data = train_db)

#---------------------------------------------------------
# STEP 3: Residualize each column of D with respect to controls
#---------------------------------------------------------

D_resid <- matrix(NA,
                  nrow = nrow(train_db),
                  ncol = ncol(D))

for(j in 1:ncol(D)){
  
  m_d <- lm(D[, j] ~ poly(age, 2) +
              relab +
              oficio +
              size_firm +
              total_hours,
            data = train_db)
  
  D_resid[, j] <- resid(m_d)
}

#---------------------------------------------------------
# STEP 4: Estimate partial regression (FWL estimator)
#         Using lm() for numerical stability
#---------------------------------------------------------

model_fwl <- lm(Y_resid ~ D_resid - 1)

beta_fwl <- coef(model_fwl)

#---------------------------------------------------------
# STEP 5: Compute observation-level influence
#         || β(-i) − β ||^2
#---------------------------------------------------------

X_fwl <- D_resid
y_fwl <- Y_resid

XtX_inv_fwl <- solve(t(X_fwl) %*% X_fwl)

resid_fwl <- resid(model_fwl)

h_fwl <- hatvalues(model_fwl)

n <- nrow(X_fwl)

influence_fwl <- numeric(n)

for(i in 1:n){
  
  xi <- matrix(X_fwl[i, ], ncol = 1)
  
  delta_beta <- -(XtX_inv_fwl %*% xi *
                    resid_fwl[i]) / (1 - h_fwl[i])
  
  influence_fwl[i] <- sum(delta_beta^2)
}

train_db$beta_influence_fwl <- influence_fwl

#---------------------------------------------------------
# STEP 6: Display most influential observations
#---------------------------------------------------------

train_db %>%
  arrange(desc(beta_influence_fwl)) %>%
  head(10)


plot(train_db$leverage,
     train_db$beta_influence,
     xlab = "Leverage",
     ylab = "Coefficient Influence",
     main = "Leverage vs Coefficient Influence")
