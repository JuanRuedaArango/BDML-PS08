# =============================================================================
# Project: PS – Prediction Income
#
# Description: Age–labor income profile and peak age (OLS + bootstrap)
#
# Authors:        Sany, Andrés, and Juan
# Affiliation:    Universidad de los Andes
#
# Created:        2026-02-12
# Last updated:   2026-02-17
#
# Reproducibility:
#   - R version:      ≥ 4.0
#   - Seed:           set.seed(369)
#
# Output:
#   - Unconditional and conditional age–income regression
#   - Bootstrap confidence interval for peak age and wage profile
# =============================================================================

rm(list = ls())
pacman::p_load(tidyverse, boot, fixest, janitor)

# =============================================================================
# 1. DATA
# =============================================================================

df <- readRDS("00_data/01_main_data.rds")

# Asegurar factor
df$sex <- factor(df$sex)

# =============================================================================
# 2. MODEL
# =============================================================================

form1 <- log(y_total_m_ha) ~ sex*age + sex*I(age^2) +
  max_educ_level + relab + oficio + formalidad +
  size_firm + total_menores + total_seniors_inactivos

model <- lm(form1, data = df)
summary(model)

# =============================================================================
# 3. PEAK AGE (analítico)
# =============================================================================

b <- coef(model)

getb <- function(nm1, nm2 = NULL){
  if(is.null(nm2)) return(unname(b[nm1]))
  i1 <- paste0(nm1, ":", nm2)
  i2 <- paste0(nm2, ":", nm1)
  unname(b[ifelse(i1 %in% names(b), i1, i2)])
}

b_age  <- getb("age")
b_age2 <- getb("I(age^2)")

female_lab <- levels(df$sex)[2]
sex_term   <- paste0("sex", female_lab)

b_age_F  <- getb(sex_term, "age")
b_age2_F <- getb(sex_term, "I(age^2)")

peak_age_m <- -b_age / (2*b_age2)
peak_age_f <- -(b_age + b_age_F) / (2*(b_age2 + b_age2_F))

# =============================================================================
# 4. BOOTSTRAP
# =============================================================================

peak_age_fn_gender <- function(data, index){
  
  d <- data[index, ]
  d$sex <- factor(d$sex, levels = levels(data$sex))
  m <- lm(form1, data = d)
  b <- coef(m)
  
  getb <- function(nm1, nm2 = NULL){
    if(is.null(nm2)) return(if(nm1 %in% names(b)) b[nm1] else 0)
    i1 <- paste0(nm1,":",nm2)
    i2 <- paste0(nm2,":",nm1)
    hit <- c(i1,i2)[c(i1,i2)%in%names(b)]
    if(length(hit)==0) return(0)
    b[hit[1]]
  }
  
  female_lab <- levels(d$sex)[2]
  sex_term   <- paste0("sex", female_lab)
  
  a_m <- getb("age")
  q_m <- getb("I(age^2)")
  a_f <- a_m + getb(sex_term,"age")
  q_f <- q_m + getb(sex_term,"I(age^2)")
  
  c(
    male   = if(q_m<0) -a_m/(2*q_m) else NA,
    female = if(q_f<0) -a_f/(2*q_f) else NA
  )
}

set.seed(1369)
bootA <- boot(df, peak_age_fn_gender, R = 1000)

ci <- function(x) quantile(x, c(.025,.975), na.rm=TRUE)

tabla_picos <- tibble(
  sexo = c("Masculino","Femenino"),
  peak_age = c(peak_age_m, peak_age_f),
  ci_lo = c(ci(bootA$t[,1])[1], ci(bootA$t[,2])[1]),
  ci_hi = c(ci(bootA$t[,1])[2], ci(bootA$t[,2])[2])
)

tabla_picos

# =============================================================================
# 5. PREDICTED CURVES
# =============================================================================

ages <- seq(min(df$age), max(df$age), by=1)

avg_prof <- df |>
  summarise(across(everything(), ~{
    if (is.numeric(.x)) {
      mean(.x, na.rm = TRUE)
    } else {
      # Tomar el valor más frecuente (modo) y conservar niveles
      factor(names(sort(table(.x), decreasing = TRUE))[1],
             levels = levels(.x))
    }
  }))

build_curve <- function(sex_label){
  
  nd <- avg_prof[rep(1,length(ages)),]
  nd$age <- ages
  nd$sex <- factor(sex_label, levels=levels(df$sex))
  
  pr <- predict(model, newdata=nd, se.fit=TRUE)
  
  tibble(
    sex = sex_label,
    age = ages,
    wage = exp(pr$fit),
    ci_lower = exp(pr$fit - 1.96*pr$se.fit),
    ci_upper = exp(pr$fit + 1.96*pr$se.fit)
  )
}

plot_data <- bind_rows(
  build_curve(levels(df$sex)[1]),
  build_curve(levels(df$sex)[2])
)

# salario en edad pico
tabla_picos$peak_wage <- sapply(1:nrow(tabla_picos), function(i){
  nd <- avg_prof
  nd$age <- tabla_picos$peak_age[i]
  nd$sex <- factor(tabla_picos$sexo[i], levels=levels(df$sex))
  exp(predict(model,newdata=nd))
})

# Crear tabla de picos por sexo
tabla_peak <- plot_data %>%
  group_by(sex) %>%
  summarise(
    peak_wage = max(wage),                   # salario máximo
    peak_age  = age[which.max(wage)]         # edad en el salario máximo
  )


# =============================================================================
# 6. GRAPH
# =============================================================================

pal_line <- c("Female"="hotpink","Male"="skyblue")
pal_fill <- c("Female"="hotpink","Male"="skyblue")

plot_gap2 <- ggplot(plot_data, aes(x = age, y = wage, color = sex)) +
  
  # Curvas predichas
  geom_line(aes(y = ci_lower), linetype = "dashed", linewidth = 0.6,
            show.legend = FALSE) +
  geom_line(aes(y = ci_upper), linetype = "dashed", linewidth = 0.6,
            show.legend = FALSE) +
  geom_line(linewidth = 1.0) +
  # Banda de IC
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = sex), alpha = 0.2, color = NA) +
  
  # Líneas verticales edad pico
  geom_vline(data = tabla_peak, aes(xintercept = peak_age, color = sex),
             linetype = "dashed", linewidth = 0.4) +
  
  # Puntos salario pico
  geom_point(data = tabla_peak, aes(x = peak_age, y = peak_wage, color = sex), size = 3) +
  
  # Etiquetas edad pico sobre el punto
  geom_text(data = tabla_peak,
            aes(x = peak_age, y = peak_wage, label = paste0("", peak_age)),
            vjust = -1, fontface = "bold", color = "black", size = 3) +
  
  # Colores y etiquetas
  scale_color_manual(values = c("Female" = "hotpink", "Male" = "skyblue")) +
  scale_fill_manual(values = c("Female" = "hotpink", "Male" = "skyblue")) +
  
  labs(x = "Age", y = "Predicted Income (COP)", color = NULL, fill = NULL) +
  theme_classic()

plot_gap2

# 7. Export graph
ggsave("02_outputs/figures/age_income_gap_profile.png",
       plot_gap2,    # <- aquí va el objeto de ggplot, no 'plot'
       width = 6,
       height = 4,
       dpi = 300)
