# =============================================================================
# Project:        PS – Prediction Income
# =============================================================================

### 1. Setup -------------------------------------------------------------------

rm(list = ls())

library(pacman)
p_load(tidyverse, rio, janitor, data.table, moments,
       modelsummary, gt)

### 2. Load data ----------------------------------------------------------------

df <- import('00_data/01_main_data.rds', setclass = 'tibble') %>% 
  select(-directorio, -secuencia_p, -orden, -f_weights)

dir.create("02_outputs/tables", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 3. CONTINUOUS VARIABLES
# =============================================================================

table_continuous <- df %>% 
  select(where(is.numeric)) %>% 
  rename(
    'Ingresos laborales por hora' = y_total_m_ha,
    'Edad' = age,
    'N. menores de edad en el hogar' = total_menores,
    'N. adultos mayores inactivos en el hogar' = total_seniors_inactivos
  )

P10 <- function(x) quantile(x, probs = 0.1, na.rm = TRUE)
P90 <- function(x) quantile(x, probs = 0.9, na.rm = TRUE)

table_continuous <- datasummary(
  All(table_continuous) ~ Mean + SD + Min + P10 + Median + P90 + Max,
  data = table_continuous,
  output = "data.frame",
  fmt = function(x) format(round(x,2), big.mark='.', dec.mark=',')
) %>% 
  arrange(desc(Mean)) %>%
  rename(
    Promedio = Mean,
    SD = SD,
    Mediana = Median
  )

table_continuous_gt <- table_continuous %>%
  gt() %>%
  tab_header(title = "Distribución de las variables numéricas") %>%
  tab_options(
    latex.use_longtable = FALSE,
    table.font.size = px(5)   # Tamaño ideal para Beamer
  )

gtsave(
  table_continuous_gt,
  "02_outputs/tables/01_tabla_continuas.tex"
)

# =============================================================================
# 4. CATEGORICAL VARIABLES
# =============================================================================

vars_factores <- df %>% 
  select(where(is.factor)) %>% 
  colnames()

table_categorical <- map(vars_factores, function(x){
  
  df_percent <- df %>% 
    select(var = all_of(x)) %>% 
    count(var) %>% 
    mutate(
      prop = n / sum(n),
      prop = paste0(round(prop*100,1), "%"),
      n = format(n, big.mark=',')
    )
  
  df_collapse <- df %>% 
    select(var = all_of(x), y_total_m_ha) %>% 
    group_by(var) %>% 
    summarise(
      average = mean(y_total_m_ha, na.rm = TRUE),
      sd = sd(y_total_m_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(
      average = format(round(average,0), big.mark=','),
      sd = format(round(sd,0), big.mark=',')
    )
  
  left_join(df_percent, df_collapse, by="var") %>% 
    mutate(group = x)
  
}) %>% bind_rows()

# Renombrar grupos
table_categorical <- table_categorical %>% 
  mutate(group = case_when(
    group == 'sex' ~ 'Sexo',
    group == 'max_educ_level' ~ 'Máximo nivel educativo',
    group == 'formalidad' ~ 'Formalidad',
    group == 'relab' ~ 'Posición ocupacional',
    group == 'size_firm' ~ 'Cantidad de trabajadores de la empresa',
    group == 'oficio' ~ 'Oficio',
    TRUE ~ group
  ))

table_categorical <- table_categorical %>% 
  select(
    ' ' = var,
    'N' = n,
    '%' = prop,
    'Promedio' = average,
    'SD' = sd,
    group
  )

table_categorical_gt <- table_categorical %>%
  gt(groupname_col = "group") %>%
  tab_options(
    latex.use_longtable = FALSE,
    table.font.size = px(6),
    data_row.padding = px(0.5),           # ↓ espacio entre filas
    column_labels.padding = px(1),      # ↓ espacio encabezado
    row_group.padding = px(1),          # ↓ espacio entre grupos
    table_body.hlines.width = px(0.3)   # líneas más finas
  )

gtsave(
  table_categorical_gt,
  "02_outputs/tables/01_tabla_categoricas.tex"
)
