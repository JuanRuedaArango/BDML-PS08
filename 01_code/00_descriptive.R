# =============================================================================
# Project:        PS – Prediction Income
# =============================================================================

### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse,rio,janitor,data.table,moments,
       modelsummary,ggpubr,gt,gtsummary,GGally,skimr,
       kableExtra,knitr)

##==: 1. Load data
df = import('00_data/01_main_data.rds',setclass = 'tibble') %>% 
  select(-directorio,-secuencia_p,-orden,-f_weights)

##==: 2. Continuous variables

table_continuous = df %>% 
  select(where(is.numeric)) %>% 
  rename('Ingresos laborales por hora' = y_total_m_ha,
         'Edad' = age,
         'N. menores de edad en el hogar' = total_menores,
         'N. adultos mayores inactivos en el hogar' = total_seniors_inactivos)

P10 = function(x) quantile(x,probs = 0.1)
P90 = function(x) quantile(x,probs = 0.9)

table_continuous = datasummary(
  All(table_continuous) ~ Mean + SD + Min + P10 + Median + P90 + Max,
  data = table_continuous,
  output = 'data.frame',
  fmt = function(x) format(round(x,2),big.mark='.',dec.mark=',')
) %>% 
  arrange(desc(Mean)) 

table_continuous = table_continuous %>% 
  rename('Promedio' = "Mean",
         'SD' = "SD",
         'Mediana' = "Median")

table_continuous = table_continuous %>% 
  gt() %>%
  tab_header(title = 'Distribución de las variables númericas') %>%
  tab_options(latex.use_longtable = FALSE)

##==: 3. Categorical variables

vars_factores = df %>% 
  select(where(is.factor)) %>% 
  colnames()

table_categorical = map(.x = vars_factores,.f = function(x){
  
  df_percent = df %>% 
    select(var = any_of(x)) %>% 
    count(var) %>% 
    mutate(prop = n/sum(n),
           prop = round(prop,3),
           prop = format(prop*100,dec.mark='.') ,
           prop = paste0(prop,'%'),
           n = format(n,big.mark=','))
  
  df_collapse = df %>% 
    select(var = any_of(x),y_total_m_ha) %>% 
    group_by(var) %>% 
    summarise(average = mean(y_total_m_ha),
              sd = sd(y_total_m_ha),
              .groups = "drop") %>% 
    mutate(across(c(average,sd), ~ format(round(.x,0),big.mark=',')))
  
  left_join(df_percent,df_collapse,by="var") %>% 
    mutate(group = x)
}) %>% list_rbind()

table_categorical = table_categorical %>% 
  mutate(group = case_when(
    group == 'sex' ~ 'Sexo',
    group == 'max_educ_level' ~ 'Máximo nivel educativo',
    group == 'formalidad' ~ 'Formalidad',
    group == 'relab' ~ 'Posición ocupacional',
    group == 'size_firm' ~ 'Cantidad de trabajadores de la empresa',
    group == 'oficio' ~ 'Oficio',
    TRUE ~ group))

table_categorical = table_categorical %>% 
  select(' ' = var,
         'N' = n,
         '%' = prop,
         'Promedio' = average,
         'SD'  = sd,
         group)

table_categorical = table_categorical %>% 
  gt(groupname_col = 'group') %>%
  tab_header(title = 'Distribución de las variables categóricas') %>%
  tab_options(latex.use_longtable = FALSE)

##==: 4. Export tables

dir.create("02_outputs/tables", recursive = TRUE, showWarnings = FALSE)

writeLines(as_latex(table_continuous),
           "02_outputs/tables/01_tabla_continuas.tex")

writeLines(as_latex(table_categorical),
           "02_outputs/tables/01_tabla_categoricas.tex")

##==: 5. Reimport tables

latex_table_continuous = readLines('02_outputs/tables/01_tabla_continuas.tex')
latex_table_categoricas = readLines('02_outputs/tables/01_tabla_categoricas.tex')

##==: 6. Add formatting + ENDGROUP

latex_table_continuous = append(
  c("\\centering",
    "\\begingroup",
    "\\fontsize{7pt}{8.5pt}\\selectfont"),
  latex_table_continuous)

latex_table_continuous = append(
  latex_table_continuous,
  c("\\endgroup"))

latex_table_categoricas = append(
  c("\\centering",
    "\\begingroup",
    "\\fontsize{7pt}{8.5pt}\\selectfont"),
  latex_table_categoricas)

latex_table_categoricas = append(
  latex_table_categoricas,
  c("\\endgroup"))

##==: 7. Final export

writeLines(latex_table_continuous,
           "02_outputs/tables/01_tabla_continuas.tex")

writeLines(latex_table_categoricas,
           "02_outputs/tables/01_tabla_categoricas.tex")
