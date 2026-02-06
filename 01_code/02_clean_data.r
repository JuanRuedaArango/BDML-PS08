#========================================================#
# PS Prediction Income
# Sany, Andres and Juan
# Fisrt Created:  05/02/2026
# Last update:  05/02/2026
# Script:       Cleaning data from Ignacio repository
#======================================================#

## Cargue de la información previamente descargada ##

base <- readRDS("~/BDML-PS08/01_code/data_output/01_data_scrapping_web_page.rds")
## nos falta agregar el crunch ##

p_load(dplyr, rvest, tidyverse, data.table)


colnames(base)

library(dplyr)
library(tidyverse)

## Seleccionamos la muestra completa, no una muestra.

base_filtrada<-base%>%filter(age>=18 & ocu==1 & !is.na(y_total_m) & !is.na(sex) & !is.na(age))%>%
  select(age,sex,y_total_m,totalHoursWorked,relab,mes,clase,college,fex_c,informal,maxEducLevel,
         pet,p6050)

## salario minimo en 2018 781.242
## linea pobreza bogotá 434630, en hogares

### miramos posible estacionalidad
# al observar los meses no se encuentra estacionalidad

base_filtrada%>%group_by(mes)%>%summarise(mediana=median(y_total_m),promedio=mean(y_total_m),maximo=max(y_total_m),minimo=min(y_total_m))


### miramos los cuantiles
# valores muy altos arriba
quantile(base_filtrada$y_total_m, c(.001,.9,.95,.99,.995,.999), na.rm=TRUE)

# quiénes son, si los quitamos quedaría fuera una proporción similar de hombres y mujeres
base_filtrada%>%filter(y_total_m>=quantile(y_total_m,.99))%>%group_by(sex)%>%summarise(n())
base_filtrada%>%filter(y_total_m>=quantile(y_total_m,.001))%>%group_by(sex)%>%summarise(n())

## base depurada

base_depurada<-base_filtrada%>%filter(y_total_m<=quantile(y_total_m,.99)&y_total_m>=quantile(y_total_m,.001))



hist(base_depurada$y_total_m)

summary(base_depurada)
summary(base)