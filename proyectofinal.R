install.packages("gtrendsR")  
library(gtrendsR)

wdir <- "C:/Users/emil_/Desktop/ITAM_Maestrtía/LuisEnrique"
setwd(wdir)

library(tidyverse)

setwd("C:/Users/emil_/Desktop/ITAM_Maestría/LuisEnrique")
#¿El cambio de sexenio en 2018 modificó el patrón temporal de desapariciones en la CDMX, y cómo se relacionan las 
#desapariciones con indicadores de violencia entre 2018 y 2024?

#variable respuesta : Yt=numero total de desapariciones en el mes t

data_final <- read.csv("data_final.csv")

datos_mes <- data_final %>%
  group_by(fecha, anio, mes, post_2018, periodo_gobierno) %>%
  summarise(
    desapariciones = sum(n_desapariciones, na.rm = TRUE),
    hombres = sum(n_hombres, na.rm = TRUE),
    mujeres = sum(n_mujeres, na.rm = TRUE),
    homicidios = sum(n_homicidios, na.rm = TRUE),
    robos = sum(n_robos, na.rm = TRUE),
    violencia_familiar = sum(n_violencia_familiar, na.rm = TRUE),
    delitos_libertad = sum(n_delitos_libertad, na.rm = TRUE),
    extorsion = sum(n_extorsion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(fecha) %>%
  mutate(t = row_number())


#EDA
library(tidyverse)
library(lubridate)
library(scales)
library(corrplot)
