#==================================================  
# Project: Sustainable Consumption
# Description:Preparación de datos descriptivos
#==================================================

# Cargar librerías
library(here)
library(tidyverse)
library(haven)
library(psych)


# Cargar datos
datos <- read_sav(here("datos", "BBDD Consolidada Maule UCM 2810.sav"))

# Escala de Modo de Vida Imperial (MVI)
vars_mvi <- c("P7_2", "P7_4", "P7_5", "P4_2", "P4_4")

# Escala de Reflexividad Ambiental Antropocéntrica (RAA)
vars_raa <- c("P7_1", "P7_3", "P4_1", "P4_3")
