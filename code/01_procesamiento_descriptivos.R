#==================================================  
# Project: Sustainablec Consumption
# Description:Descriptivos
#==================================================

# Cargar librerías
library(here)
library(tidyverse)
library(haven)
library(psych)
library(dplyr)

# Cargar datos
datos <- read_sav(here("datos", "BBDD Consolidada Maule UCM 2810.sav"))

covar <- dplyr::select(datos, M1,M2,M5,M61)



# Renombrar 
covar <- covar %>%
  rename(
    region = M1,  # región 1=maule, 2=rm
    sexo = M2,) # sexo 1=hombre, 2=mujer


# Recodificar 
covar <- covar %>%
  mutate(
    edad = case_when(
      M5 >= 18 & M5 <= 24 ~ 1,
      M5 >= 25 & M5 <= 34 ~ 2,
      M5 >= 35 & M5 <= 44 ~ 3,
      M5 >= 45 & M5 <= 54 ~ 4,
      M5 >= 55 & M5 <= 64 ~ 5,
      M5 >= 65 ~ 6,
      TRUE ~ NA_real_
    ),
    edad = factor(edad, 
                  levels = c(1, 2, 3, 4, 5, 6),
                  labels = c("18 a 24 años", "25 a 34 años", "35 a 44 años", 
                             "45 a 54 años", "55 a 64 años", "65 años o más")))


# Recodificar educ
covar <- covar %>%
  mutate(
    educacion = case_when(
      M61 %in% c(1, 2, 3) ~ 1,   #sin educacion hasta básica completa
      M61 %in% c(4, 5) ~ 2,   #educación media comp o incom
      M61 %in% c(6, 7) ~ 3,   #educación técnica comp o incom
      M61 %in% c(8, 9) ~ 4,  #educación universitaria comp o incom
      TRUE ~ NA_real_
    ),
    educacion = factor(educacion, 
                       levels = 1:4,
                       labels = c("Educación básica o menos", 
                                  "Educación media", 
                                  "Educación técnica", 
                                  "Educación universitaria")))


covar <- dplyr::select(covar, sexo, edad, educacion, region)

save(covar, file = "objects/covar.RData")
  

load("objects/vars_modelo.RData")

# Combinar variables y covariables en un solo dataframe
datos_completos <- cbind(vars_modelo, covar)


fc <- cbind(P7_2, P7_4, P7_5, P4_2, P4_4, 
           P7_1, P7_3, urgenciacrisis, 
           actividadeshumanas) ~ sexo + edad + educacion + region


# N de clases 
lca2 <- poLCA(fc, vars_modelo, nclass = 2, 
              maxiter = 1000, nrep = 10)
lca3 <- poLCA(fc, vars_modelo, nclass = 3,  # modelo con mejor ajuste
              maxiter = 1000, nrep = 10)
lca4 <- poLCA(fc, vars_modelo, nclass = 4, 
              maxiter = 1000, nrep = 10)
lca5 <- poLCA(fc, vars_modelo, nclass = 5, 
              maxiter = 1000, nrep = 10)
lca6 <- poLCA(fc, vars_modelo, nclass = 6, 
              maxiter = 1000, nrep = 10)

