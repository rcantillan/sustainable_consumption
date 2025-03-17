#==================================================  
# Project: Sustainablec Consumption
# Description:Análisis de Clases Latentes
#==================================================

# Cargar librerías
library(here)
library(tidyverse)
library(haven)
library(psych)
library(poLCA)
library(dplyr)


# Cargar datos
datos <- read_sav(here("datos", "BBDD Consolidada Maule UCM 2810.sav"))

# Ver la estructura de los datos
str(datos)

# Escala de Modo de Vida Imperial (MVI)
vars_mvi <- c("P7_2", "P7_4", "P7_5", "P4_2", "P4_4")

# Escala de Reflexividad Ambiental Antropocéntrica (RAA)
vars_raa <- c("P7_1", "P7_3", "P4_1", "P4_3")


#  Recodificar P4_1 y P4_3 para mantener coherencia con las escalas
datos <- datos %>%
  mutate(
    urgenciacrisis = 5 - P4_1, 
    actividadeshumanas = 5 - P4_3
  )

#  Variables seleccionadas 
vars_modelo <- dplyr::select(datos,P7_2, P7_4, P7_5, P4_2, P4_4, 
                      P7_1, P7_3, urgenciacrisis, actividadeshumanas)


# Dicotomizar las variables originales directamente a categorías 1 y 2
vars_modelo <- vars_modelo %>%
  mutate(across(everything(), 
                ~ifelse(is.na(.), NA, ifelse(. >= 3, 2, 1))))

#guardar 
save(vars_modelo, file = "objects/vars_modelo.RData")


#________________________Clases latentes
set.seed(1234)

# Clases latentes sin restricciones
f<- cbind(P7_2, P7_4, P7_5, P4_2, P4_4, 
          P7_1, P7_3, urgenciacrisis, 
          actividadeshumanas) ~ NULL


# N de clases 
lca2 <- poLCA(f, vars_modelo, nclass = 2, 
              maxiter = 1000, nrep = 10)
lca3 <- poLCA(f, vars_modelo, nclass = 3,  # modelo con mejor ajuste
              maxiter = 1000, nrep = 10)
lca4 <- poLCA(f, vars_modelo, nclass = 4, 
              maxiter = 1000, nrep = 10)
lca5 <- poLCA(f, vars_modelo, nclass = 5, 
              maxiter = 1000, nrep = 10)
lca6 <- poLCA(f, vars_modelo, nclass = 6, 
              maxiter = 1000, nrep = 10)

#Comparar los modelos
modelos <- list(lca2, lca3, lca4, lca5, lca6)
n_clases <- 2:6

# Crear tabla de comparación
resultados <- data.frame(
  Clases = n_clases,
  BIC = sapply(modelos, function(x) x$bic),
  AIC = sapply(modelos, function(x) x$aic),
  Log_likelihood = sapply(modelos, function(x) x$llik)
)

# Ver resultados
print(resultados)

# probabilidades condicionales
modelo_3 <- lca3


# Extraer probabilidades condicionales
probs_clase <- modelo_3$probs
print(probs_clase)

# Extraer tamaño de las clases
class_sizes <- modelo_3$P

#============================================================
# Categoría 1: Desacuerdo (valores originales 1-2)
# Categoría 2: Acuerdo (valores originales 3-4)

#Clase 1 (37.8% de la muestra): alta adherencia a modo de vida imperial, moderada reflexibidad ambiental 
#Clase 2 (53.5% de la muestra - mayoría): moderada adherencia a modo de vida imperial, baja reflexividad ambiental
#Clase 3 (8.7% de la muestra): moderada adherencia a modo de vida imperial, alta reflexividad ambiental




