
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(here)
library(haven) # Necesario para read_sav

# Cargar datos
datos <- read_sav(here("datos", "BBDD Consolidada Maule UCM 2810.sav"))


# revisión ingreso 
# Contar NAs en cada variable de ingreso
na_count <- data.frame(
  Variable = c("M151", "M152", "M153", "M154", "M155", "M156"),
  Cantidad_NA = c(
    sum(is.na(datos$M151)),
    sum(is.na(datos$M152)),
    sum(is.na(datos$M153)),
    sum(is.na(datos$M154)),
    sum(is.na(datos$M155)),
    sum(is.na(datos$M156))
  ),
  Porcentaje_NA = c(
    sum(is.na(datos$M151)) / nrow(datos) * 100,
    sum(is.na(datos$M152)) / nrow(datos) * 100,
    sum(is.na(datos$M153)) / nrow(datos) * 100,
    sum(is.na(datos$M154)) / nrow(datos) * 100,
    sum(is.na(datos$M155)) / nrow(datos) * 100,
    sum(is.na(datos$M156)) / nrow(datos) * 100
  )
)

# Mostrar resultados
print(na_count)

# Calcular cuántas personas tienen al menos una variable de ingreso no-NA
personas_con_ingresos <- sum(!is.na(datos$M151) | 
                               !is.na(datos$M152) | 
                               !is.na(datos$M153) | 
                               !is.na(datos$M154) | 
                               !is.na(datos$M155) | 
                               !is.na(datos$M156))

# Calcular porcentaje de la muestra con al menos un dato de ingreso
porcentaje_con_ingresos <- personas_con_ingresos / nrow(datos) * 100

cat("\nPersonas con al menos un dato de ingreso:", personas_con_ingresos, 
    "(", round(porcentaje_con_ingresos, 1), "%)\n")




# crear indicador  de clase social
# Crear indicador de clase social sin variables de ingreso
datos_clase <- datos %>%
  mutate(
    # 1. Educación (mayor nivel entre el entrevistado y el sostenedor)
    nivel_educ = pmax(M61, M62, na.rm = TRUE),
    
    # 2. Ocupación (mayor categoría ocupacional entre entrevistado y sostenedor)
    nivel_ocup = pmax(M9A, M9B, na.rm = TRUE),
    
    # 3. Índice de posesiones (normalizado)
    indice_posesiones = (
      scale(M13_1) +  # Autos
        scale(M13_2) +  # Baños
        scale(M13_3) +  # Metros cuadrados construidos
        scale(M13_5) +  # Televisores
        scale(M13_6)    # Libros
    ) / 5,  # Promedio para dar igual peso
    
    # 4. Gastos mensuales (normalizado)
    indice_gastos = (
      scale(M16_1) +  # Agua
        scale(M16_2) +  # Electricidad
        scale(M16_3) +  # Combustible
        scale(M16_4)    # Calefacción
    ) / 4  # Promedio para dar igual peso
  )

# Normalizar variables para que todas tengan el mismo peso
datos_clase <- datos_clase %>%
  mutate(
    nivel_educ_norm = scale(nivel_educ),
    nivel_ocup_norm = scale(nivel_ocup)
  )

# Crear índice compuesto con ponderación ajustada
# Educación (40%), Ocupación (35%), Posesiones (20%), Gastos (5%)
datos_clase <- datos_clase %>%
  mutate(
    indice_clase_social = (
      nivel_educ_norm * 0.40 +
        nivel_ocup_norm * 0.35 +
        indice_posesiones * 0.20 +
        indice_gastos * 0.05
    ),
    
    # Usar puntos de corte ajustados basados en la distribución observada
    clase_social_cat = case_when(
      indice_clase_social < -1.0 ~ "Baja",
      indice_clase_social >= -1.0 & indice_clase_social < -0.25 ~ "Media-baja",
      indice_clase_social >= -0.25 & indice_clase_social < 0.5 ~ "Media",
      indice_clase_social >= 0.5 & indice_clase_social < 0.9 ~ "Media-alta",
      indice_clase_social >= 0.9 ~ "Alta",
      TRUE ~ NA_character_
    ),
    
    clase_social_cat = factor(clase_social_cat, 
                              levels = c("Baja", "Media-baja", "Media", 
                                         "Media-alta", "Alta"))
  )

# Verificar distribución
table(datos_clase$clase_social_cat, useNA = "ifany")

# Visualizar el índice completo
hist(datos_clase$indice_clase_social, 
     main = "Distribución del índice de clase social",
     xlab = "Índice", 
     col = "lightblue",
     breaks = 20)

# Visualizar la parte alta de la distribución
hist(datos_clase$indice_clase_social[datos_clase$indice_clase_social > 0.5], 
     main = "Distribución de valores altos del índice",
     xlab = "Índice", 
     col = "lightblue",
     breaks = 20)

# Calcular estadísticas descriptivas por categoría
estadisticas_por_clase <- datos_clase %>%
  group_by(clase_social_cat) %>%
  summarize( 
    n = n(),
    porcentaje = n() / nrow(datos_clase) * 100,
    indice_promedio = mean(indice_clase_social, na.rm = TRUE),
    educacion_promedio = mean(nivel_educ, na.rm = TRUE),
    ocupacion_promedio = mean(nivel_ocup, na.rm = TRUE),
    autos_promedio = mean(M13_1, na.rm = TRUE),
    metros2_promedio = mean(M13_3, na.rm = TRUE)
  )

print(estadisticas_por_clase)

# Guardar la variable de clase social para usar en el modelo de clases latentes
vars_modelo$clase_social <- datos_clase$clase_social_cat

# Opcionalmente, guardar el dataframe
save(datos_clase, file = "datos_con_clase_social.RData")
