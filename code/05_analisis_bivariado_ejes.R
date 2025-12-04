# ============================================================================
# Script: 05_analisis_bivariado_ejes.R
# Propósito: Análisis bivariado de clases latentes con tres ejes temáticos:
#   1. Percepción de responsabilidad (diagnóstico)
#   2. Consumo de carne y sustentabilidad
#   3. Consumo digital y sustentabilidad
# ============================================================================

# Cargar paquetes necesarios
library(tidyverse)
library(here)
library(haven)
library(knitr)
library(kableExtra)
library(ggplot2)
library(nnet)
library(broom)

# ============================================================================
# 1. CARGA Y PREPARACIÓN DE DATOS
# ============================================================================

cat("========================================\n")
cat("1. Cargando datos...\n")
cat("========================================\n")

# Intentar cargar datos con clases latentes
if (file.exists(here("objects", "datos_con_clases.RData"))) {
  cat("-> Cargando desde objects/datos_con_clases.RData\n")
  load(here("objects", "datos_con_clases.RData"))
  
  # Verificar que datos y clase_latente_factor existan
  if (!exists("datos") || !"clase_latente_factor" %in% names(datos)) {
    cat("-> ADVERTENCIA: datos_con_clases.RData no contiene la estructura esperada.\n")
    cat("-> Reconstruyendo clase social y clases latentes...\n")
    source_reconstruction <- TRUE
  } else {
    source_reconstruction <- FALSE
  }
} else {
  cat("-> objects/datos_con_clases.RData no existe.\n")
  cat("-> Reconstruyendo clase social y clases latentes...\n")
  source_reconstruction <- TRUE
}

# Reconstruir si es necesario
if (source_reconstruction) {
  # Cargar datos base
  datos <- read_sav(here("datos", "BBDD Consolidada Maule UCM Codificada 20220307.sav"))
  
  # ========================================================================
  # CONSTRUCCIÓN DE CLASE SOCIAL (del script 02_reporte_completo.qmd)
  # ========================================================================
  
  # Diccionario de puntajes de prestigio ocupacional
  puntajes_prestigio <- tribble(
    ~categoria, ~descripcion, ~puntaje_prestigio,
    1, "Trabajadores no calificados en ventas y servicios, peones agropecuarios, forestales, construcción, etc.", 25.0,
    2, "Obreros, operarios y artesanos de artes mecánicas y de otros oficios", 35.0,
    3, "Trabajadores de los servicios y vendedores de comercio y mercados", 37.0,
    4, "Agricultores y trabajadores calificados agropecuarios y pesqueros", 42.0,
    5, "Operadores de instalaciones y máquinas y montadores / conductores de vehículos", 40.0,
    6, "Empleados de oficina públicos y privados", 47.0,
    7, "Técnicos y profesionales de nivel medio (incluye hasta suboficiales FFAA y Carabineros)", 58.0,
    8, "Profesionales, científicos e intelectuales", 72.0,
    9, "Alto ejecutivo (gerente general o gerente de área o sector) de empresa privadas o públicas", 78.0,
    10, "Otros grupos no identificados (incluye rentistas, incapacitados, estudiantes, dueña de casa, etc.)", 40.0
  )
  
  # Función para asignar puntajes de prestigio
  asignar_prestigio <- function(categoria) {
    if (is.na(categoria)) return(NA)
    prestigio <- puntajes_prestigio$puntaje_prestigio[puntajes_prestigio$categoria == categoria]
    if (length(prestigio) == 0) return(NA)
    return(prestigio)
  }
  
  # Aplicar función a variables ocupacionales
  datos <- datos %>%
    mutate(
      prestigio_M9A = sapply(M9A, asignar_prestigio),
      prestigio_M9B = sapply(M9B, asignar_prestigio),
      nivel_ocup_prestigio = pmax(prestigio_M9A, prestigio_M9B, na.rm = TRUE)
    )
  
  # Crear indicador de clase social
  datos_clase <- datos %>%
    mutate(
      nivel_educ = pmax(M61, M62, na.rm = TRUE),
      nivel_ocup = nivel_ocup_prestigio,
      indice_posesiones = (
        scale(M13_1) + scale(M13_2) + scale(M13_3) + 
        scale(M13_5) + scale(M13_6)
      ) / 5,
      indice_gastos = (
        scale(M16_1) + scale(M16_2) + scale(M16_3) + scale(M16_4)
      ) / 4
    )
  
  datos_clase <- datos_clase %>%
    mutate(
      nivel_educ_norm = scale(nivel_educ),
      nivel_ocup_norm = scale(nivel_ocup),
      indice_clase_social = (
        nivel_educ_norm * 0.40 +
        nivel_ocup_norm * 0.35 +
        indice_posesiones * 0.20 +
        indice_gastos * 0.05
      ),
      clase_social_cat = case_when(
        indice_clase_social < quantile(indice_clase_social, 0.25, na.rm = TRUE) ~ "Baja",
        indice_clase_social >= quantile(indice_clase_social, 0.25, na.rm = TRUE) & 
          indice_clase_social < quantile(indice_clase_social, 0.60, na.rm = TRUE) ~ "Media-baja",
        indice_clase_social >= quantile(indice_clase_social, 0.60, na.rm = TRUE) & 
          indice_clase_social < quantile(indice_clase_social, 0.85, na.rm = TRUE) ~ "Media",
        indice_clase_social >= quantile(indice_clase_social, 0.85, na.rm = TRUE) & 
          indice_clase_social < quantile(indice_clase_social, 0.95, na.rm = TRUE) ~ "Media-alta",
        indice_clase_social >= quantile(indice_clase_social, 0.95, na.rm = TRUE) ~ "Alta",
        TRUE ~ NA_character_
      ),
      clase_social_cat = factor(clase_social_cat, 
                               levels = c("Baja", "Media-baja", "Media", "Media-alta", "Alta"))
    )
  
  # ========================================================================
  # CONSTRUCCIÓN DE CLASES LATENTES (del script 03_LCA_covar.R)
  # ========================================================================
  
  library(poLCA)
  
  # Recodificar variables para modelo LCA
  datos <- datos %>%
    mutate(
      urgenciacrisis = 5 - P4_1, 
      actividadeshumanas = 5 - P4_3
    )
  
  # Variables del modelo
  vars_modelo <- datos %>% 
    dplyr::select(P7_2, P7_4, P7_5, P4_2, P4_4, 
                  P7_1, P7_3, urgenciacrisis, actividadeshumanas)
  
  # Dicotomizar variables
  vars_modelo <- vars_modelo %>%
    mutate(across(everything(), 
                  ~ifelse(is.na(.), NA, ifelse(. >= 3, 2, 1))))
  
  # Preparación de datos con covariables
  datos_modelo <- vars_modelo %>%
    mutate(
      clase_social = datos_clase$clase_social_cat,
      edad = case_when(
        datos$M5 >= 18 & datos$M5 <= 24 ~ 1,
        datos$M5 >= 25 & datos$M5 <= 34 ~ 2,
        datos$M5 >= 35 & datos$M5 <= 44 ~ 3,
        datos$M5 >= 45 & datos$M5 <= 54 ~ 4,
        datos$M5 >= 55 & datos$M5 <= 64 ~ 5,
        datos$M5 >= 65 ~ 6,
        TRUE ~ NA_real_
      ),
      edad = factor(edad, 
                    levels = c(1, 2, 3, 4, 5, 6),
                    labels = c("18 a 24 años", "25 a 34 años", "35 a 44 años", 
                               "45 a 54 años", "55 a 64 años", "65 años o más")),
      sexo = factor(datos$M1),
      posicion_politica = case_when(
        datos$M11 >= 1 & datos$M11 <= 4 ~ "Izquierda",
        datos$M11 >= 5 & datos$M11 <= 6 ~ "Centro",
        datos$M11 >= 7 & datos$M11 <= 10 ~ "Derecha",
        TRUE ~ NA_character_
      ),
      posicion_politica = factor(posicion_politica,
                                 levels = c("Izquierda", "Centro", "Derecha")),
      rural_urbano = factor(datos$M12,
                            levels = 1:3,
                            labels = c("Urbano", "Rural", "Mixto"))
    )
  
  # Fórmula con covariables
  f_covariables <- cbind(P7_2, P7_4, P7_5, P4_2, P4_4, 
                         P7_1, P7_3, urgenciacrisis, 
                         actividadeshumanas) ~ sexo + edad + clase_social + posicion_politica + rural_urbano
  
  # Ajustar modelo de 3 clases
  set.seed(123)
  cat("-> Ajustando modelo LCA de 3 clases...\n")
  modelo_cov3 <- poLCA(f_covariables, datos_modelo, 
                       nclass = 3, 
                       maxiter = 1000, 
                       nrep = 5, 
                       verbose = FALSE)
  
  # Asignar clases a individuos
  posterior_probs <- modelo_cov3$posterior
  clase_asignada <- apply(posterior_probs, 1, which.max)
  porcentajes_cov <- round(modelo_cov3$P * 100, 0)
  
  datos <- datos %>%
    mutate(clase_latente = NA)
  
  var_originales <- c("P7_2", "P7_4", "P7_5", "P4_2", "P4_4", 
                      "P7_1", "P7_3", "urgenciacrisis", "actividadeshumanas")
  indices_validos <- which(!is.na(rowSums(as.matrix(datos_modelo[, var_originales]))))
  datos$clase_latente[indices_validos] <- clase_asignada
  
  datos <- datos %>%
    mutate(clase_latente_factor = factor(clase_latente,
                                         levels = 1:3,
                                         labels = paste0("Clase ", 1:3, " (", porcentajes_cov, "%)")))
  
  # Guardar datos con clases
  save(datos, datos_clase, modelo_cov3, file = here("objects", "datos_con_clases.RData"))
  cat("-> Datos con clases latentes guardados en objects/datos_con_clases.RData\n")
}

# Integrar clase social si no está en datos
if (!"clase_social_cat" %in% names(datos) && exists("datos_clase")) {
  datos <- datos %>%
    mutate(clase_social_cat = datos_clase$clase_social_cat)
}

# Agregar variables sociodemográficas si no existen
if (!"sexo" %in% names(datos)) {
  datos <- datos %>%
    mutate(sexo = factor(M1))
}

if (!"edad_cat" %in% names(datos)) {
  datos <- datos %>%
    mutate(
      edad_cat = case_when(
        M5 >= 18 & M5 <= 24 ~ "18-24",
        M5 >= 25 & M5 <= 34 ~ "25-34",
        M5 >= 35 & M5 <= 44 ~ "35-44",
        M5 >= 45 & M5 <= 54 ~ "45-54",
        M5 >= 55 & M5 <= 64 ~ "55-64",
        M5 >= 65 ~ "65+",
        TRUE ~ NA_character_
      ),
      edad_cat = factor(edad_cat, levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
    )
}

if (!"posicion_politica" %in% names(datos)) {
  datos <- datos %>%
    mutate(
      posicion_politica = case_when(
        M11 >= 1 & M11 <= 4 ~ "Izquierda",
        M11 >= 5 & M11 <= 6 ~ "Centro",
        M11 >= 7 & M11 <= 10 ~ "Derecha",
        TRUE ~ NA_character_
      ),
      posicion_politica = factor(posicion_politica, levels = c("Izquierda", "Centro", "Derecha"))
    )
}

if (!"rural_urbano" %in% names(datos)) {
  datos <- datos %>%
    mutate(
      rural_urbano = factor(M12, levels = 1:3, labels = c("Urbano", "Rural", "Mixto"))
    )
}

cat("-> Datos preparados. N total =", nrow(datos), "\n")
cat("-> Distribución de clases latentes:\n")
print(table(datos$clase_latente_factor, useNA = "ifany"))

# ============================================================================
# 2. RECODIFICACIÓN EJE 1: PERCEPCIÓN DE RESPONSABILIDAD
# ============================================================================

cat("\n========================================\n")
cat("2. Recodificando Eje 1: Percepción de responsabilidad\n")
cat("========================================\n")

datos <- datos %>%
  mutate(
    # P5_RE: ¿Quién causa los problemas ambientales?
    # Recodificar en macro-categorías: Hogares/Ciudadanía, Empresas, Estado, Todos/Otras
    P5_RE_cat = case_when(
      P5_RE %in% c(1, 2) ~ "Hogares/Ciudadanía",
      P5_RE == 3 ~ "Empresas",
      P5_RE == 4 ~ "Estado",
      P5_RE %in% c(5, 6, 7, 8) ~ "Todos/Otras",
      TRUE ~ NA_character_
    ),
    P5_RE_cat = factor(P5_RE_cat, 
                       levels = c("Hogares/Ciudadanía", "Empresas", "Estado", "Todos/Otras")),
    
    # P6_RE: ¿Quién debe solucionar?
    P6_RE_cat = case_when(
      P6_RE %in% c(1, 2) ~ "Hogares/Ciudadanía",
      P6_RE == 3 ~ "Empresas",
      P6_RE == 4 ~ "Estado",
      P6_RE %in% c(5, 6, 7, 8) ~ "Todos/Otras",
      TRUE ~ NA_character_
    ),
    P6_RE_cat = factor(P6_RE_cat, 
                       levels = c("Hogares/Ciudadanía", "Empresas", "Estado", "Todos/Otras")),
    
    # P8: Responsabilidad de los hogares (1-4) → Dicotomizar Baja (1-2) vs Alta (3-4)
    P8_dicho = case_when(
      P8 %in% c(1, 2) ~ "Baja",
      P8 %in% c(3, 4) ~ "Alta",
      TRUE ~ NA_character_
    ),
    P8_dicho = factor(P8_dicho, levels = c("Baja", "Alta"))
  )

cat("-> Distribución P5_RE_cat (Quién causa):\n")
print(table(datos$P5_RE_cat, useNA = "ifany"))
cat("-> Distribución P6_RE_cat (Quién debe solucionar):\n")
print(table(datos$P6_RE_cat, useNA = "ifany"))
cat("-> Distribución P8_dicho (Responsabilidad hogares):\n")
print(table(datos$P8_dicho, useNA = "ifany"))

# ============================================================================
# 3. RECODIFICACIÓN EJE 2: CONSUMO DE CARNE Y SUSTENTABILIDAD
# ============================================================================

cat("\n========================================\n")
cat("3. Recodificando Eje 2: Consumo de carne y sustentabilidad\n")
cat("========================================\n")

datos <- datos %>%
  mutate(
    # P3_10: Preocupación por impacto de ganadería → Dicotomizar
    P3_10_dicho = case_when(
      P3_10 %in% c(1, 2) ~ "Baja preocupación",
      P3_10 %in% c(3, 4) ~ "Alta preocupación",
      TRUE ~ NA_character_
    ),
    P3_10_dicho = factor(P3_10_dicho, levels = c("Baja preocupación", "Alta preocupación")),
    
    # P9_6: Percepción impacto alimentación → Dicotomizar
    P9_6_dicho = case_when(
      P9_6 %in% c(1, 2) ~ "Bajo impacto",
      P9_6 %in% c(3, 4) ~ "Alto impacto",
      TRUE ~ NA_character_
    ),
    P9_6_dicho = factor(P9_6_dicho, levels = c("Bajo impacto", "Alto impacto")),
    
    # A5_1: Proyección futura consumo carne
    A5_1_cat = case_when(
      A5_1 == 1 ~ "Más",
      A5_1 == 2 ~ "Lo mismo",
      A5_1 == 3 ~ "Menos",
      TRUE ~ NA_character_
    ),
    A5_1_cat = factor(A5_1_cat, levels = c("Más", "Lo mismo", "Menos")),
    
    # A6A: Disposición a cambiar conducta → Ordinal (0=sigue igual, 1=disminuiría, 2=dejaría)
    A6A_ord = case_when(
      A6A == 1 ~ 0,  # Seguiría comiendo igual
      A6A == 2 ~ 1,  # Disminuiría consumo
      A6A == 3 ~ 2,  # Dejaría de comer carne
      TRUE ~ NA_real_
    ),
    
    # A6B1_1, A6B2_2: Disposición a compensar pagando más
    # Indicador ordinal (0=no, 1=doble, 2=triple)
    A6B_compensar = case_when(
      A6B1_1 == 1 & A6B2_2 == 1 ~ 0,  # No pagaría más (rechaza ambas)
      A6B1_1 == 2 & A6B2_2 == 1 ~ 1,  # Pagaría doble (acepta A6B1_1 pero rechaza A6B2_2)
      A6B1_1 == 2 & A6B2_2 == 2 ~ 2,  # Pagaría triple (acepta ambas)
      TRUE ~ NA_real_
    )
  )

cat("-> Distribución P3_10_dicho (Preocupación ganadería):\n")
print(table(datos$P3_10_dicho, useNA = "ifany"))
cat("-> Distribución P9_6_dicho (Percepción impacto alimentación):\n")
print(table(datos$P9_6_dicho, useNA = "ifany"))
cat("-> Distribución A5_1_cat (Proyección consumo carne):\n")
print(table(datos$A5_1_cat, useNA = "ifany"))
cat("-> Distribución A6A_ord (Disposición a cambiar):\n")
print(table(datos$A6A_ord, useNA = "ifany"))
cat("-> Distribución A6B_compensar (Disposición a compensar):\n")
print(table(datos$A6B_compensar, useNA = "ifany"))

# ============================================================================
# 4. RECODIFICACIÓN EJE 3: CONSUMO DIGITAL Y SUSTENTABILIDAD
# ============================================================================

cat("\n========================================\n")
cat("4. Recodificando Eje 3: Consumo digital y sustentabilidad\n")
cat("========================================\n")

datos <- datos %>%
  mutate(
    # P9_5: Percepción impacto uso Internet → Dicotomizar
    P9_5_dicho = case_when(
      P9_5 %in% c(1, 2) ~ "Bajo impacto",
      P9_5 %in% c(3, 4) ~ "Alto impacto",
      TRUE ~ NA_character_
    ),
    P9_5_dicho = factor(P9_5_dicho, levels = c("Bajo impacto", "Alto impacto")),
    
    # A7A: Disposición a cambiar uso Internet → Ordinal
    A7A_ord = case_when(
      A7A == 1 ~ 0,  # Seguiría usando igual
      A7A == 2 ~ 1,  # Disminuiría uso
      A7A == 3 ~ 2,  # Dejaría de usar tanto
      TRUE ~ NA_real_
    ),
    
    # A7B1_1, A7B2_1: Disposición a compensar → Indicador ordinal
    A7B_compensar = case_when(
      A7B1_1 == 1 & A7B2_1 == 1 ~ 0,  # No pagaría más
      A7B1_1 == 2 & A7B2_1 == 1 ~ 1,  # Pagaría doble
      A7B1_1 == 2 & A7B2_1 == 2 ~ 2,  # Pagaría triple
      TRUE ~ NA_real_
    )
  )

cat("-> Distribución P9_5_dicho (Percepción impacto Internet):\n")
print(table(datos$P9_5_dicho, useNA = "ifany"))
cat("-> Distribución A7A_ord (Disposición a cambiar Internet):\n")
print(table(datos$A7A_ord, useNA = "ifany"))
cat("-> Distribución A7B_compensar (Disposición a compensar):\n")
print(table(datos$A7B_compensar, useNA = "ifany"))

# ============================================================================
# 5. ANÁLISIS BIVARIADO - EJE 1: PERCEPCIÓN DE RESPONSABILIDAD
# ============================================================================

cat("\n========================================\n")
cat("5. Análisis Bivariado - Eje 1: Percepción de responsabilidad\n")
cat("========================================\n")

# Función para realizar análisis bivariado con Chi-cuadrado
analisis_bivariado <- function(datos, var_independiente, var_dependiente, nombre_eje) {
  cat(paste("\n--- Análisis:", nombre_eje, "---\n"))
  
  # Crear tabla de contingencia
  tabla <- table(datos[[var_independiente]], datos[[var_dependiente]], useNA = "ifany")
  cat("Tabla de contingencia:\n")
  print(tabla)
  
  # Tabla de proporciones por fila (clase latente)
  tabla_prop <- prop.table(tabla, margin = 1) * 100
  cat("\nProporciones por clase latente (%):\n")
  print(round(tabla_prop, 1))
  
  # Test Chi-cuadrado
  if (sum(!is.na(datos[[var_independiente]]) & !is.na(datos[[var_dependiente]])) > 0) {
    chi_test <- tryCatch({
      chisq.test(tabla)
    }, error = function(e) {
      cat("Error en Chi-cuadrado:", e$message, "\n")
      NULL
    })
    
    if (!is.null(chi_test)) {
      cat(paste("\nChi-cuadrado: X2 =", round(chi_test$statistic, 2), 
                ", df =", chi_test$parameter, 
                ", p-value =", format.pval(chi_test$p.value, digits = 3), "\n"))
    }
  }
  
  return(list(tabla = tabla, tabla_prop = tabla_prop))
}

# Análisis para cada variable del Eje 1
if ("P5_RE_cat" %in% names(datos)) {
  res_p5 <- analisis_bivariado(datos, "clase_latente_factor", "P5_RE_cat", 
                                "Clase latente x Quién causa problemas ambientales")
}

if ("P6_RE_cat" %in% names(datos)) {
  res_p6 <- analisis_bivariado(datos, "clase_latente_factor", "P6_RE_cat", 
                                "Clase latente x Quién debe solucionar")
}

if ("P8_dicho" %in% names(datos)) {
  res_p8 <- analisis_bivariado(datos, "clase_latente_factor", "P8_dicho", 
                                "Clase latente x Responsabilidad hogares")
}

# ============================================================================
# 6. ANÁLISIS BIVARIADO - EJE 2: CONSUMO DE CARNE
# ============================================================================

cat("\n========================================\n")
cat("6. Análisis Bivariado - Eje 2: Consumo de carne\n")
cat("========================================\n")

# Análisis para cada variable del Eje 2
if ("P3_10_dicho" %in% names(datos)) {
  res_p3_10 <- analisis_bivariado(datos, "clase_latente_factor", "P3_10_dicho", 
                                   "Clase latente x Preocupación ganadería")
}

if ("P9_6_dicho" %in% names(datos)) {
  res_p9_6 <- analisis_bivariado(datos, "clase_latente_factor", "P9_6_dicho", 
                                  "Clase latente x Percepción impacto alimentación")
}

if ("A5_1_cat" %in% names(datos)) {
  res_a5_1 <- analisis_bivariado(datos, "clase_latente_factor", "A5_1_cat", 
                                  "Clase latente x Proyección consumo carne")
}

if ("A6A_ord" %in% names(datos)) {
  # Para variable ordinal, convertir a factor temporalmente
  datos_temp <- datos %>%
    mutate(A6A_ord_fac = factor(A6A_ord, 
                                 levels = c(0, 1, 2),
                                 labels = c("Sigue igual", "Disminuiría", "Dejaría")))
  res_a6a <- analisis_bivariado(datos_temp, "clase_latente_factor", "A6A_ord_fac", 
                                 "Clase latente x Disposición cambiar conducta carne")
}

if ("A6B_compensar" %in% names(datos)) {
  datos_temp <- datos %>%
    mutate(A6B_compensar_fac = factor(A6B_compensar, 
                                       levels = c(0, 1, 2),
                                       labels = c("No paga", "Doble", "Triple")))
  res_a6b <- analisis_bivariado(datos_temp, "clase_latente_factor", "A6B_compensar_fac", 
                                 "Clase latente x Disposición compensar carne")
}

# ============================================================================
# 7. ANÁLISIS BIVARIADO - EJE 3: CONSUMO DIGITAL
# ============================================================================

cat("\n========================================\n")
cat("7. Análisis Bivariado - Eje 3: Consumo digital\n")
cat("========================================\n")

# Análisis para cada variable del Eje 3
if ("P9_5_dicho" %in% names(datos)) {
  res_p9_5 <- analisis_bivariado(datos, "clase_latente_factor", "P9_5_dicho", 
                                  "Clase latente x Percepción impacto Internet")
}

if ("A7A_ord" %in% names(datos)) {
  datos_temp <- datos %>%
    mutate(A7A_ord_fac = factor(A7A_ord, 
                                 levels = c(0, 1, 2),
                                 labels = c("Sigue igual", "Disminuiría", "Dejaría")))
  res_a7a <- analisis_bivariado(datos_temp, "clase_latente_factor", "A7A_ord_fac", 
                                 "Clase latente x Disposición cambiar Internet")
}

if ("A7B_compensar" %in% names(datos)) {
  datos_temp <- datos %>%
    mutate(A7B_compensar_fac = factor(A7B_compensar, 
                                       levels = c(0, 1, 2),
                                       labels = c("No paga", "Doble", "Triple")))
  res_a7b <- analisis_bivariado(datos_temp, "clase_latente_factor", "A7B_compensar_fac", 
                                 "Clase latente x Disposición compensar Internet")
}

# ============================================================================
# 8. GRÁFICOS DE BARRAS APILADAS
# ============================================================================

cat("\n========================================\n")
cat("8. Generando gráficos de barras apiladas\n")
cat("========================================\n")

# Función para crear gráfico de barras apiladas
crear_grafico_barras <- function(datos, var_dep, titulo, archivo = NULL) {
  # Preparar datos
  datos_plot <- datos %>%
    filter(!is.na(clase_latente_factor) & !is.na(!!sym(var_dep))) %>%
    count(clase_latente_factor, !!sym(var_dep)) %>%
    group_by(clase_latente_factor) %>%
    mutate(prop = n / sum(n) * 100) %>%
    ungroup()
  
  # Crear gráfico
  p <- ggplot(datos_plot, aes(x = clase_latente_factor, y = prop, 
                               fill = !!sym(var_dep))) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(prop, 1), "%")), 
              position = position_stack(vjust = 0.5),
              color = "white", size = 3.5, fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = titulo,
         x = "Clase Latente",
         y = "Porcentaje (%)",
         fill = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # Guardar si se proporciona archivo
  if (!is.null(archivo)) {
    ggsave(here("plots", archivo), p, width = 10, height = 6, dpi = 300)
    cat(paste("-> Gráfico guardado:", archivo, "\n"))
  }
  
  return(p)
}

# Crear directorio de plots si no existe
if (!dir.exists(here("plots"))) {
  dir.create(here("plots"), recursive = TRUE)
}

# Gráficos para Eje 1
if ("P5_RE_cat" %in% names(datos)) {
  p1 <- crear_grafico_barras(datos, "P5_RE_cat", 
                              "Clase Latente x Quién Causa Problemas Ambientales",
                              "eje1_quien_causa.png")
  print(p1)
}

if ("P6_RE_cat" %in% names(datos)) {
  p2 <- crear_grafico_barras(datos, "P6_RE_cat", 
                              "Clase Latente x Quién Debe Solucionar",
                              "eje1_quien_soluciona.png")
  print(p2)
}

if ("P8_dicho" %in% names(datos)) {
  p3 <- crear_grafico_barras(datos, "P8_dicho", 
                              "Clase Latente x Responsabilidad de los Hogares",
                              "eje1_responsabilidad_hogares.png")
  print(p3)
}

# Gráficos para Eje 2
if ("P3_10_dicho" %in% names(datos)) {
  p4 <- crear_grafico_barras(datos, "P3_10_dicho", 
                              "Clase Latente x Preocupación por Ganadería",
                              "eje2_preocupacion_ganaderia.png")
  print(p4)
}

if ("A5_1_cat" %in% names(datos)) {
  p5 <- crear_grafico_barras(datos, "A5_1_cat", 
                              "Clase Latente x Proyección Consumo de Carne",
                              "eje2_proyeccion_carne.png")
  print(p5)
}

# Gráficos para Eje 3
if ("P9_5_dicho" %in% names(datos)) {
  p6 <- crear_grafico_barras(datos, "P9_5_dicho", 
                              "Clase Latente x Percepción Impacto Internet",
                              "eje3_percepcion_internet.png")
  print(p6)
}

# ============================================================================
# 9. COMPARACIÓN CARNE VS DIGITAL
# ============================================================================

cat("\n========================================\n")
cat("9. Comparación Carne vs Digital\n")
cat("========================================\n")

# Crear indicadores comparativos
datos_comp <- datos %>%
  mutate(
    # Percepción de impacto
    percepcion_carne = case_when(
      P9_6_dicho == "Alto impacto" ~ 1,
      P9_6_dicho == "Bajo impacto" ~ 0,
      TRUE ~ NA_real_
    ),
    percepcion_digital = case_when(
      P9_5_dicho == "Alto impacto" ~ 1,
      P9_5_dicho == "Bajo impacto" ~ 0,
      TRUE ~ NA_real_
    ),
    # Disposición a cambiar
    disposicion_carne = A6A_ord,
    disposicion_digital = A7A_ord
  )

# Comparar percepciones por clase
if (all(c("percepcion_carne", "percepcion_digital") %in% names(datos_comp))) {
  cat("\n--- Percepción de impacto promedio por clase ---\n")
  percepciones <- datos_comp %>%
    group_by(clase_latente_factor) %>%
    summarise(
      Percepcion_Carne = mean(percepcion_carne, na.rm = TRUE) * 100,
      Percepcion_Digital = mean(percepcion_digital, na.rm = TRUE) * 100,
      N = n()
    )
  print(percepciones)
  
  # Gráfico comparativo
  percepciones_long <- percepciones %>%
    pivot_longer(cols = c(Percepcion_Carne, Percepcion_Digital),
                 names_to = "Tipo", values_to = "Porcentaje") %>%
    mutate(Tipo = recode(Tipo, 
                         "Percepcion_Carne" = "Alimentación",
                         "Percepcion_Digital" = "Internet"))
  
  p_comp1 <- ggplot(percepciones_long, aes(x = clase_latente_factor, y = Porcentaje, 
                                            fill = Tipo)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Alimentación" = "#E74C3C", "Internet" = "#3498DB")) +
    labs(title = "Comparación: Percepción de Alto Impacto Ambiental",
         subtitle = "Alimentación vs Internet por Clase Latente",
         x = "Clase Latente",
         y = "% con percepción de alto impacto",
         fill = "Ámbito") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  print(p_comp1)
  ggsave(here("plots", "comparacion_percepcion_carne_digital.png"), 
         p_comp1, width = 10, height = 6, dpi = 300)
}

# Comparar disposiciones a cambiar
if (all(c("disposicion_carne", "disposicion_digital") %in% names(datos_comp))) {
  cat("\n--- Disposición a cambiar promedio por clase ---\n")
  disposiciones <- datos_comp %>%
    group_by(clase_latente_factor) %>%
    summarise(
      Disposicion_Carne = mean(disposicion_carne, na.rm = TRUE),
      Disposicion_Digital = mean(disposicion_digital, na.rm = TRUE),
      N = n()
    )
  print(disposiciones)
  
  # Gráfico comparativo
  disposiciones_long <- disposiciones %>%
    pivot_longer(cols = c(Disposicion_Carne, Disposicion_Digital),
                 names_to = "Tipo", values_to = "Nivel") %>%
    mutate(Tipo = recode(Tipo, 
                         "Disposicion_Carne" = "Consumo carne",
                         "Disposicion_Digital" = "Uso Internet"))
  
  p_comp2 <- ggplot(disposiciones_long, aes(x = clase_latente_factor, y = Nivel, 
                                             fill = Tipo)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Consumo carne" = "#E74C3C", "Uso Internet" = "#3498DB")) +
    labs(title = "Comparación: Disposición a Cambiar Conducta",
         subtitle = "Consumo de Carne vs Uso de Internet por Clase Latente",
         x = "Clase Latente",
         y = "Nivel promedio (0=sigue igual, 2=dejaría)",
         fill = "Ámbito") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  print(p_comp2)
  ggsave(here("plots", "comparacion_disposicion_carne_digital.png"), 
         p_comp2, width = 10, height = 6, dpi = 300)
}

# ============================================================================
# 10. MODELOS LOGÍSTICOS
# ============================================================================

cat("\n========================================\n")
cat("10. Modelos logísticos con covariables\n")
cat("========================================\n")

# Preparar datos para modelado
datos_modelo_log <- datos %>%
  filter(!is.na(clase_latente_factor)) %>%
  mutate(
    # Relevel clase latente (Clase 1 como referencia)
    clase_latente_factor = relevel(clase_latente_factor, ref = "Clase 1 (60%)")
  )

# Función para ajustar y resumir modelo logístico
ajustar_modelo_logistico <- function(datos, var_dep, nombre_modelo) {
  cat(paste("\n--- Modelo:", nombre_modelo, "---\n"))
  
  # Verificar que la variable dependiente existe y tiene variación
  if (!var_dep %in% names(datos)) {
    cat(paste("Variable", var_dep, "no encontrada en los datos.\n"))
    return(NULL)
  }
  
  # Filtrar datos completos
  datos_mod <- datos %>%
    filter(!is.na(!!sym(var_dep)) & 
           !is.na(clase_latente_factor) & 
           !is.na(sexo) & 
           !is.na(edad_cat) & 
           !is.na(clase_social_cat) & 
           !is.na(posicion_politica) & 
           !is.na(rural_urbano))
  
  if (nrow(datos_mod) < 50) {
    cat(paste("Datos insuficientes para el modelo (N =", nrow(datos_mod), ")\n"))
    return(NULL)
  }
  
  # Verificar si la variable es dicotómica o categórica
  niveles <- length(unique(datos_mod[[var_dep]]))
  
  if (niveles == 2) {
    # Modelo logístico binario
    formula_str <- paste(var_dep, "~ clase_latente_factor + sexo + edad_cat + clase_social_cat + posicion_politica + rural_urbano")
    
    modelo <- tryCatch({
      glm(as.formula(formula_str), data = datos_mod, family = binomial())
    }, error = function(e) {
      cat("Error al ajustar modelo:", e$message, "\n")
      return(NULL)
    })
    
  } else if (niveles > 2) {
    # Modelo multinomial
    formula_str <- paste(var_dep, "~ clase_latente_factor + sexo + edad_cat + clase_social_cat + posicion_politica + rural_urbano")
    
    modelo <- tryCatch({
      multinom(as.formula(formula_str), data = datos_mod, trace = FALSE)
    }, error = function(e) {
      cat("Error al ajustar modelo:", e$message, "\n")
      return(NULL)
    })
  } else {
    cat("Variable dependiente no tiene suficiente variación.\n")
    return(NULL)
  }
  
  if (!is.null(modelo)) {
    cat("\nResumen del modelo:\n")
    print(summary(modelo))
    
    # Coeficientes en formato tidy
    coef_tidy <- tidy(modelo, conf.int = TRUE, exponentiate = TRUE)
    cat("\nCoeficientes (OR con IC 95%):\n")
    print(coef_tidy %>% 
            filter(grepl("clase_latente_factor", term)) %>%
            select(term, estimate, conf.low, conf.high, p.value))
  }
  
  return(modelo)
}

# Modelos para variables clave de cada eje

# Eje 1: Responsabilidad
if ("P8_dicho" %in% names(datos_modelo_log)) {
  modelo_p8 <- ajustar_modelo_logistico(datos_modelo_log, "P8_dicho", 
                                         "Responsabilidad de hogares (P8)")
}

# Eje 2: Consumo de carne
if ("P3_10_dicho" %in% names(datos_modelo_log)) {
  modelo_p3_10 <- ajustar_modelo_logistico(datos_modelo_log, "P3_10_dicho", 
                                            "Preocupación por ganadería (P3_10)")
}

if ("A5_1_cat" %in% names(datos_modelo_log)) {
  modelo_a5_1 <- ajustar_modelo_logistico(datos_modelo_log, "A5_1_cat", 
                                           "Proyección consumo carne (A5_1)")
}

# Eje 3: Consumo digital
if ("P9_5_dicho" %in% names(datos_modelo_log)) {
  modelo_p9_5 <- ajustar_modelo_logistico(datos_modelo_log, "P9_5_dicho", 
                                           "Percepción impacto Internet (P9_5)")
}

# ============================================================================
# 11. GUARDAR RESULTADOS
# ============================================================================

cat("\n========================================\n")
cat("11. Guardando resultados\n")
cat("========================================\n")

# Guardar datos con todas las recodificaciones
save(datos, file = here("objects", "datos_con_ejes.RData"))
cat("-> Datos guardados en objects/datos_con_ejes.RData\n")

# Guardar modelos
if (exists("modelo_p8") || exists("modelo_p3_10") || exists("modelo_a5_1") || exists("modelo_p9_5")) {
  modelos_lista <- list(
    modelo_p8 = if(exists("modelo_p8")) modelo_p8 else NULL,
    modelo_p3_10 = if(exists("modelo_p3_10")) modelo_p3_10 else NULL,
    modelo_a5_1 = if(exists("modelo_a5_1")) modelo_a5_1 else NULL,
    modelo_p9_5 = if(exists("modelo_p9_5")) modelo_p9_5 else NULL
  )
  save(modelos_lista, file = here("objects", "modelos_ejes.RData"))
  cat("-> Modelos guardados en objects/modelos_ejes.RData\n")
}

cat("\n========================================\n")
cat("ANÁLISIS COMPLETADO\n")
cat("========================================\n")
cat("Los resultados se han guardado en:\n")
cat("  - objects/datos_con_ejes.RData\n")
cat("  - objects/modelos_ejes.RData\n")
cat("  - plots/*.png (gráficos)\n")
cat("\n")
