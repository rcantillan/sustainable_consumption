


# --- Script para Analizar Movilidad según Clases Latentes ---

# PASO 0: Cargar bibliotecas necesarias y verificar datos
library(tidyverse)
library(haven)      # Si necesitas volver a cargar datos .sav
library(knitr)      # Para tablas bonitas
library(kableExtra) # Para tablas bonitas
library(ggplot2)
library(scales)     # Para formato de porcentajes en gráficos

# Verifica si el dataframe 'datos' existe y tiene la variable de clase latente
# ASUME que ya tienes 'datos' cargado y contiene 'clase_latente_factor'
# Si no, carga tus datos guardados:
# datos <- read_sav(here::here("datos", "BBDD_con_clases_latentes.sav")) # Ajusta el nombre si es diferente

if (!exists("datos")) {
  stop("El dataframe 'datos' no se encontró. Cárgalo primero.")
}
if (!"clase_latente_factor" %in% names(datos)) {
  stop("La variable 'clase_latente_factor' no se encontró en 'datos'. Asegúrate de haberla creado y guardado.")
}

# Verifica cuántos NAs hay en las variables originales de movilidad
na_o3_1 <- sum(is.na(datos$O3_1))
na_o4_1 <- sum(is.na(datos$O4_1))
cat(paste("\nValores perdidos (NA) en O3_1 (Presente):", na_o3_1, "\n"))
cat(paste("Valores perdidos (NA) en O4_1 (Futuro):", na_o4_1, "\n\n"))

# --- PASO 1: Recodificación de Variables de Movilidad ---

# Definir etiquetas para las nuevas categorías
labels_recod5 <- c(
  "1" = "A pie",
  "2" = "En bicicleta",
  "3" = "Moto/Scooter",
  "4" = "Auto/Camioneta",
  "5" = "Bus/Micro/Colectivo"
)

labels_recod4 <- c(
  "1" = "Activa (Pie/Bici)",
  "2" = "Moto/Scooter",
  "3" = "Auto/Camioneta",
  "4" = "Bus/Micro/Colectivo"
)

datos <- datos %>%
  mutate(
    # Recodificación 5 categorías para O3_1 (Presente)
    movilidad_presente_recod5 = case_when(
      O3_1 == 1 ~ 1, # A pie
      O3_1 == 2 ~ 2, # En bicicleta
      O3_1 %in% c(3, 7) ~ 3, # En moto, En moto eléctrica / Scooter eléctrico
      O3_1 %in% c(4, 5, 8) ~ 4, # En un automóvil, En camioneta, En auto
      O3_1 == 6 ~ 5, # En bus interurbano / micro / colectivo
      TRUE ~ NA_real_ # Maneja NAs u otros valores no esperados
    ),
    # Recodificación 5 categorías para O4_1 (Futuro)
    movilidad_futura_recod5 = case_when(
      O4_1 == 1 ~ 1,
      O4_1 == 2 ~ 2,
      O4_1 %in% c(3, 7) ~ 3,
      O4_1 %in% c(4, 5, 8) ~ 4,
      O4_1 == 6 ~ 5,
      TRUE ~ NA_real_
    ),
    # Opcional: Recodificación 4 categorías para O3_1 (Presente)
    movilidad_presente_recod4 = case_when(
      O3_1 %in% c(1, 2) ~ 1, # A pie, En bicicleta -> Activa
      O3_1 %in% c(3, 7) ~ 2, # Moto/Scooter
      O3_1 %in% c(4, 5, 8) ~ 3, # Auto/Camioneta
      O3_1 == 6 ~ 4, # Bus/Micro/Colectivo
      TRUE ~ NA_real_
    ),
    # Opcional: Recodificación 4 categorías para O4_1 (Futuro)
    movilidad_futura_recod4 = case_when(
      O4_1 %in% c(1, 2) ~ 1,
      O4_1 %in% c(3, 7) ~ 2,
      O4_1 %in% c(4, 5, 8) ~ 3,
      O4_1 == 6 ~ 4,
      TRUE ~ NA_real_
    ),
    # Convertir las nuevas variables a factores con etiquetas
    movilidad_presente_recod5 = factor(movilidad_presente_recod5, levels = 1:5, labels = labels_recod5),
    movilidad_futura_recod5 = factor(movilidad_futura_recod5, levels = 1:5, labels = labels_recod5),
    movilidad_presente_recod4 = factor(movilidad_presente_recod4, levels = 1:4, labels = labels_recod4),
    movilidad_futura_recod4 = factor(movilidad_futura_recod4, levels = 1:4, labels = labels_recod4)
  )

cat("--- Variables de movilidad recodificadas creadas ---\n")

# --- PASO 2: Análisis Movilidad PRESENTE (5 Categorías) ---

cat("\n--- Análisis: Clase Latente vs. Movilidad PRESENTE (5 Categorías) ---\n")

# Tabla cruzada (Frecuencias)
tabla_pres_freq <- table(datos$clase_latente_factor, datos$movilidad_presente_recod5, useNA = "ifany")
cat("\nTabla de Frecuencias (Presente):\n")
print(tabla_pres_freq)

# Tabla cruzada (Porcentajes por fila - por clase latente)
tabla_pres_prop <- prop.table(tabla_pres_freq[, 1:5], margin = 1) # Excluye NA de prop.table si existe
cat("\nTabla de Porcentajes por Clase Latente (Presente):\n")
print(round(tabla_pres_prop * 100, 1))

# Mostrar tabla formateada (opcional, requiere kableExtra)
# kable(tabla_pres_prop * 100, digits = 1, caption = "Movilidad Presente (%) por Clase Latente") %>%
#   kable_styling(bootstrap_options = "striped", full_width = F) %>%
#   add_header_above(c(" " = 1, "Modo de Transporte Presente (5 Cat.)" = 5))

# Prueba Chi-cuadrado
cat("\nPrueba Chi-cuadrado (Presente):\n")
# Usamos la tabla de frecuencias sin la columna NA si existe
tabla_chi_pres <- tabla_pres_freq[, 1:ncol(tabla_pres_freq) - (any(is.na(colnames(tabla_pres_freq))))]
chi_test_pres <- chisq.test(tabla_chi_pres)
print(chi_test_pres)
# Interpreta el p-value: si es < 0.05, hay asociación estadísticamente significativa.
# ¡Cuidado con las advertencias sobre frecuencias esperadas bajas!

# Gráfico (Barras apiladas por porcentaje)
plot_data_pres <- datos %>%
  filter(!is.na(clase_latente_factor) & !is.na(movilidad_presente_recod5)) %>%
  count(clase_latente_factor, movilidad_presente_recod5) %>%
  group_by(clase_latente_factor) %>%
  mutate(prop = n / sum(n))

plot_pres <- ggplot(plot_data_pres, aes(x = clase_latente_factor, y = prop, fill = movilidad_presente_recod5)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 3) + # Añadir etiquetas de %
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2", name = "Modo Presente") + # Elige una paleta de colores
  labs(
    title = "Distribución del Modo de Movilidad Presente por Clase Latente",
    subtitle = "Recodificado en 5 categorías",
    x = "Clase Latente",
    y = "Porcentaje dentro de cada Clase Latente"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Mejorar legibilidad de etiquetas x

print(plot_pres)


# --- PASO 3: Análisis Movilidad FUTURA (5 Categorías) ---

cat("\n\n--- Análisis: Clase Latente vs. Movilidad FUTURA (5 Categorías) ---\n")

# Tabla cruzada (Frecuencias)
tabla_fut_freq <- table(datos$clase_latente_factor, datos$movilidad_futura_recod5, useNA = "ifany")
cat("\nTabla de Frecuencias (Futuro):\n")
print(tabla_fut_freq)

# Tabla cruzada (Porcentajes por fila - por clase latente)
tabla_fut_prop <- prop.table(tabla_fut_freq[, 1:5], margin = 1)
cat("\nTabla de Porcentajes por Clase Latente (Futuro):\n")
print(round(tabla_fut_prop * 100, 1))

# Prueba Chi-cuadrado
cat("\nPrueba Chi-cuadrado (Futuro):\n")
tabla_chi_fut <- tabla_fut_freq[, 1:ncol(tabla_fut_freq) - (any(is.na(colnames(tabla_fut_freq))))]
chi_test_fut <- chisq.test(tabla_chi_fut)
print(chi_test_fut)

# Gráfico (Barras apiladas por porcentaje)
plot_data_fut <- datos %>%
  filter(!is.na(clase_latente_factor) & !is.na(movilidad_futura_recod5)) %>%
  count(clase_latente_factor, movilidad_futura_recod5) %>%
  group_by(clase_latente_factor) %>%
  mutate(prop = n / sum(n))

plot_fut <- ggplot(plot_data_fut, aes(x = clase_latente_factor, y = prop, fill = movilidad_futura_recod5)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2", name = "Modo Futuro") +
  labs(
    title = "Distribución del Modo de Movilidad Futuro Preferido por Clase Latente",
    subtitle = "Recodificado en 5 categorías",
    x = "Clase Latente",
    y = "Porcentaje dentro de cada Clase Latente"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_fut)

# --- PASO 4: (Opcional) Análisis con 4 Categorías ---
# Descomenta y ejecuta este bloque si quieres analizar la versión con 4 categorías

# cat("\n\n--- Análisis: Clase Latente vs. Movilidad PRESENTE (4 Categorías) ---\n")
# tabla_pres4_freq <- table(datos$clase_latente_factor, datos$movilidad_presente_recod4, useNA = "ifany")
# print(tabla_pres4_freq)
# tabla_pres4_prop <- prop.table(tabla_pres4_freq[, 1:4], margin = 1)
# print(round(tabla_pres4_prop * 100, 1))
# tabla_chi_pres4 <- tabla_pres4_freq[, 1:ncol(tabla_pres4_freq) - (any(is.na(colnames(tabla_pres4_freq))))]
# chi_test_pres4 <- chisq.test(tabla_chi_pres4)
# print(chi_test_pres4)
# 
# plot_data_pres4 <- datos %>%
#   filter(!is.na(clase_latente_factor) & !is.na(movilidad_presente_recod4)) %>%
#   count(clase_latente_factor, movilidad_presente_recod4) %>%
#   group_by(clase_latente_factor) %>%
#   mutate(prop = n / sum(n))
# 
# plot_pres4 <- ggplot(plot_data_pres4, aes(x = clase_latente_factor, y = prop, fill = movilidad_presente_recod4)) +
#   geom_bar(stat = "identity", position = "fill") +
#    geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
#             position = position_fill(vjust = 0.5), size = 3) +
#   scale_y_continuous(labels = scales::percent) +
#   scale_fill_brewer(palette = "Set3", name = "Modo Presente (4 Cat)") + 
#   labs(title = "Movilidad Presente (4 Cat.) por Clase Latente", x = "Clase Latente", y = "% dentro de Clase") +
#   theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# print(plot_pres4)

# cat("\n\n--- Análisis: Clase Latente vs. Movilidad FUTURA (4 Categorías) ---\n")
# # Repetir pasos similares para movilidad_futura_recod4

cat("\n--- Fin del Análisis de Movilidad por Clase Latente ---\n")








# --- Script para Modelo Logístico Multinomial (Presente y Futuro) ---
# --- (Controles: Clase Social, Sexo, Edad, Rural/Urbano) ---
# --- (EXCLUYENDO: educacion por separado, posicion_politica) ---

# PASO 0: Cargar Bibliotecas y Verificar Datos

library(tidyverse)
library(nnet)       # Para multinom()
library(haven)      # Si necesitas cargar datos .sav
library(gtsummary)  # Para tablas de regresión elegantes (opcional)
library(effects)    # Para graficar efectos y probabilidades predichas

# Verifica la existencia de los dataframes necesarios
if (!exists("datos")) { stop("El dataframe 'datos' (con M*, O*, clase_latente_factor) no se encontró.") }
if (!exists("datos_modelo")) { stop("El dataframe 'datos_modelo' (usado en poLCA con covariables procesadas) no se encontró.") }

# Verifica columnas clave en 'datos'
required_cols_datos <- c("O3_1", "O4_1", "clase_latente_factor")
if (!all(required_cols_datos %in% names(datos))) {
  stop(paste("Faltan columnas en 'datos':", paste(required_cols_datos[!required_cols_datos %in% names(datos)], collapse=", ")))
}
# Verifica columnas clave en 'datos_modelo' (Quitamos educacion, MANTENEMOS clase_social)
required_cols_datos_modelo <- c("edad", "sexo", "rural_urbano", "clase_social") # <--- Lista actualizada
if (!all(required_cols_datos_modelo %in% names(datos_modelo))) {
  stop(paste("Faltan columnas en 'datos_modelo':", paste(required_cols_datos_modelo[!required_cols_datos_modelo %in% names(datos_modelo)], collapse=", ")))
} else {
  cat("--- Dataframes 'datos' y 'datos_modelo' encontrados con columnas esperadas ---\n")
}


# --- PASO 1: Añadir Clase Latente y Movilidad Recodificada a datos_modelo ---

cat("--- Añadiendo clase latente y variables de movilidad a datos_modelo ---\n")

# ASUNCIÓN IMPORTANTE: Orden de filas coincide entre 'datos' y 'datos_modelo'.
if (nrow(datos) != nrow(datos_modelo)) {
  warning(paste("ADVERTENCIA: Filas difieren entre 'datos' (", nrow(datos),") y 'datos_modelo' (", nrow(datos_modelo),")."))
}

# Definir etiquetas para recodificación de movilidad
labels_recod5 <- c("1" = "A pie", "2" = "En bicicleta", "3" = "Moto/Scooter", "4" = "Auto/Camioneta", "5" = "Bus/Micro/Colectivo")

# Añadir/Verificar columnas asegurando el tipo correcto
datos_modelo <- datos_modelo %>%
  mutate(
    # Añadir clase latente desde 'datos'
    clase_latente_factor = as.factor(datos$clase_latente_factor),
    
    # Recodificar movilidad presente desde 'datos$O3_1'
    movilidad_presente_recod5 = factor(case_when(
      datos$O3_1 == 1 ~ 1, datos$O3_1 == 2 ~ 2, datos$O3_1 %in% c(3, 7) ~ 3, datos$O3_1 %in% c(4, 5, 8) ~ 4, datos$O3_1 == 6 ~ 5, TRUE ~ NA_real_
    ), levels = 1:5, labels = labels_recod5),
    
    # Recodificar movilidad futura desde 'datos$O4_1'
    movilidad_futura_recod5 = factor(case_when(
      datos$O4_1 == 1 ~ 1, datos$O4_1 == 2 ~ 2, datos$O4_1 %in% c(3, 7) ~ 3, datos$O4_1 %in% c(4, 5, 8) ~ 4, datos$O4_1 == 6 ~ 5, TRUE ~ NA_real_
    ), levels = 1:5, labels = labels_recod5),
    
    # --- Se elimina la creación/modificación de 'educacion' aquí ---
    
    # Asegurar que las covariables restantes sean factores
    across(all_of(required_cols_datos_modelo), as.factor)
  )

# Definir la lista FINAL de controles sociodemográficos a usar
sociodem_vars_en_modelo <- required_cols_datos_modelo # Contiene clase_social, edad, sexo, rural_urbano
cat("-> Controles sociodemográficos a usar:", paste(sociodem_vars_en_modelo, collapse=", "), "\n")


cat("-> Variables añadidas/verificadas en datos_modelo.\n")


# --- PASO 2: Definir Referencias ---

# Referencia Variable Independiente Principal: Clase Latente.
if (!"clase_latente_factor" %in% names(datos_modelo)) stop("Columna 'clase_latente_factor' no encontrada.")
ref_clase_latente <- levels(datos_modelo$clase_latente_factor)[1]
datos_modelo$clase_latente_factor <- relevel(datos_modelo$clase_latente_factor, ref = ref_clase_latente)
cat(paste("-> Referencia para Clase Latente establecida en:", ref_clase_latente, "\n"))

# Referencias Variables de Control Sociodemográficas
# *** ¡¡¡ VERIFICA QUE ESTAS REFERENCIAS SEAN LAS CORRECTAS PARA TUS DATOS !!! ***
datos_modelo$sexo <- relevel(datos_modelo$sexo, ref = "1") # Asume 1=Hombre
datos_modelo$edad <- relevel(datos_modelo$edad, ref = "18 a 24 años")
datos_modelo$clase_social <- relevel(datos_modelo$clase_social, ref = "Baja") # Asume ref "Baja"
datos_modelo$rural_urbano <- relevel(datos_modelo$rural_urbano, ref = "Urbano")

# --- Se elimina el relevel para 'educacion' ---

cat("-> Categorías de referencia establecidas.\n")


# --- PASO 3: MODELO PARA MOVILIDAD PRESENTE ---

cat("\n\n=====================================================")
cat("\n--- INICIO: Modelo para Movilidad PRESENTE (5 Cat) ---")
cat("\n=====================================================\n")

# 3.1 Definir VD y Variables Específicas
vd_presente <- "movilidad_presente_recod5"
if (!vd_presente %in% names(datos_modelo)) stop(paste("Columna VD", vd_presente, "no encontrada."))
datos_modelo[[vd_presente]] <- relevel(datos_modelo[[vd_presente]], ref = "Bus/Micro/Colectivo")
vars_modelo_pres <- c(vd_presente, "clase_latente_factor", sociodem_vars_en_modelo) # sociodem_vars_en_modelo ya no tiene educacion

# 3.2 Manejo de NAs
datos_completos_pres <- datos_modelo %>%
  select(all_of(vars_modelo_pres)) %>%
  filter(if_all(everything(), ~ !is.na(.)))

n_completo_pres <- nrow(datos_completos_pres)
n_perdido_pres <- nrow(datos_modelo) - n_completo_pres
cat(paste("\nModelo PRESENTE: Se usarán", n_completo_pres, "obs. completas (", n_perdido_pres, "perdidas por NAs).\n"))
if (n_completo_pres < 50 * length(vars_modelo_pres)) warning("N relativo bajo.")

# 3.3 Ajuste del Modelo
if (n_completo_pres > 0) {
  formula_multi_pres <- as.formula(paste(vd_presente, "~", paste(c("clase_latente_factor", sociodem_vars_en_modelo), collapse = " + ")))
  cat("Ajustando modelo PRESENTE:\n")
  print(formula_multi_pres) # Verifica que 'educacion' no está
  modelo_multi_pres <- multinom(formula_multi_pres, data = datos_completos_pres, Hess = TRUE, trace = FALSE, maxit=300)
  cat("-> Modelo PRESENTE ajustado.\n")
  
  # 3.4 Interpretación y Resultados
  cat("\n--- Resultados Modelo PRESENTE ---\n")
  # RRR
  rrr_pres <- exp(coef(modelo_multi_pres))
  cat("\nRRR (Presente) vs. 'Bus/Micro/Colectivo':\n"); print(round(rrr_pres, 3))
  # P-values
  summary_modelo_pres <- summary(modelo_multi_pres)
  z_scores_pres <- summary_modelo_pres$coefficients / summary_modelo_pres$standard.errors
  p_values_pres <- (1 - pnorm(abs(z_scores_pres))) * 2
  cat("\nP-valores (Presente):\n"); print(round(p_values_pres, 3))
  
  # Tabla gtsummary
  tryCatch({
    tbl_regression(modelo_multi_pres, exponentiate = TRUE) %>% bold_p(t = 0.05) %>% bold_labels() %>%
      modify_header(label ~ "**Variable**") %>% as_gt() %>%
      gt::tab_header(title = "Modelo Multinomial: Movilidad PRESENTE",
                     subtitle = paste0("VD: Modo(Ref=Bus)|Controles(sin Ed.,sin Pos.Pol)|N=", n_completo_pres)) %>% print() # Subtítulo actualizado
  }, error = function(e){ cat("\nError gtsummary (Presente):", e$message, "\n")})
  
  # Gráfico effects
  tryCatch({
    plot(effects::effect("clase_latente_factor", modelo_multi_pres), multiline = TRUE, confint = TRUE,
         main = "Prob. Predicha Modo PRESENTE por Clase Latente") %>% print()
  }, error = function(e) { cat("\nError gráfico effects (Presente):", e$message, "\n")})
  
} else {
  cat("\nNo hay datos completos para ajustar el modelo PRESENTE después de filtrar NAs.\n")
}


# --- PASO 4: MODELO PARA MOVILIDAD FUTURA ---

cat("\n\n===================================================")
cat("\n--- INICIO: Modelo para Movilidad FUTURA (5 Cat) ---")
cat("\n===================================================\n")

# 4.1 Definir VD y Variables Específicas
vd_futura <- "movilidad_futura_recod5"
if (!vd_futura %in% names(datos_modelo)) stop(paste("Columna VD", vd_futura, "no encontrada."))
datos_modelo[[vd_futura]] <- relevel(datos_modelo[[vd_futura]], ref = "Bus/Micro/Colectivo")
vars_modelo_fut <- c(vd_futura, "clase_latente_factor", sociodem_vars_en_modelo) # Usa la misma lista de controles

# 4.2 Manejo de NAs
datos_completos_fut <- datos_modelo %>%
  select(all_of(vars_modelo_fut)) %>%
  filter(if_all(everything(), ~ !is.na(.)))
n_completo_fut <- nrow(datos_completos_fut)
n_perdido_fut <- nrow(datos_modelo) - n_completo_fut
cat(paste("\nModelo FUTURO: Se usarán", n_completo_fut, "obs. completas (", n_perdido_fut, "perdidas por NAs).\n"))
if (n_completo_fut < 50 * length(vars_modelo_fut)) warning("N relativo bajo.")

# 4.3 Ajuste del Modelo
if (n_completo_fut > 0) {
  formula_multi_fut <- as.formula(paste(vd_futura, "~", paste(c("clase_latente_factor", sociodem_vars_en_modelo), collapse = " + ")))
  cat("Ajustando modelo FUTURO:\n")
  print(formula_multi_fut) # Verifica que 'educacion' no está
  modelo_multi_fut <- multinom(formula_multi_fut, data = datos_completos_fut, Hess = TRUE, trace = FALSE, maxit=300)
  cat("-> Modelo FUTURO ajustado.\n")
  
  # 4.4 Interpretación y Resultados
  cat("\n--- Resultados Modelo FUTURO ---\n")
  # RRR
  rrr_fut <- exp(coef(modelo_multi_fut))
  cat("\nRRR (Futuro) vs. 'Bus/Micro/Colectivo':\n"); print(round(rrr_fut, 3))
  # P-values
  summary_modelo_fut <- summary(modelo_multi_fut)
  z_scores_fut <- summary_modelo_fut$coefficients / summary_modelo_fut$standard.errors
  p_values_fut <- (1 - pnorm(abs(z_scores_fut))) * 2
  cat("\nP-valores (Futuro):\n"); print(round(p_values_fut, 3))
  
  # Tabla gtsummary
  tryCatch({
    tbl_regression(modelo_multi_fut, exponentiate = TRUE) %>% bold_p(t = 0.05) %>% bold_labels() %>%
      modify_header(label ~ "**Variable**") %>% as_gt() %>%
      gt::tab_header(title = "Modelo Multinomial: Movilidad FUTURA",
                     subtitle = paste0("VD: Modo(Ref=Bus)|Controles(sin Ed.,sin Pos.Pol)|N=", n_completo_fut)) %>% print() # Subtítulo actualizado
  }, error = function(e){ cat("\nError gtsummary (Futuro):", e$message, "\n")})
 
   # Gráfico effects
  tryCatch({
    plot(effects::effect("clase_latente_factor", modelo_multi_fut), multiline = TRUE, confint = TRUE,
         main = "Prob. Predicha Modo FUTURO por Clase Latente") %>% print()
  }, error = function(e) { cat("\nError gráfico effects (Futuro):", e$message, "\n")})
  
} else {
  cat("\nNo hay datos completos para ajustar el modelo FUTURO después de filtrar NAs.\n")
}


cat("\n\n--- Fin del Análisis Multinomial (sin educacion sep., sin pos. politica) ---\n")

# --- FIN DEL SCRIPT ---


