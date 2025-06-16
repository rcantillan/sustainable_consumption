library(tidyverse)
library(poLCA)
library(knitr)
library(kableExtra)
library(ggplot2)
library(gridExtra)
library(here)
library(haven) # Necesario para read_sav

# Cargar datos
datos <- read_sav(here("datos", "BBDD Consolidada Maule UCM Codificada 20220307.sav"))

# Escala de Modo de Vida Imperial (MVI)
vars_mvi <- c("P7_2", "P7_4", "P7_5", "P4_2", "P4_4")

# Escala de Reflexividad Ambiental Antropocéntrica (RAA)
vars_raa <- c("P7_1", "P7_3", "P4_1", "P4_3")

# Recodificar P4_1 y P4_3 para mantener coherencia con las escalas
datos <- datos %>%
  mutate(
    urgenciacrisis = 5 - P4_1, 
    actividadeshumanas = 5 - P4_3
  )

# Variables seleccionadas 
vars_modelo <- datos %>% 
  dplyr::select(P7_2, P7_4, P7_5, P4_2, P4_4, 
                P7_1, P7_3, urgenciacrisis, actividadeshumanas)

# Dicotomizar las variables originales directamente a categorías 1 y 2
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
    
    # Sexo (M1)
    sexo = factor(datos$M1),
    
    # Educación (M2)
    educacion = factor(datos$M2),
    
    # Religión (M10)
    religion = factor(datos$M10, 
                      levels = 1:5,
                      labels = c("Católico", "Protestante", "Evangélico", 
                                 "Testigo de Jehová", "Ateo/agnóstico")),
    
    # Posición política (M11)
    posicion_politica = case_when(
      datos$M11 >= 1 & datos$M11 <= 4 ~ "Izquierda",
      datos$M11 >= 5 & datos$M11 <= 6 ~ "Centro",
      datos$M11 >= 7 & datos$M11 <= 10 ~ "Derecha",
      TRUE ~ NA_character_
    ),
    posicion_politica = factor(posicion_politica,
                               levels = c("Izquierda", "Centro", "Derecha")),
    
    # Rural/Urbano (M12)
    rural_urbano = factor(datos$M12,
                          levels = 1:3,
                          labels = c("Urbano", "Rural", "Mixto"))
  )

# Fórmula con covariables (ajustada para las variables disponibles)
f_covariables <- cbind(P7_2, P7_4, P7_5, P4_2, P4_4, 
                       P7_1, P7_3, urgenciacrisis, 
                       actividadeshumanas) ~ sexo + edad + clase_social + posicion_politica + rural_urbano


# Ajustar modelos con diferente número de clases
set.seed(123)  # Para reproducibilidad

# Lista para almacenar los modelos
modelos_cov <- list()
n_clases <- 2:4

# Ajustar modelos
for (i in 1:length(n_clases)) {
  cat("Ajustando modelo con", n_clases[i], "clases...\n")
  modelos_cov[[i]] <- poLCA(f_covariables, datos_modelo, 
                            nclass = n_clases[i], 
                            maxiter = 1000, 
                            nrep = 5, 
                            verbose = TRUE)
}


# Verificar cuáles modelos se ajustaron correctamente
modelos_validos <- !sapply(modelos_cov, is.null)
cat("Modelos ajustados correctamente:", sum(modelos_validos), "de", length(n_clases), "\n")




# Crear tabla de comparación solo con los modelos válidos
if (sum(modelos_validos) > 0) {
  # Obtener índices de modelos válidos
  indices_validos <- which(modelos_validos)
  
  # Crear vectores para almacenar resultados
  clases <- n_clases[indices_validos]
  log_likelihood <- numeric(length(indices_validos))
  bic_values <- numeric(length(indices_validos))
  aic_values <- numeric(length(indices_validos))
  entropia_values <- numeric(length(indices_validos))
  # --- Se eliminan las inicializaciones para g_squared, chi_squared, df_values ---
  # g_squared <- numeric(length(indices_validos))
  # chi_squared <- numeric(length(indices_validos))
  # df_values <- numeric(length(indices_validos))
  
  # Extraer valores de cada modelo válido
  for (i in 1:length(indices_validos)) {
    idx <- indices_validos[i]
    modelo <- modelos_cov[[idx]]
    
    log_likelihood[i] <- modelo$llik
    bic_values[i] <- modelo$bic
    aic_values[i] <- modelo$aic
    entropia_values[i] <- poLCA.entropy(modelo)
    # --- Se eliminan las asignaciones para Gsq, Chisq, df ---
    # g_squared[i] <- modelo$Gsq # No disponible con covariables
    # chi_squared[i] <- modelo$Chisq # No disponible con covariables
    # df_values[i] <- modelo$df # No disponible con covariables
  }
  
  # Crear tabla de resultados (sin Gsq, Chisq, df)
  resultados <- data.frame(
    Clases = clases,
    Log_likelihood = log_likelihood,
    BIC = bic_values,
    AIC = aic_values,
    Entropia = entropia_values
    # --- Se eliminan las columnas G_squared, Chi_squared, df ---
    # G_squared = g_squared,
    # Chi_squared = chi_squared,
    # df = df_values
  )
  
  # Mostrar resultados
  print(resultados)
  
  # Visualizar criterios de información
  if (nrow(resultados) > 1) {
    # Gráfico de BIC y AIC
    p1 <- ggplot(resultados, aes(x = Clases)) +
      geom_line(aes(y = BIC, colour = "BIC"), size = 1) +
      geom_point(aes(y = BIC, colour = "BIC"), size = 3) +
      geom_line(aes(y = AIC, colour = "AIC"), size = 1) +
      geom_point(aes(y = AIC, colour = "AIC"), size = 3) +
      theme_minimal() +
      labs(title = "Criterios de información por número de clases",
           y = "Valor", 
           colour = "Criterio") +
      theme(legend.position = "top")
    
    print(p1)
    
    # Gráfico de entropía
    p2 <- ggplot(resultados, aes(x = Clases, y = Entropia)) +
      geom_line(size = 1, color = "darkgreen") +
      geom_point(size = 3, color = "darkgreen") +
      theme_minimal() +
      labs(title = "Entropía por número de clases",
           y = "Entropía") +
      theme(legend.position = "none")
    
    print(p2)
  }
} else {
  cat("No hay modelos válidos para comparar\n")
}


p1
p2








# Seleccionar el modelo de 3 clases
modelo_cov3 <- modelos_cov[[2]]  # El índice 2 corresponde a 3 clases (ya que empezamos con 2 clases)

# Extraer probabilidades condicionales
probs_clase_cov <- modelo_cov3$probs

# Definir nombres de variables originales y etiquetas para el gráfico
var_originales <- c("P7_2", "P7_4", "P7_5", "P4_2", "P4_4", 
                    "P7_1", "P7_3", "urgenciacrisis", "actividadeshumanas")

etiquetas <- c(
  "P7_2" = "Problemas sociales > ambientales (MVI)",
  "P7_4" = "Crecimiento sin daño ambiental (MVI)",
  "P7_5" = "Problemas ambientales de países desarrollados (MVI)",
  "P4_2" = "No cambiar costumbres, tecnología resolverá (MVI)",
  "P4_4" = "Progreso es vivir como países desarrollados (MVI)",
  "P7_1" = "Resolver problemas cambiando costumbres (RAA)",
  "P7_3" = "Clases altas más responsables de crisis (RAA)",
  "urgenciacrisis" = "Urgencia de la crisis ambiental (RAA)",
  "actividadeshumanas" = "Actividades humanas causan cambios climáticos (RAA)"
)

# Preparar los datos para el gráfico
LMmodelo_cov <- data.frame()

# Para cada variable y cada clase
for (i in 1:length(probs_clase_cov)) {
  var_nombre <- var_originales[i]
  
  for (j in 1:3) { # Para las 3 clases
    temp_df <- data.frame(
      item = var_nombre,
      state = j,
      category = c(0, 1),  # 0 = Desacuerdo, 1 = Acuerdo
      value = c(probs_clase_cov[[i]][j, 1], probs_clase_cov[[i]][j, 2])
    )
    LMmodelo_cov <- rbind(LMmodelo_cov, temp_df)
  }
}

# Obtener porcentajes de las clases
porcentajes_cov <- round(modelo_cov3$P * 100, 0)

# Formatear los datos 
LMmodelo_cov <- LMmodelo_cov %>%
  dplyr::mutate(clase = factor(paste0("Clase ", state, " (", porcentajes_cov[state], "%)"),
                               levels = paste0("Clase ", 1:3, " (", porcentajes_cov[1:3], "%)"))) %>%
  dplyr::mutate(category = factor(ifelse(category == 0, "Desacuerdo", "Acuerdo"),
                                  levels = c("Desacuerdo", "Acuerdo")))

# Gráfico horizontal con barras apiladas
ggplot(LMmodelo_cov, aes(y = factor(item, levels = var_originales, labels = etiquetas[var_originales]), 
                         x = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(. ~ clase) +  # Clases en columnas
  scale_fill_manual(values = c("Desacuerdo" = "#A8D8E8", "Acuerdo" = "#000000")) +
  labs(title = "Probabilidades condicionales por clase latente (con covariables)",
       x = "Probabilidad", y = "", fill = "Respuesta") + 
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 11),
    legend.position = "top",
    panel.grid.major = element_blank()
  )

# Integrar la variable de clase en el dataframe original
# Primero, obtener las probabilidades posteriores de pertenencia a cada clase
posterior_probs <- modelo_cov3$posterior

# Asignar cada observación a la clase con mayor probabilidad posterior
clase_asignada <- apply(posterior_probs, 1, which.max)

# Añadir la clase asignada al dataframe original
datos <- datos %>%
  mutate(clase_latente = NA)

# Solo asignar clases a las observaciones que se usaron en el modelo
# (aquellas sin valores faltantes en las variables del modelo)
indices_validos <- which(!is.na(rowSums(as.matrix(datos_modelo[, var_originales]))))
datos$clase_latente[indices_validos] <- clase_asignada

# Convertir a factor con etiquetas descriptivas
datos <- datos %>%
  mutate(clase_latente_factor = factor(clase_latente,
                                       levels = 1:3,
                                       labels = paste0("Clase ", 1:3, " (", porcentajes_cov, "%)")))

# Guardar el dataframe con la nueva variable
# write_sav(datos, here("datos", "BBDD_con_clases_latentes.sav"))

# Mostrar distribución de clases
table(datos$clase_latente, useNA = "ifany")






# --- BLOQUE COMPLETO CON WORKAROUND PARA $nclass FALTANTE ---

# PASO 0: Cargar bibliotecas (asegúrate de que estén cargadas)
library(tidyverse)
library(poLCA)
library(ggplot2)

# --- PASO 1: SELECCIÓN DEL MODELO FINAL (Forzando Índice 2 basado en errores) ---

# Define el número de clases que CORRESPONDE al índice 2
# (según tu bucle original n_clases <- 2:6, el índice 2 es para 3 clases)
clases_deseadas <- 3
indice_problematico <- 2 # Índice del objeto con $nclass faltante

cat(paste("\n--- Procediendo con el modelo en índice", indice_problematico,
          "que corresponde a", clases_deseadas, "clases (con workaround) ---\n"))

# Verifica que las variables necesarias existen
if (!exists("modelos_cov") || !is.list(modelos_cov)) { stop("Objeto 'modelos_cov' no encontrado o inválido.") }
if (length(modelos_cov) < indice_problematico) { stop(paste("El índice", indice_problematico, "está fuera de los límites de 'modelos_cov'"))}

# Seleccionar el objeto del modelo final usando el índice problemático
modelo_final_seleccionado <- modelos_cov[[indice_problematico]]

# --- PASO 2: VERIFICACIÓN DEL MODELO SELECCIONADO (modificada) ---

cat("--- Verificando el modelo seleccionado... ---\n")

if (is.null(modelo_final_seleccionado)) {
  stop(paste("-> ERROR CRÍTICO: El modelo seleccionado (índice", indice_problematico, ") es NULL."))
}

if (!inherits(modelo_final_seleccionado, "poLCA")) {
  # Esto no debería pasar según str(), pero lo dejamos por seguridad
  stop(paste("-> ERROR CRÍTICO: El objeto seleccionado (índice", indice_problematico, ") no es un objeto de clase 'poLCA' válido."))
}

# -->> INICIO WORKAROUND <<--
# Ya sabemos por str() y el error anterior que $nclass falta.
# Usaremos 'clases_deseadas' en su lugar.
cat(paste("-> ADVERTENCIA: El componente $nclass falta en este objeto poLCA (índice", indice_problematico,
          "). Se usará el valor 'clases_deseadas' =", clases_deseadas, "en su lugar.\n"))

# Verificamos que 'clases_deseadas' sea usable
if (clases_deseadas < 2) {
  stop(paste("-> ERROR CRÍTICO: 'clases_deseadas' está establecido en", clases_deseadas,
             ". Se necesitan al menos 2 clases."))
}
# -->> FIN WORKAROUND <<--

cat(paste("-> Verificación (con workaround): Usaremos nclass =", clases_deseadas, "para continuar.\n"))

# --- PASO 3: EXTRACCIÓN DE DATOS Y CREACIÓN DEL GRÁFICO (modificado) ---

cat("--- Extrayendo coeficientes y preparando datos para el gráfico... ---\n")

# Extraer coeficientes y errores estándar
coeficientes <- modelo_final_seleccionado$coeff
errores_est <- modelo_final_seleccionado$coeff.se

# Verificar que coeficientes y SEs existen
if (is.null(coeficientes) || is.null(errores_est)) {
  stop(paste("-> ERROR CRÍTICO: Faltan componentes $coeff o $coeff.se en el objeto del modelo (índice",
             indice_problematico, ")."))
}

# Nombres de las clases para las comparaciones (vs Clase 1 por defecto)
# -->> USA 'clases_deseadas' en lugar de 'n_clases_modelo' <<--
n_comparaciones <- clases_deseadas - 1
if (n_comparaciones <= 0) { stop("Error lógico: n_comparaciones debe ser > 0") }
nombres_comparacion <- paste("Log-Odds(Clase", 2:clases_deseadas, "/ Clase 1)")

# -->> WORKAROUND ADICIONAL: Asignar nombres de columna a coeficientes/SE <<--
cat("-> Aplicando workaround: Asignando nombres a columnas de coeficientes/SE...\n")
tryCatch({
  # Verifica dimensiones antes de asignar nombres
  if (ncol(coeficientes) != n_comparaciones) {
    stop(paste("El número de columnas en $coeff (", ncol(coeficientes), ") no coincide con las comparaciones esperadas (", n_comparaciones, ")."))
  }
  if (ncol(errores_est) != n_comparaciones) {
    stop(paste("El número de columnas en $coeff.se (", ncol(errores_est), ") no coincide con las comparaciones esperadas (", n_comparaciones, ")."))
  }
  colnames(coeficientes) <- nombres_comparacion
  colnames(errores_est) <- nombres_comparacion
  cat("-> Nombres de columna asignados correctamente.\n")
}, error = function(e) {
  stop(paste("-> ERROR CRÍTICO asignando nombres de columna a coeficientes/SE:", e$message))
})
# -->> FIN WORKAROUND ADICIONAL <<--


# Convertir matrices a dataframes y añadir nombres de covariables/niveles
if (is.null(rownames(coeficientes))) {
  stop("-> ERROR CRÍTICO: Los coeficientes no tienen nombres de fila (rownames). No se pueden identificar las variables.")
}
df_coef <- as.data.frame(coeficientes) %>% rownames_to_column(var = "Variable")
df_se <- as.data.frame(errores_est) %>% rownames_to_column(var = "Variable")


# Unir coeficientes y errores estándar y calcular CIs (95%)
z_critico <- qnorm(0.975)

plot_data <- df_coef %>%
  pivot_longer(cols = -Variable, names_to = "Comparacion", values_to = "Estimado") %>%
  left_join(
    df_se %>% pivot_longer(cols = -Variable, names_to = "Comparacion", values_to = "SE"),
    by = c("Variable", "Comparacion")
  ) %>%
  filter(!is.na(SE) & is.finite(SE) & SE > 0) %>% # Filtra SEs inválidos
  mutate(
    CI_lower = Estimado - z_critico * SE,
    CI_upper = Estimado + z_critico * SE,
    Comparacion = factor(Comparacion, levels = nombres_comparacion), # Usa los nombres asignados
    Variable = fct_rev(factor(Variable)) # Reordenar eje Y
  )

if(nrow(plot_data) == 0){
  stop("-> ERROR: No quedaron datos válidos para graficar después de procesar coeficientes y SE. Verifica los valores en $coeff y $coeff.se.")
}

cat("--- Creando el gráfico de efectos de covariables... ---\n")

# Crear el gráfico de coeficientes
plot_efectos_covariables <- ggplot(plot_data, aes(x = Estimado, y = Variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "gray") +
  geom_point(aes(color = Comparacion), size = 2.5, alpha = 0.8) +
  # -->> USA 'n_comparaciones' calculado a partir de 'clases_deseadas' <<--
  facet_wrap(~ Comparacion, scales = "free_x", ncol = n_comparaciones) +
  labs(
    title = "Efecto Estimado de Covariables en la Pertenencia a Clases Latentes",
    # -->> USA 'clases_deseadas' en lugar de 'n_clases_modelo' <<--
    subtitle = paste("Modelo de", clases_deseadas, "clases (Índice", indice_problematico,
                     ") | Coeficientes vs Clase 1 | IC 95%\nADVERTENCIA: Objeto de modelo poLCA con $nclass faltante."), # Subtítulo más informativo
    x = "Estimado del Coeficiente (Log-Odds relativo a Clase 1)",
    y = "Covariable / Nivel"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size=10),
    plot.subtitle = element_text(color = "orange4"), # Destaca la advertencia
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )

# Mostrar el gráfico
print(plot_efectos_covariables)

cat(paste0("\n--- Gráfico generado con workaround exitosamente para el modelo de ", clases_deseadas, " clases (índice ", indice_problematico, ") ---\n"))
cat("--- RECUERDA: Interpreta con cautela debido al estado inusual del objeto del modelo. ---\n")

# --- FIN DEL BLOQUE ---




