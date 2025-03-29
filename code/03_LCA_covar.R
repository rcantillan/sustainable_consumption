library(tidyverse)
library(poLCA)
library(knitr)
library(kableExtra)
library(ggplot2)
library(gridExtra)
library(here)
library(haven) # Necesario para read_sav

# Cargar datos
datos <- read_sav(here("datos", "BBDD Consolidada Maule UCM 2810.sav"))

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
n_clases <- 2:6

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
  g_squared <- numeric(length(indices_validos))
  chi_squared <- numeric(length(indices_validos))
  df_values <- numeric(length(indices_validos))
  
  # Extraer valores de cada modelo válido
  for (i in 1:length(indices_validos)) {
    idx <- indices_validos[i]
    modelo <- modelos_cov[[idx]]
    
    log_likelihood[i] <- modelo$llik
    bic_values[i] <- modelo$bic
    aic_values[i] <- modelo$aic
    entropia_values[i] <- poLCA.entropy(modelo)
    g_squared[i] <- modelo$Gsq
    chi_squared[i] <- modelo$Chisq
    df_values[i] <- modelo$df
  }
  
  # Crear tabla de resultados
  resultados <- data.frame(
    Clases = clases,
    Log_likelihood = log_likelihood,
    BIC = bic_values,
    AIC = aic_values,
    Entropia = entropia_values,
    G_squared = g_squared,
    Chi_squared = chi_squared,
    df = df_values
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








