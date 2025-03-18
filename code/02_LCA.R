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

# Categoría 1: Desacuerdo (valores originales 1-2)
# Categoría 2: Acuerdo (valores originales 3-4)


#guardar 
save(vars_modelo, file = "objects/vars_modelo.RData")


#________________________Definir numero de grupos 
set.seed(1234)

# Clases latentes sin restricciones
f<- cbind(P7_2, P7_4, P7_5, P4_2, P4_4, 
          P7_1, P7_3, urgenciacrisis, 
          actividadeshumanas) ~ NULL


# N de clases 
lca2 <- poLCA(f, vars_modelo, nclass = 2, 
              maxiter = 1000, nrep = 10 , graphs=TRUE)
lca3 <- poLCA(f, vars_modelo, nclass = 3,  # modelo con mejor ajuste
              maxiter = 1000, nrep = 10, graphs=TRUE)
lca4 <- poLCA(f, vars_modelo, nclass = 4, 
              maxiter = 1000, nrep = 10, graphs=TRUE)
lca5 <- poLCA(f, vars_modelo, nclass = 5, 
              maxiter = 1000, nrep = 10, graphs=TRUE)
lca6 <- poLCA(f, vars_modelo, nclass = 6, 
              maxiter = 1000, nrep = 10, graphs=TRUE)

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

# Añadir entropía a los resultados comparativos
resultados$Entropia <- sapply(modelos, poLCA.entropy)

# Gráfico de criterios de información
ggplot(resultados, aes(x = Clases)) +
  geom_line(aes(y = BIC, colour = "BIC"), size = 1) +
  geom_point(aes(y = BIC, colour = "BIC"), size = 3) +
  geom_line(aes(y = AIC, colour = "AIC"), size = 1) +
  geom_point(aes(y = AIC, colour = "AIC"), size = 3) +
  theme_minimal() +
  labs(title = "Criterios de información por número de clases",
       y = "Valor", 
       colour = "Criterio") +
  theme(legend.position = "top")

# Mostrar tabla completa de resultados
resultados


#_________________________Analisis  3 clases. 

# probabilidades condicionales
modelo_3 <- lca3

# Examinar tamaños de clase 
tamano_clases <- data.frame(
  Clase = paste("Clase", 1:length(modelo_3$P)),
  Proporcion = modelo_3$P,
  Porcentaje = round(modelo_3$P * 100, 1)
)
print(tamano_clases)


# Extraer tamaño de las clases
class_sizes <- modelo_3$P

# Visualizar 
porcentajes <- round(modelo_3$P * 100, 1)
bp <- barplot(modelo_3$P * 100, 
              names.arg = paste("Clase", 1:length(modelo_3$P)),
              ylab = "Porcentaje (%)", 
              main = "Distribución de casos de clase latente",
              col = c("skyblue", "lightgreen", "salmon"))


# Añadir porcentajes directamente sobre las barras
text(bp, modelo_3$P * 50, # Posición a mitad de la barra
     labels = paste0(round(modelo_3$P * 100, 1), "%"),
     col = "black")



#_________________________Probabilidades condicionales por pregunta 

# Extraer probabilidades condicionales
probs_clase <- modelo_3$probs
print(probs_clase)

# Definir los nombres de variables
vars_nombres <- c("Problemas sociales > ambientales (MVI)", 
                  "Crecimiento sin daño ambiental (MVI)", 
                  "Problemas ambientales de países desarrollados (MVI)", 
                  "No cambiar costumbres, tecnología resolverá (MVI)", 
                  "Progreso es vivir como países desarrollados (MVI)", 
                  "Resolver problemas cambiando costumbres (RAA)", 
                  "Clases altas más responsables de crisis (RAA)",
                  "Urgencia de la crisis ambiental (RAA)", 
                  "Actividades humanas causan cambios climáticos (RAA)")

# Analizar las probabilidades para cada clase y crear una visualización más informativa
# Crear un dataframe largo para visualización
resultados_largo <- data.frame()

for (i in 1:length(probs_clase)) {
  var_nombre <- vars_nombres[i]
  temp_df <- data.frame(
    Variable = var_nombre,
    Clase = rep(paste("Clase", 1:3), each = 2),
    Respuesta = rep(c("Desacuerdo", "Acuerdo"), 3),
    Probabilidad = c(probs_clase[[i]][1, 1], probs_clase[[i]][1, 2],
                     probs_clase[[i]][2, 1], probs_clase[[i]][2, 2],
                     probs_clase[[i]][3, 1], probs_clase[[i]][3, 2])
  )
  resultados_largo <- rbind(resultados_largo, temp_df)
}



# Gráfico de barras apiladas que muestra ambas probabilidades
ggplot(resultados_largo, aes(x = Clase, y = Probabilidad, fill = Respuesta)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Desacuerdo" = "black", "Acuerdo" = "lightblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8)) +
  labs(title = "Probabilidades condicionales por clase latente",
       subtitle = "Probabilidad de responder Acuerdo vs Desacuerdo para cada variable",
       y = "Probabilidad")





#_______________________________Probabilidades por clase

# Definir los nombres 
var_originales <- c("P7_2", "P7_4", "P7_5", "P4_2", "P4_4", 
                    "P7_1", "P7_3", "urgenciacrisis", "actividadeshumanas")

# Preparar los datos 
LMmodelo <- data.frame()

# Para cada variable y cada clase
for (i in 1:length(probs_clase)) {
  var_nombre <- var_originales[i]
  
  for (j in 1:3) { # Para las 3 clases
    temp_df <- data.frame(
      item = var_nombre,
      state = j,
      category = c(0, 1),  # 0 = Desacuerdo, 1 = Acuerdo
      value = c(probs_clase[[i]][j, 1], probs_clase[[i]][j, 2])
    )
    LMmodelo <- rbind(LMmodelo, temp_df)
  }
}

# Obtener porcentajes de las clases
porcentajes <- round(modelo_3$P * 100, 0)

# Formatear los datos 
LMmodelo <- LMmodelo %>%
  dplyr::mutate(clase = case_when(
    state == 1 ~ paste0("Clase 1\n (", porcentajes[1], "%)"),
    state == 2 ~ paste0("Clase 2\n (", porcentajes[2], "%)"),
    state == 3 ~ paste0("Clase 3\n (", porcentajes[3], "%)")
  )) %>%
  dplyr::mutate(category = case_when(
    category == 0 ~ "Desacuerdo",
    category == 1 ~ "Acuerdo"
  ))

# Definir el orden de las variables
level_order <- var_originales

# Crear el gráfico con barras VERTICALES
ggplot(LMmodelo, aes(x = factor(item, level = level_order), y = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(clase ~ .) +
  scale_fill_manual(values = c("#000000", "#A8D8E8")) +  
  labs(x = "", y = "", fill = "") + 
  theme(text = element_text(size = 15)) +
  theme(
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 15),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11)
  ) +
  guides(fill = guide_legend(reverse = FALSE))



#________________________ Cambio de orden del grafico 


# Preparar los datos en formato adecuado
LMmodelo <- data.frame()

# Para cada variable y cada clase
for (i in 1:length(probs_clase)) {
  var_nombre <- var_originales[i]
  
  for (j in 1:3) { # Para las 3 clases
    temp_df <- data.frame(
      item = var_nombre,
      state = j,
      category = c(0, 1),  # 0 = Desacuerdo, 1 = Acuerdo
      value = c(probs_clase[[i]][j, 1], probs_clase[[i]][j, 2])
    )
    LMmodelo <- rbind(LMmodelo, temp_df)
  }
}

# Obtener porcentajes de las clases
porcentajes <- round(modelo_3$P * 100, 0)

# Formatear los datos 
LMmodelo <- LMmodelo %>%
  dplyr::mutate(clase = factor(paste0("Clase ", state, " (", porcentajes[state], "%)"),
                               levels = paste0("Clase ", 1:3, " (", porcentajes[1:3], "%)"))) %>%
  dplyr::mutate(category = factor(ifelse(category == 0, "Desacuerdo", "Acuerdo"),
                                  levels = c("Desacuerdo", "Acuerdo")))

# Mapeo simple para etiquetas más descriptivas
etiquetas <- c(
  "P7_2" = "P7_2: Problemas sociales > ambientales", 
  "P7_4" = "P7_4: Crecimiento sin daño ambiental", 
  "P7_5" = "P7_5: Problemas ambientales de países desarrollados", 
  "P4_2" = "P4_2: No cambiar costumbres, tecnología resolverá", 
  "P4_4" = "P4_4: Progreso como países desarrollados", 
  "P7_1" = "P7_1: Resolver problemas cambiando costumbres", 
  "P7_3" = "P7_3: Clases altas más responsables de crisis",
  "urgenciacrisis" = "Urgencia de la crisis ambiental", 
  "actividadeshumanas" = "Actividades humanas causan cambios"
)

# Crear el gráfico básico con barras HORIZONTALES
ggplot(LMmodelo, aes(y = factor(item, levels = var_originales, labels = etiquetas[var_originales]), 
                     x = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(. ~ clase) +  # Clases en columnas
  scale_fill_manual(values = c("Desacuerdo" = "#A8D8E8", "Acuerdo" = "#000000")) +
  labs(x = "", y = "", fill = "Respuesta") + 
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 11),
    legend.position = "top",
    panel.grid.major = element_blank()
  )



# Asignar clases a cada caso
clases_asignadas <- modelo_3$predclass


# Añadir predicciones de clase al conjunto de datos 
datos$clase_latente <- clases_asignadas

# Evaluar la calidad de clasificación
calidad_clasificacion <- mean(apply(modelo_3$posterior, 1, max))
cat("Probabilidad media de clasificación correcta:", round(calidad_clasificacion, 3), "\n")




# Cargar covariables
load("objects/covar.RData")


# Crear dataframe con clase latente y covariables
datos_analisis <- data.frame(
  clase_latente = factor(datos$clase_latente),
  sexo = covar$sexo,
  edad = covar$edad,
  educacion = covar$educacion,
  region = covar$region
)

# Tablas de contingencia para cada covariable
for(var in c("sexo", "edad", "educacion", "region")) {
  cat("\n\n### Distribución de clases por", var, "###\n")
  tabla <- table(datos_analisis$clase_latente, datos_analisis[[var]])
  
  print(tabla)
  
  # Porcentajes por columna
  prop_tabla <- prop.table(tabla, 2) * 100
  print(round(prop_tabla, 1))
  
  # Test chi-cuadrado
  test <- chisq.test(tabla)
  print(test)
}


# Crear fórmula
f_multi <- cbind(P7_2, P7_4, P7_5, P4_2, P4_4, 
                 P7_1, P7_3, urgenciacrisis, 
                 actividadeshumanas) ~ sexo + edad + educacion + region


# Combinar los dataframes
datos_completos <- cbind(vars_modelo, covar)




# Ajustar modelos con covariables para diferentes números de clases
lca_cov2 <- poLCA(f_multi, datos_completos, nclass = 2, maxiter = 3000, nrep = 10)
lca_cov3 <- poLCA(f_multi, datos_completos, nclass = 3, maxiter = 3000, nrep = 10)
lca_cov4 <- poLCA(f_multi, datos_completos, nclass = 4, maxiter = 3000, nrep = 10)
lca_cov5 <- poLCA(f_multi, datos_completos, nclass = 5, maxiter = 3000, nrep = 10)
lca_cov6 <- poLCA(f_multi, datos_completos, nclass = 6, maxiter = 3000, nrep = 10)



# Crear una lista con los modelos
modelos_cov <- list(lca_cov2, lca_cov3, lca_cov4, lca_cov5, lca_cov6)
n_clases <- 2:6

# Crear tabla de comparación
resultados_cov <- data.frame(
  Clases = n_clases,
  BIC = sapply(modelos_cov, function(x) x$bic),
  AIC = sapply(modelos_cov, function(x) x$aic),
  Log_likelihood = sapply(modelos_cov, function(x) x$llik)
)

# Ver resultados
print(resultados_cov)

# Añadir entropía
resultados_cov$Entropia <- sapply(modelos_cov, poLCA.entropy)

# Gráfico de criterios de información para modelos con covariables
ggplot(resultados_cov, aes(x = Clases)) +
  geom_line(aes(y = BIC, colour = "BIC"), size = 1) +
  geom_point(aes(y = BIC, colour = "BIC"), size = 3) +
  geom_line(aes(y = AIC, colour = "AIC"), size = 1) +
  geom_point(aes(y = AIC, colour = "AIC"), size = 3) +
  theme_minimal() +
  labs(title = "Criterios de información por número de clases (Con covariables)",
       y = "Valor", 
       colour = "Criterio") +
  theme(legend.position = "top")

# Comparar modelos con y sin covariables
resultados_combinados <- rbind(
  transform(resultados, Tipo = "Sin covariables"),
  transform(resultados_cov, Tipo = "Con covariables")
)

# Visualizar comparación
ggplot(resultados_combinados, aes(x = Clases, y = BIC, color = Tipo, group = Tipo)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Comparación de BIC entre modelos con y sin covariables",
       y = "BIC") +
  theme(legend.position = "top")





# Seleccionar el modelo de 3 clases con covariables
modelo_cov3 <- lca_cov3

# Examinar tamaños de clase y probabilidades condicionales
tamano_clases_cov <- data.frame(
  Clase = paste("Clase", 1:length(modelo_cov3$P)),
  Proporcion = modelo_cov3$P,
  Porcentaje = round(modelo_cov3$P * 100, 1)
)
print(tamano_clases_cov)

# Extraer probabilidades condicionales
probs_clase_cov <- modelo_cov3$probs
print(probs_clase_cov)

#  Visualizar probabilidades condicionales por pregunta
resultados_largo_cov <- data.frame()

for (i in 1:length(probs_clase_cov)) {
  var_nombre <- vars_nombres[i]
  temp_df <- data.frame(
    Variable = var_nombre,
    Clase = rep(paste("Clase", 1:3), each = 2),
    Respuesta = rep(c("Desacuerdo", "Acuerdo"), 3),
    Probabilidad = c(probs_clase_cov[[i]][1, 1], probs_clase_cov[[i]][1, 2],
                     probs_clase_cov[[i]][2, 1], probs_clase_cov[[i]][2, 2],
                     probs_clase_cov[[i]][3, 1], probs_clase_cov[[i]][3, 2])
  )
  resultados_largo_cov <- rbind(resultados_largo_cov, temp_df)
}


# Gráfico de barras apiladas
ggplot(resultados_largo_cov, aes(x = Clase, y = Probabilidad, fill = Respuesta)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Desacuerdo" = "black", "Acuerdo" = "lightblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8)) +
  labs(title = "Probabilidades condicionales por clase latente (con covariables)",
       subtitle = "Probabilidad de responder Acuerdo vs Desacuerdo para cada variable",
       y = "Probabilidad")




# Definir los nombres originales
var_originales <- c("P7_2", "P7_4", "P7_5", "P4_2", "P4_4", 
                    "P7_1", "P7_3", "urgenciacrisis", "actividadeshumanas")

# Preparar los datos para el modelo con covariables
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

# Mapeo simple para etiquetas más descriptivas
etiquetas <- c(
  "P7_2" = "P7_2: Problemas sociales > ambientales", 
  "P7_4" = "P7_4: Crecimiento sin daño ambiental", 
  "P7_5" = "P7_5: Problemas ambientales de países desarrollados", 
  "P4_2" = "P4_2: No cambiar costumbres, tecnología resolverá", 
  "P4_4" = "P4_4: Progreso como países desarrollados", 
  "P7_1" = "P7_1: Resolver problemas cambiando costumbres", 
  "P7_3" = "P7_3: Clases altas más responsables de crisis",
  "urgenciacrisis" = "Urgencia de la crisis ambiental", 
  "actividadeshumanas" = "Actividades humanas causan cambios"
)

# Crear el gráfico horizontal con barras apiladas (como en tu código original)
ggplot(LMmodelo_cov, aes(y = factor(item, levels = var_originales, labels = etiquetas[var_originales]), 
                         x = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(. ~ clase) +  # Clases en columnas
  scale_fill_manual(values = c("Desacuerdo" = "#A8D8E8", "Acuerdo" = "#000000")) +
  labs(title = "Probabilidades condicionales por clase latente (con covariables)",
       x = "", y = "", fill = "Respuesta") + 
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 11),
    legend.position = "top",
    panel.grid.major = element_blank()
  )



#  Analizar los coeficientes del modelo (efecto de covariables)
coeficientes <- modelo_cov3$coeff
errores <- modelo_cov3$coeff.se
odds_ratios <- exp(coeficientes)

# Tabla de resultados
tabla_coeficientes <- data.frame(
  Variable = rownames(coeficientes),
  Clase2_Coef = coeficientes[, 1],
  Clase2_SE = errores[, 1],
  Clase2_OR = odds_ratios[, 1],
  Clase3_Coef = coeficientes[, 2],
  Clase3_SE = errores[, 2],
  Clase3_OR = odds_ratios[, 2]
)

print(tabla_coeficientes)



























