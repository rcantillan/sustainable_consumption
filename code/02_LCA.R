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



# Extraer probabilidades condicionales
probs_clase <- modelo_3$probs
print(probs_clase)

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



# Probabilidad de responder "De acuerdo" (categoría 2) en cada variable

vars_nombres <- c( "Problemas sociales > ambientales (MVI)", 
                   "Crecimiento sin daño ambiental (MVI)", 
                   "Problemas ambientales de países desarrollados (MVI)", 
                   "No cambiar costumbres, tecnología resolverá (MVI)", 
                   "Progreso es vivir como países desarrollados (MVI)", 
                   "Resolver problemas cambiando costumbres (RAA)", 
                   "Clases altas más responsables de crisis (RAA)",
                   "Urgencia de la crisis ambiental (RAA)", 
                   "Actividades humanas causan cambios climáticos (RAA)" )

# Extraer probabilidades para la categoría "De acuerdo" (categoría 2)
probs_acuerdo <- data.frame(
  Variable = vars_nombres,
  Clase1 = sapply(1:length(modelo_3$probs), function(i) modelo_3$probs[[i]][1, 2]),
  Clase2 = sapply(1:length(modelo_3$probs), function(i) modelo_3$probs[[i]][2, 2]),
  Clase3 = sapply(1:length(modelo_3$probs), function(i) modelo_3$probs[[i]][3, 2])
)


# Preparar matriz para el heatmap
matriz_prob <- as.matrix(probs_acuerdo[, -1])
rownames(matriz_prob) <- probs_acuerdo$Variable

# Definir paleta de colores azules (sin blanco)
mi_paleta <- colorRampPalette(c("lightblue", "royalblue", "darkblue"))(100)

# Configurar márgenes para evitar superposiciones
par(mar = c(5, 18, 4, 5))

# Crear el heatmap con márgenes expandidos
heatmap(matriz_prob, 
        Rowv = NA, Colv = NA,    # Sin reordenar filas ni columnas
        col = mi_paleta,         # Usar paleta personalizada
        scale = "none",          # No estandarizar valores
        margins = c(3, 18),      # Aumentar margen para las etiquetas
        main = "Probabilidad de estar 'De acuerdo' por clase",
        xlab = "", ylab = "",    # Eliminar etiquetas de ejes
        cexRow = 0.9,            # Reducir tamaño de etiquetas de filas
        cexCol = 1.0)            # Tamaño de etiquetas de columnas

# Añadir leyenda de colores
legend(x = "right", 
       legend = c("0.0", "0.25", "0.5", "0.75", "1.0"), 
       fill = mi_paleta[c(1, 25, 50, 75, 100)],
       title = "Probabilidad",
       cex = 0.8,
       bty = "n")

#_______________ Interpretar clases

# Definir umbrales 
umbral_alto <- 0.7   # Probabilidad alta de estar de acuerdo
umbral_bajo <- 0.3   # Probabilidad baja de estar de acuerdo

# Crear función simple para interpretar las clases
interpretar_clases <- function(probs, tamanos_clase, umbral_alto = 0.7, umbral_bajo = 0.3) {
  for (i in 1:ncol(probs)) {
    clase_nombre <- colnames(probs)[i]
    tamano <- tamanos_clase[i, "Porcentaje"]
    cat(paste0("\n### ", clase_nombre, " (", tamano, "% de la muestra)\n"))
    
# Características donde la clase tiene alta probabilidad de acuerdo
    altas <- rownames(probs)[probs[, i] > umbral_alto]
    if (length(altas) > 0) {
      cat("\nAlta probabilidad de estar de acuerdo con:\n")
      for (caract in altas) {
        cat("- ", caract, "\n")
      }}
    
# Características donde la clase tiene baja probabilidad de acuerdo
    bajas <- rownames(probs)[probs[, i] < umbral_bajo]
    if (length(bajas) > 0) {
      cat("\nBaja probabilidad de estar de acuerdo con:\n")
      for (caract in bajas) {
        cat("- ", caract, "\n")
      }}
    
    cat("\n")}}

# Ejecutar la interpretación
interpretar_clases(matriz_prob, tamano_clases)     # A partir de esto sería importante decidir los nombres de cada clase en función de las características que las definen


# Asignar clases a cada caso
clases_asignadas <- modelo_3$predclass

# Añadir predicciones de clase al conjunto de datos 
datos$clase_latente <- clases_asignadas

# Evaluar la calidad de clasificación
calidad_clasificacion <- mean(apply(modelo_3$posterior, 1, max))
print(paste("Probabilidad media de clasificación:", round(calidad_clasificacion, 3)))


# Cargar covariables
load("objects/covar.RData")


str(covar)
# Crear dataframe con clase latente y covariables
datos_completos <- cbind(clase_latente = datos$clase_latente, covar)

#  factor la primera categoría como referencia
datos_completos$edad <- relevel(datos_completos$edad, ref = "18 a 24 años")
datos_completos$educacion <- relevel(datos_completos$educacion, ref = "Educación básica o menos")


# Tablas de contingencia para todas las variables
for(var in c("sexo", "edad", "educacion", "region")) {
  cat("\n\n### Distribución de clases por", var, "###\n")
  tabla <- table(datos_completos$clase_latente, datos_completos[[var]])
  
  print(tabla)
  
  # Porcentajes por columna
  prop_tabla <- prop.table(tabla, 2) * 100
  print(round(prop_tabla, 1))
  
  # Test chi-cuadrado
  test <- chisq.test(tabla)
  print(test)
}

# conjunto de datos para el analisis 
datos_analisis <- cbind(
  dplyr::select(datos, P7_2, P7_4, P7_5, P4_2, P4_4, P7_1, P7_3, urgenciacrisis, actividadeshumanas),covar)

# Crear fórmula
f_multi <- cbind(P7_2, P7_4, P7_5, P4_2, P4_4, 
                 P7_1, P7_3, urgenciacrisis, 
                 actividadeshumanas) ~ sexo + edad + educacion + region


#multinomial 
lca_multi <- poLCA(f_multi, datos_analisis, nclass = 3, 
                   maxiter = 1000, nrep = 5, graphs=TRUE)



print("Coeficientes del modelo (interpretados como log-odds):")
print(lca_multi$coeff)

#odds ratio

or <- exp(lca_multi$coeff)
print("Odds ratios:")
print(or)


# Guardar modelo y resultados para análisis
modelo_final <- modelo_3  # Asumiendo que este es tu modelo de 3 clases
save(modelo_final, probs_acuerdo, tamano_clases, file = "objects/resultados_lca.RData")

# Asignar las clases latentes a tus datos originales
datos_con_clases <- datos %>%
  mutate(clase_latente = modelo_final$predclass)

# Guardar el dataset con las clases asignadas
save(datos_con_clases, file = "objects/datos_con_clases.RData")
# Verificar que se guardaron correctamente
load("objects/resultados_lca.RData")
print(tamano_clases)
