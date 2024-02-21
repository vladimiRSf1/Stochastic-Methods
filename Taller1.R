#Stochastic Methods in Hydraulic Resources
# Ejercicio 01 - Estadística Descriptiva

# Vladimir Sánchez Figueroa



# Eliminar variables y limpiar consola
rm(list=ls())
cat('\f')

# Configurar directorio de trabajo
location <- 'E:/MRH/Stochastic Methods/Taller1'
setwd(location)

library(ggplot2)
library(gridExtra)
library(e1071)
library(knitr)
library(kableExtra)
library(webshot2)
library(dplyr)

# ========================================================================================



# Leer datos en formato txt
concent <- read.table('SMwr_Ex_01.txt', header=TRUE, sep='\t')


# 1. Haga un histograma de A con anchura de clases de 1, 2 y 5 unidades. 
# ¿Qué fracción de los datos tiene valores entre 5 y 10?

# Anchos de clase para el histograma
anchos_clase <- c(1, 2, 5)

# Crear una lista para almacenar los histogramas
histogramas <- list()

# Calcular los histogramas para diferentes anchos de clase
for (ancho in anchos_clase) {
  histograma <- ggplot(data = concent, aes(x = A)) +
    geom_histogram(binwidth = ancho, color = "black", fill = "skyblue", alpha = 0.7) +
    labs(title = paste("Histograma A (Clases", ancho,"und)"),
         x = "Valores",
         y = "Frecuencia")
  
  histogramas[[as.character(ancho)]] <- histograma
}

# Organizar los histogramas en una fila con tres columnas
pto1<-grid.arrange(grobs = histogramas, ncol = 3)

ggsave("Pto1.png", plot = pto1, width = 10, height = 5, units = "in")

## Calcular la fracción de los datos de A entre 5 y 10
datos_A_entre_5_y_10 <- concent$A[concent$A >= 5 & concent$A <= 10]
fraccion_A_entre_5_y_10 <- length(datos_A_entre_5_y_10) / nrow(concent)

# Imprimir la fracción
cat("SOLUCION PUNTO 1.\n\n")
cat("Fracción de datos de A entre 5 y 10:", fraccion_A_entre_5_y_10, "\n")

# 2. Haga un histograma de B con anchura de clases de 1, 2 y 5 unidades.
# ¿Qué fracción de los datos tiene valores entre 10 y 15?

# Anchos de clase para el histograma
anchos_clase <- c(1, 2, 5)

# Crear una lista para almacenar los histogramas
histogramas <- list()

# Calcular los histogramas para diferentes anchos de clase
for (ancho in anchos_clase) {
  histograma <- ggplot(data = concent, aes(x = B)) +
    geom_histogram(binwidth = ancho, color = "black", fill = "skyblue", alpha = 0.7) +
    labs(title = paste("Histograma B (Clases", ancho,"und)"),
         x = "Valores",
         y = "Frecuencia")
  
  histogramas[[as.character(ancho)]] <- histograma
}

# Organizar los histogramas en una fila con tres columnas
pto2<-grid.arrange(grobs = histogramas, ncol = 3)

ggsave("Pto2.png", plot = pto2, width = 10, height = 5, units = "in")

# Calcular la fracción de los datos de B entre 10 y 15
datos_B_entre_10_y_15 <- concent$B[concent$B >= 10 & concent$B <= 15]
fraccion_B_entre_10_y_15 <- length(datos_B_entre_10_y_15) / nrow(concent)

# Imprimir la fracción
cat("\nSOLUCION PUNTO 2.\n\n")
cat("Fracción de datos de B entre 10 y 15:", fraccion_B_entre_10_y_15, "\n")

# 3. Construya la distribución acumulativa de A y B.

# Crear el gráfico de la distribución acumulativa de A
grafico_A <- ggplot(concent, aes(x = A)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "Distribución acumulativa de A",
       x = "Valores de A",
       y = "Probabilidad acumulativa")

# Crear el gráfico de la distribución acumulativa de B
grafico_B <- ggplot(concent, aes(x = B)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "Distribución acumulativa de B",
       x = "Valores de B",
       y = "Probabilidad acumulativa")

# Mostrar los gráficos
pto3<-grid.arrange(grafico_A, grafico_B, ncol = 2)

ggsave("Pto3.png", plot = pto3, width = 10, height = 5, units = "in")

# 4. Calcule la media, la varianza, la asimetría, la curtosis, los cuantiles, la mediana y el rango intercuartílico de A y B.

# Calcular las medidas estadísticas para la columna A
media_A <- round(mean(concent$A),3)
varianza_A <- round(var(concent$A),3)
asimetria_A <- round(skewness(concent$A),3)
curtosis_A <- round(kurtosis(concent$A),3)
cuantiles_A <- round(quantile(concent$A, probs = c(0.25, 0.5, 0.75)),3)
mediana_A <- round(median(concent$A),3)
rango_intercuartilico_A <- round(IQR(concent$A),3)

# Calcular las medidas estadísticas para la columna B
media_B <- round(mean(concent$B),3)
varianza_B <- round(var(concent$B),3)
asimetria_B <- round(skewness(concent$B),3)
curtosis_B <- round(kurtosis(concent$B),3)
cuantiles_B <- round(quantile(concent$B, probs = c(0.25, 0.5, 0.75)),3)
mediana_B <- round(median(concent$B),3)
rango_intercuartilico_B <- round(IQR(concent$B),3)

# Mostrar los resultados
cat("\nSOLUCION PUNTO 4.\n\n")
Estadis. <- data.frame(
  Estadisticos = c("Media", "Varianza", "Asimetria", "Curtosis", "Cuantil 25%", "Mediana", "Cuantil 75%", "Rango Intercuartílico"),
  A = c(media_A, varianza_A, asimetria_A, curtosis_A, cuantiles_A[1], mediana_A, cuantiles_A[3], rango_intercuartilico_A),
  B = c(media_B, varianza_B, asimetria_B, curtosis_B, cuantiles_B[1], mediana_B, cuantiles_B[3], rango_intercuartilico_B)
)
print(Estadis.)
cat("\n")
# Crear la tabla de resultados
pto4 <- kable(Estadis., format = "html", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

writeLines(as.character(pto4), "pto4.html")

# Capturar una imagen del contenido HTML de la tabla
webshot2::webshot("pto4.html", file = "pto4.png", delay = 1)

# 5. Dibuje un diagrama de caja y bigotes de los valores de A y B. ¿Hay posibles valores atípicos?

rango_max <- max(max(concent$A), max(concent$B))

# Función para encontrar valores atípicos
find_outliers <- function(x) {
  quartiles <- quantile(x, probs = c(0.25, 0.75))
  iqr <- IQR(x)
  lower_fence <- quartiles[1] - 1.5 * iqr
  upper_fence <- quartiles[2] + 1.5 * iqr
  outliers <- x[x < lower_fence | x > upper_fence]
  return(outliers)
}

# Encontrar valores atípicos para A y B
outliers_A <- find_outliers(concent$A)
outliers_B <- find_outliers(concent$B)

# Crear el diagrama de caja y bigotes para A
boxplot_A <- ggplot(concent, aes(x = factor(1), y = A)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  geom_point(data = data.frame(y = outliers_A), aes(y = y), color = "red", size = 2) +
  geom_text(data = data.frame(y = outliers_A, label = round(outliers_A, 3)), aes(y = y, label = label),
            color = "red", hjust = -0.25, vjust = 0.25, angle=90) +
  labs(title = "Diagrama de caja y bigotes de A",
       x = "",
       y = "Valores de A") +
  theme_minimal() +
  coord_flip() +
  ylim(0, rango_max)  # Establecer el mismo rango en el eje y para ambos gráficos

# Crear el diagrama de caja y bigotes para B
boxplot_B <- ggplot(concent, aes(x = factor(1), y = B)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  geom_point(data = data.frame(y = outliers_B), aes(y = y), color = "red", size = 2) +
  geom_text(data = data.frame(y = outliers_B, label = round(outliers_B, 3)), aes(y = y, label = label),
            color = "red", hjust = -0.25, vjust = 0.25 , angle=90) +
  labs(title = "Diagrama de caja y bigotes de B",
       x = "",
       y = "Valores de B") +
  theme_minimal() +
  coord_flip() +
  ylim(0, rango_max)  # Establecer el mismo rango en el eje y para ambos gráficos

# Organizar los gráficos en una disposición de 2 filas y 1 columna
grid.arrange(boxplot_A, boxplot_B, ncol = 1)

# Organizar los gráficos en una disposición de 2 filas y 1 columna
pto5<-grid.arrange(boxplot_A, boxplot_B, ncol = 1)

ggsave("Pto5.png", plot = pto5, width = 10, height = 5, units = "in")

# 6. Suponga que A es la concentración de algún contaminante en el suelo (mg/kg). Supongamos que las
# muestras se han tomado aleatoriamente del sitio de interés. Si la concentración crítica es de 5 mg/kg y
# el sitio tiene 8000 m2

# . ¿Qué área aproximada del sitio debe ser limpiada?

proporcion_excedente_A <- sum(concent$A > 5) / nrow(concent)



# Calcular el área aproximada que debe ser limpiada
area_limpiada_A <- proporcion_excedente_A * 8000

  
# 7. Suponga que B es la concentración de algún contaminante en el suelo (mg/kg). Supongamos que las
# muestras se han tomado aleatoriamente del sitio de interés. Si la concentración crítica es de 10 mg/kg
# y el sitio tiene 8000 m2

# . ¿Qué área aproximada del sitio debe ser limpiada?

proporcion_excedente_B <- sum(concent$B > 10) / nrow(concent)



# Calcular el área aproximada que debe ser limpiada
area_limpiada_B <- proporcion_excedente_B * 8000

# Imprimir el resultado
cat("\nSOLUCION PUNTO 6 y 7.\n\n")
cat("El área aproximada del sitio que debe ser limpiada debido a la concentración de A es de", round(area_limpiada_A, 2), "m².","\nEl área aproximada del sitio que debe ser limpiada debido a la concentración de B es de", round(area_limpiada_B, 2), "m².\n\n")


  
# 8. Calcule el coeficiente de correlación entre A y B.

# Calcular el coeficiente de correlación entre A y B
coeficiente_correlacion <- cor(concent$A, concent$B)

# Crear el gráfico de dispersión
scatter_plot <- ggplot(concent, aes(x = A, y = B)) +
  geom_point(color = "blue") +  # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Línea de tendencia
  labs(title = "Correlación entre A y B",
       x = "A",
       y = "B") +
  theme_minimal()

# Agregar el coeficiente de correlación en la parte superior del gráfico
scatter_plot <- scatter_plot +
  annotate("text", x = max(concent$A), y = max(concent$B), label = paste("Coef. de Correlación:", round(coeficiente_correlacion, 3)), hjust = 1, vjust = 1, color = "black")

# Imprimir el gráfico
print(scatter_plot)
ggsave("Pto8.png", plot = scatter_plot, bg = "white")


# 9. ¿Qué fracción de los datos tiene un valor A menor que 5 y un valor B menor que 10?

# Filtrar el dataframe para obtener las filas donde A < 5 y B < 10
datos_Y <- filter(concent, A < 5 & B < 10)

# Calcular la fracción de los datos filtrados respecto al total de datos
fraccionY <- nrow(datos_Y) / nrow(concent)

# 10. ¿Qué fracción de los datos tiene un valor A menor que 5 o un valor B menor que 10?

datos_O <- filter(concent, A < 5 | B < 10)

# Calcular la fracción de los datos filtrados respecto al total de datos
fraccionO <- nrow(datos_O) / nrow(concent)




# Convertir la lista de resultados en una cadena de caracteres
resultados_texto <- paste(Estadis., collapse = "\n")

# Abrir un archivo de texto para escribir
file <- file("resultados.txt", "w")

# Imprimir la fracción del punto 1
cat("SOLUCION PUNTO 1.\n\n", file=file)
cat("Fracción de datos de A entre 5 y 10:", fraccion_A_entre_5_y_10, "\n\n", file=file)

# Imprimir la fracción del punto 2
cat("SOLUCION PUNTO 2.\n\n", file=file)
cat("Fracción de datos de B entre 10 y 15:", fraccion_B_entre_10_y_15, "\n\n", file=file)

# Imprimir los resultados del punto 4
cat("SOLUCION PUNTO 4.\n\n", file=file)
cat(resultados_texto, "\n\n", file=file)

# Imprimir el resultado de los puntos 6 y 7
cat("SOLUCION PUNTO 6 y 7.\n\n", file=file)
cat("El área aproximada del sitio que debe ser limpiada debido a la concentración de A es de", round(area_limpiada_A, 2), "m².","\nEl área aproximada del sitio que debe ser limpiada debido a la concentración de B es de", round(area_limpiada_B, 2), "m².\n\n", file=file)

# Imprimir el resultado de los puntos 9 y 10
cat("SOLUCION PUNTO 9 y 10.\n\n", file=file)
cat("La fracción de los datos con A < 5 y B < 10 es:", fraccionY,"\nLa fracción de los datos con A < 5 O B < 10 es:", fraccionO, "\n", file=file)

# Cerrar el archivo
close(file)
