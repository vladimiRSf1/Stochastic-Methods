#Stochastic Methods in Hydraulic Resources
# Ejercicio 04 - Funciones aleatorias

# Vladimir Sánchez Figueroa



# Eliminar variables y limpiar consola
rm(list=ls())
cat('\f')

# Configurar directorio de trabajo
location <- 'F:/MRH/Stochastic Methods/Taller4'
setwd(location)


# ==============
  file_path <- "resultados.txt"
con <- file(file_path, "w")
# ====================================
cat("Punto 1.\n", file = con)
cat("a. Serie aleatoria de valores continuos:\n", file = con)
cat("   Precipitación diaria.\n", file = con)
cat("   Caudal de un río.\n\n", file = con)

# b. Proceso de retícula
cat("b. Proceso de retícula:\n", file = con)
cat("   Distribución espacial de la humedad del suelo.\n", file = con)
cat("   Distribución espacial de la infiltración del agua.\n\n", file = con)

# c. Proceso de tiempo continuo
cat("c. Proceso de tiempo continuo:\n", file = con)
cat("   Evapotranspiración instantánea.\n", file = con)
cat("   Cambio continuo en el nivel de un acuífero.\n\n", file = con)

# d. Proceso de espacio continuo
cat("d. Proceso de espacio continuo:\n", file = con)
cat("   Distribución espacial de la intensidad de la lluvia.\n", file = con)
cat("   Variación espacial de la temperatura del agua en un embalse.\n\n", file = con)

# e. Proceso de espacio-tiempo continuo
cat("e. Proceso de espacio-tiempo continuo:\n", file = con)
cat("   Distribución espacial y temporal de la altura de las olas en un océano.\n", file = con)
cat("   Evolución espacio-temporal de la temperatura superficial del mar.\n\n", file = con)

# f. Proceso de punto compuesto en el tiempo
cat("f. Proceso de punto compuesto en el tiempo:\n", file = con)
cat("   Frecuencia de ocurrencia de eventos extremos de inundación en un año.\n", file = con)
cat("   Número de eventos de sequía en una temporada.\n\n", file = con)

# g. Proceso de punto compuesto en el espacio
cat("g. Proceso de punto compuesto en el espacio:\n", file = con)
cat("   Distribución espacial de la ubicación de los embalses.\n", file = con)
cat("   Distribución espacial de la ocurrencia de deslizamientos de tierra.\n", file = con)

# ========================================

cat("\nPunto 2.\n", file = con)
cat("b) Estacionario en sentido amplio.\n", file = con)

#===================================

cat("\nPunto 3.\n", file = con)
cat("c) Estacionario de segundo orden.\n", file = con)


#==========================================================================

cat("\nPunto 4.\n", file = con)
cat("c) Estacionario de segundo orden.\n", file = con)

#==========================================================================

cat("\nPunto 5.\n", file = con)
cat("d) Estrictamente estacionario.\n", file = con)

#==========================================================================


cat("\nPunto 9.\n", file = con)
cat("Gráfico generado como Punto_9.\n", file = con)
# Función para calcular la varianza de Z_T(t)
varianza_ZT <- function(T) {
  return(20 / T)
}

# Secuencia de valores de T de 1 a 100 días
T_valores <- 1:100

# Calcular la varianza de Z_T(t) para cada valor de T
varianza_ZT_valores <- varianza_ZT(T_valores)

# Graficar la relación entre la varianza de Z_T(t) y T
png("punto_9.png", width = 800, height = 600)
plot(T_valores, varianza_ZT_valores, type = "l", 
     xlab = "Tamaño de la ventana de promediado (T)",
     ylab = "Varianza de Z_T(t)",
     main = "Relación entre la varianza de Z_T(t) y T",
     col = "blue", lwd = 2)
dev.off()



