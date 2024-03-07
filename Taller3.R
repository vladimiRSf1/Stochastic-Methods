#Stochastic Methods in Hydraulic Resources
# Ejercicio 03 - Estadisticas hidrologicas y extremos

# Vladimir Sánchez Figueroa



# Eliminar variables y limpiar consola
rm(list=ls())
cat('\f')

# Configurar directorio de trabajo
location <- 'F:/MRH/Stochastic Methods/Taller3'
setwd(location)

library(trend)
library(extRemes)
library(evir)
require(wql)
library(dplyr)
library(fitdistrplus)
library(evd)

#==============
file_path <- "resultados.txt"
con <- file(file_path, "w")
# ====================================
  # Datos

# Crear un dataframe con los valores de año y caudal directamente
df <- data.frame(
  Año = c(1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 
          1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 
          1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 
          1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 
          1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 
          2000, 2001),
  Caudal_Maximo = c(1266, 1492, 1862, 861, 715, 1367, 1837, 1429, 1429, 1261,
                    1607, 2132, 1652, 1537, 1155, 1899, 1956, 1596, 1380, 745,
                    2181, 955, 1007, 824, 1271, 1044, 597, 1216, 1061, 1450,
                    2016, 1270, 1341, 1075, 2482, 874, 1689, 1554, 1831, 1149,
                    1444, 1791, 1207, 3050, 1578, 2817, 792, 1143, 1698, 2076,
                    1204, 1835)
)

Qmax<- as.numeric(df$Caudal_Maximo)

#====================================
# Realizar el test de Wilcoxon de independencia para los datos de caudales máximos
wilcox_test <- wilcox.test(Qmax)

# Imprimir el resultado
print("Test de Wilcoxon de independencia para los datos de caudales máximos:")
print(wilcox_test)

# ======================================

nombre_archivo <- "punto2.png"

png(nombre_archivo)

# Realizar el test de Mann-Kendall
result <- mannKen(df$Caudal_Maximo)

# Imprimir el resultado del test
print(result)

# Establecer parámetros gráficos
par(mar=c(3,3,2,0.5), oma=c(0,0,1,0), mgp=c(1.5,0.25,0), cex.axis=0.9, cex=0.9, tck=-0.01)

# Graficar serie de tiempo del caudal máximo diario anual
plot(df$Año, df$Caudal_Maximo, type='o', pch=16, cex=1,
     xlab='Año',
     ylab='Caudal Máximo [m3/s]',
     main='Serie de tiempo del caudal máximo diario anual')

# Ajustar modelo de regresión lineal
model <- lm(Caudal_Maximo ~ Año, data = df)
abline(model, col='green', lty=2)  # Agregar línea de regresión lineal

# Agregar suavizado local
lines(lowess(df$Año, df$Caudal_Maximo), col='red', lty=2)

# Obtener pendiente de la regresión y valor p
Pen_sen <- round(result$sen.slope,3)
p_value <- round(result$p.value,3)

# Mostrar información sobre la pendiente y el valor p
mtext(bquote('Pend. Sen' == .(Pen_sen) ~ "m3/s/año" ), side=3, line=-1.5, adj=0.98)
mtext(bquote('p-value' == .(p_value)), side=3, line=-3, adj=0.98)

# Realizar la comprobación utilizando una condicional
if (p_value > 0.1) {
  pto2 <- "No hay tendencia para una significancia del 10%(Test de MannKendall)"
} else {
  pto2 <- "Sí hay tendencia para una significancia del 10% (Test de MannKendall)"
}

# Imprimir el resultado
cat("Punto 2","\n","\n", file = con)
cat("p_value: ",p_value, "\n", file = con)
cat(pto2, "\n","\n", file = con)

# Finalizar el dispositivo gráfico
dev.off()




# =================

# Ajustar los datos a una distribución Gumbel
fit1 <- fevd(Qmax, type = "Gumbel")

# Graficar probabilidad-probabilidad y cuantil-cuantil
plot(fit1)

# Estimar los niveles de retorno para 1000 años
return_levels <- return.level(fit1, return.period = 1000)

# Imprimir los niveles de retorno
cat("Punto 3","\n","\n", file = con)
cat(return_levels, "\n","\n", file = con)

#=============================================

# Ordenar los caudales máximos de menor a mayor
df <- df[order(df$Caudal_Maximo),]

# Calcular los cuantiles teóricos de la distribución Gumbel
n <- nrow(df)
i <- 1:n
p <- (i - 0.44) / (n + 0.12)

# Calcular los valores transformados
x <- -log(-log(p))

# Ajustar el modelo de regresión lineal
modelo <- lm(Caudal_Maximo ~ x, data = df)

# Obtener los parámetros de la distribución Gumbel
beta1 <- coef(modelo)[1]
beta2 <- coef(modelo)[2]

# Estimar la inundación de 1000 años
p_1000 <- 1 - 1/1000
x_1000 <- -log(-log(p_1000))
inundacion_1000 <- exp(-exp((x_1000 - beta1) / beta2))

# Calcular los límites de confianza del 95%
conf_int <- confint(modelo, level = 0.95)
lower <- exp(-exp((x_1000 - conf_int["x", "2.5 %"]) / beta2))
upper <- exp(-exp((x_1000 - conf_int["x", "97.5 %"]) / beta2))

# Imprimir resultados
cat("Punto 4","\n","\n", file = con)
cat("Estimación de la inundación de 1000 años:", inundacion_1000,"\n", file = con)
cat("Límite inferior del 95%:", lower,"\n", file = con)
cat("Límite superior del 95%:", upper,"\n","\n", file = con)



# =======================================================
# Calcular la media y la desviación estándar de los caudales máximos
media <- mean(df$Caudal_Maximo)
desviacion <- sd(df$Caudal_Maximo)

# Calcular los parámetros de la distribución Gumbel
beta <- desviacion * sqrt(6) / pi
alfa <- media - 0.5772 * beta

# Estimar la inundación de 1000 años
inundacion_1000 <- alfa - beta * log(-log(1 - 1/1000))

# Calcular los límites de confianza del 95%
z <- qnorm(0.975)
lower <- alfa - beta * log(-log(1 - 0.025)) - z * desviacion * sqrt(6) / pi
upper <- alfa - beta * log(-log(1 - 0.975)) + z * desviacion * sqrt(6) / pi

# Imprimir resultados
cat("Punto 5","\n","\n", file = con)
cat("Estimación de la inundación de 1000 años:", inundacion_1000,"\n", file = con)
cat("Límite inferior del 95%:", lower,"\n", file = con)
cat("Límite superior del 95%:", upper,"\n", file = con)
cat("Parámetros de la distribución Gumbel:","\n", file = con)
cat("  Alfa:", alfa,"\n", file = con)
cat("  Beta:", beta,"\n","\n", file = con)

#=========================================================

unique_data <- unique(df$Caudal_Maximo)
# Establecer el nombre del archivo para guardar la gráfica
nombre_archivo <- "punto6.png"

# Abrir el dispositivo gráfico para guardar la gráfica en formato PNG
png(nombre_archivo)

# Ajustar una distribución lognormal a los caudales máximos observados
fit <- fitdist(df$Caudal_Maximo, "lnorm")

# Estimar la inundación de 1000 años
inundacion_1000_lognorm <- qlnorm(1 - 1/1000, meanlog = fit$estimate[1], sdlog = fit$estimate[2])

# Imprimir resultado
cat("Punto 6","\n","\n", file = con)
cat("Estimación de la inundación de 1000 años (distribución lognormal):", inundacion_1000_lognorm, "\n","\n", file = con)

# Realizar la prueba de Kolmogorov-Smirnov para una distribución lognormal
ks_test_lognorm <- ks.test(unique_data, "plnorm", meanlog = fit$estimate[1], sdlog = fit$estimate[2])

# Graficar el ajuste de la distribución lognormal
plot(fit)


# Finalizar el dispositivo gráfico
dev.off()

#========================================
# Eliminar valores atados de los datos
unique_data <- unique(df$Caudal_Maximo)

ks_test_gumbel <- ks.test(unique_data, "pgumbel", alfa, beta)

# Imprimir el resultado de la prueba
cat("Punto 7","\n","\n", file = con)
cat("Estadístico D:", ks_test_gumbel$statistic, "\n", file = con)
cat("p-valor:", ks_test_gumbel$p.value, "\n", "\n", file = con)



#===========================================


# Imprimir el resultado de la prueba
cat("Punto 8","\n","\n", file = con)
cat("Estadístico D:", ks_test_lognorm$statistic, "\n", file = con)
cat("p-valor:", ks_test_lognorm$p.value, "\n", "\n", file = con)



#============================

# Probabilidad de ocurrencia de una inundación de 1000 años en un año dado
prob_ocurrencia_anual <- 0.001

# Probabilidad de que no ocurra en un año dado
prob_no_ocurrencia_anual <- 1 - prob_ocurrencia_anual

# Probabilidad de que no ocurra en 40 años consecutivos
prob_no_ocurrencia_40_anios <- prob_no_ocurrencia_anual^40

# Probabilidad de que al menos ocurra una vez en los próximos 40 años
prob_al_menos_una_vez <- 1 - prob_no_ocurrencia_40_anios


# Imprimir el resultado
cat("Punto 9","\n","\n", file = con)
cat("Probabilidad de que ocurra al menos una inundación de 1000 años en los próximos 40 años:", prob_al_menos_una_vez,"\n","\n", file = con)


#=============================
# Tasa promedio de ocurrencia de la inundación de 1000 años en un año dado
tasa_promedio <- 0.001

# Número de años en el intervalo
anos <- 100

# Parámetro lambda para la distribución de Poisson
lambda <- tasa_promedio * anos

# Calcular la probabilidad de que ocurran exactamente dos veces en los próximos 100 años
probabilidad_dos_veces <- dpois(2, lambda)



cat("Punto 10","\n","\n", file = con)
cat("Probabilidad de que ocurra dos inundaciones de 1000 años en los próximos 100 años:", probabilidad_dos_veces,"\n","\n", file = con)

