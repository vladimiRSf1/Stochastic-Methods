#Stochastic Methods in Hydraulic Resources
# Ejercicio 05 - Time series

# Vladimir Sánchez Figueroa



# Eliminar variables y limpiar consola
rm(list=ls())
cat('\f')

# Configurar directorio de trabajo
location <- 'F:/MRH/Stochastic Methods/Taller5'
setwd(location)


library(xlsx)
library(e1071)
library(ggplot2)
library(strucchange)
library(trend)
library(ggplot2)
library(wql)
library(CPAT)
library(climtrends)
library(gridExtra)
library(modifiedmk)
library(stats)
library(nortest)


# =============LECTURA DE ARCHIVO=======================

caudales <- read.table("SMWR_Lebrija.txt", header = FALSE)
fecha_inicio <- as.Date("2000-01-01")  # Fecha de inicio
num_dias <- nrow(caudales)  # Número total de días
fechas <- seq(fecha_inicio, by = "day", length.out = num_dias)

# Asignar las fechas a los caudales
caudales$Fecha <- fechas
colnames(caudales) <- c("Caudal", "Fecha")
caudales <- caudales[, c("Fecha", "Caudal")]

# =============AGREGAR SERIES=====================

# Calcular caudales medios mensuales
caudales_medios_mensuales <- aggregate(caudales[, "Caudal"], by = list(Mes = format(caudales$Fecha, "%m"), Año = format(caudales$Fecha, "%Y")), FUN = mean)

caudales_medios_mensuales$Mes <- as.character(caudales_medios_mensuales$Mes)
caudales_medios_mensuales$Año <- as.character(caudales_medios_mensuales$Año)
caudales_medios_mensuales$Fecha <- paste("01",caudales_medios_mensuales$Mes, caudales_medios_mensuales$Año, sep = "-")
caudales_medios_mensuales$Fecha <- as.Date(caudales_medios_mensuales$Fecha, format = "%d-%m-%Y")

# Calcular caudales medios anuales
caudales_medios_anuales <- aggregate(caudales[, "Caudal"], by = list(Año = format(caudales$Fecha, "%Y")), FUN = mean)

# Calcular caudales medios mensuales multianuales
caudales_medios_mensuales_multianuales <- aggregate(caudales[, "Caudal"], by = list(Mes = format(caudales$Fecha, "%m")), FUN = mean)

# Definir el nombre del archivo Excel
archivo_excel <- "series_caudales.xlsx"

# Escribir las series en diferentes hojas del archivo Excel
write.xlsx(caudales, file = archivo_excel, sheetName = "Serie Original", row.names = FALSE)
write.xlsx(caudales_medios_mensuales, file = archivo_excel, sheetName = "Caudales Medios Mensuales", append = TRUE, row.names = FALSE)
write.xlsx(caudales_medios_anuales, file = archivo_excel, sheetName = "Caudales Medios Anuales", append = TRUE, row.names = FALSE)
write.xlsx(caudales_medios_mensuales_multianuales, file = archivo_excel, sheetName = "Caudales Medios Mensuales Multianuales", append = TRUE, row.names = FALSE)

# ===========CALCULO DE ESTADISTICOS=============================

# a. Media
media_caudales <- round(mean(caudales$Caudal), 3)
media_caudales_mensuales <- round(mean(caudales_medios_mensuales$x), 3)
media_caudales_anuales <- round(mean(caudales_medios_anuales$x), 3)
media_caudales_multianuales <- round(mean(caudales_medios_mensuales_multianuales$x), 3)

# b. Varianza y desviación estándar
varianza_caudales <- round(var(caudales$Caudal), 3)
desviacion_estandar_caudales <- round(sd(caudales$Caudal), 3)
varianza_caudales_mensuales <- round(var(caudales_medios_mensuales$x), 3)
desviacion_estandar_caudales_mensuales <- round(sd(caudales_medios_mensuales$x), 3)
varianza_caudales_anuales <- round(var(caudales_medios_anuales$x), 3)
desviacion_estandar_caudales_anuales <- round(sd(caudales_medios_anuales$x), 3)
varianza_caudales_multianuales <- round(var(caudales_medios_mensuales_multianuales$x), 3)
desviacion_estandar_caudales_multianuales <- round(sd(caudales_medios_mensuales_multianuales$x), 3)

# c. Coeficiente de Variación
coef_variacion_caudales <- round(desviacion_estandar_caudales / media_caudales * 100, 3)
coef_variacion_caudales_mensuales <- round(desviacion_estandar_caudales_mensuales / media_caudales_mensuales * 100, 3)
coef_variacion_caudales_anuales <- round(desviacion_estandar_caudales_anuales / media_caudales_anuales * 100, 3)
coef_variacion_caudales_multianuales <- round(desviacion_estandar_caudales_multianuales / media_caudales_multianuales * 100, 3)

# d. Coeficiente de Asimetría
asimetria_caudales <- round(skewness(caudales$Caudal), 3)
asimetria_caudales_mensuales <- round(skewness(caudales_medios_mensuales$x), 3)
asimetria_caudales_anuales <- round(skewness(caudales_medios_anuales$x), 3)
asimetria_caudales_multianuales <- round(skewness(caudales_medios_mensuales_multianuales$x), 3)

# e. Coeficiente de Curtosis
curtosis_caudales <- round(kurtosis(caudales$Caudal), 3)
curtosis_caudales_mensuales <- round(kurtosis(caudales_medios_mensuales$x), 3)
curtosis_caudales_anuales <- round(kurtosis(caudales_medios_anuales$x), 3)
curtosis_caudales_multianuales <- round(kurtosis(caudales_medios_mensuales_multianuales$x), 3)

# # f. Covarianza y Correlación
# covarianza_series_mensuales <- round(cov(caudales_medios_mensuales$x, caudales_medios_anuales$x), 3)
# correlacion_series_mensuales <- round(cor(caudales_medios_mensuales$x, caudales_medios_anuales$x), 3)

# # g. Autocorrelación (ACF) y Autocorrelación parcial (PACF)
# acf_caudales <- round(acf(caudales$Caudal), 3)
# pacf_caudales <- round(pacf(caudales$Caudal), 3)

# # h. La función de densidad espectral (PSD)
# psd_caudales <- round(spec.pgram(caudales$Caudal, log = "no"), 3)


# Crear dataframe Estadis
Estadisticos <- data.frame(
  Estadisticos = c("Media", "Varianza", "Desviacion Estandar", "Coeficiente de Variacion", "Asimetria", "Curtosis"),
  Serie_Original = c(media_caudales, varianza_caudales, desviacion_estandar_caudales, coef_variacion_caudales, asimetria_caudales, curtosis_caudales),
  Serie_Mensuales = c(media_caudales_mensuales, varianza_caudales_mensuales, desviacion_estandar_caudales_mensuales, coef_variacion_caudales_mensuales, asimetria_caudales_mensuales, curtosis_caudales_mensuales),
  Serie_Anuales = c(media_caudales_anuales, varianza_caudales_anuales, desviacion_estandar_caudales_anuales, coef_variacion_caudales_anuales, asimetria_caudales_anuales, curtosis_caudales_anuales),
  Serie_Multianuales = c(media_caudales_multianuales, varianza_caudales_multianuales, desviacion_estandar_caudales_multianuales, coef_variacion_caudales_multianuales, asimetria_caudales_multianuales, curtosis_caudales_multianuales)
)

write.xlsx(Estadisticos, file = archivo_excel, sheetName = "Estadísticos", append = TRUE, row.names = FALSE)


# ========GRAFICO DE SERIES DE TIEMPO===================================
png("Series de tiempo.png", width = 1000, height = 800)
par(mfrow = c(2, 2))

# Graficar la serie original
plot(caudales$Fecha, caudales$Caudal, type = "l", main = "Caudales Diarios", xlab = "Fecha", ylab = "Caudal")

# Graficar la serie mensual
plot(caudales_medios_mensuales$Fecha, caudales_medios_mensuales$x, type = "l", main = "Caudales Mensuales", xlab = "Fecha", ylab = "Caudal")

# Graficar la serie anual
plot(caudales_medios_anuales$Año, caudales_medios_anuales$x, type = "l", main = "Caudales Medios Anuales", xlab = "Año", ylab = "Caudal")

# Graficar la serie multianual
plot(caudales_medios_mensuales_multianuales$Mes, caudales_medios_mensuales_multianuales$x, 
     type = "l", main = strwrap("Caudales Medios Mensuales Multianuales", width = 30), 
     xlab = "Mes", ylab = "Caudal")
axis(side = 1, at = 1:12)
dev.off()

#=========CORRELOGRAMAS===========================================
png("Correlogramas.png", width = 1000, height = 800) 
# Graficar el correlograma total de cada serie de tiempo con títulos ajustados
par(mfrow = c(2, 2))  # Dividir la ventana gráfica en un arreglo de 2x2

# Serie original
acf(caudales$Caudal, main = strwrap("Correlograma - Caudales Diarios", width = 30))

# Caudales medios mensuales
acf(caudales_medios_mensuales$x, main = strwrap("Correlograma - Caudales Medios Mensuales", width = 30))

# Caudales medios anuales
acf(caudales_medios_anuales$x, main = strwrap("Correlograma - Caudales Medios Anuales", width = 30))

# Caudales medios mensuales multianuales
acf(caudales_medios_mensuales_multianuales$x, main = strwrap("Correlograma - Caudales Medios Mensuales Multianuales", width = 30))
dev.off()

#=========CORRELOGRAMAS PARCIALES===========================================
png("Correlogramas Parciales.png", width = 1000, height = 800) 
par(mfrow = c(2, 2))  # Dividir la ventana gráfica en un arreglo de 2x2

# Serie original
pacf(caudales$Caudal, main = strwrap("Correlograma Parcial - Caudales Diarios", width = 30))

# Caudales medios mensuales
pacf(caudales_medios_mensuales$x, main = strwrap("Correlograma Parcial - Caudales Medios Mensuales", width = 30))

# Caudales medios anuales
pacf(caudales_medios_anuales$x, main = strwrap("Correlograma Parcial - Caudales Medios Anuales", width = 30))

# Caudales medios mensuales multianuales
pacf(caudales_medios_mensuales_multianuales$x, main = strwrap("Correlograma Parcial - Caudales Medios Mensuales Multianuales", width = 30))
dev.off()


#faltan numerales d,e,f

#=========PERIODOGRAMAS===========================================
png("Periodogramas.png", width = 1000, height = 800) 
# Calcular y graficar los periodogramas de cada serie de tiempo
par(mfrow = c(2, 2))  # Dividir la ventana gráfica en un arreglo de 2x2

# Serie original
periodograma_original <- spec.pgram(caudales$Caudal, log = "no", main = strwrap("Periodograma - Caudales Diarios", width = 30))

# Caudales medios mensuales
periodograma_mensuales <- spec.pgram(caudales_medios_mensuales$x, log = "no", main = strwrap("Periodograma - Caudales Medios Mensuales", width = 30))

# Caudales medios anuales
periodograma_anuales <- spec.pgram(caudales_medios_anuales$x, log = "no", main = strwrap("Periodograma - Caudales Medios Anuales", width = 30))

# Caudales medios mensuales multianuales
periodograma_multianuales <- spec.pgram(caudales_medios_mensuales_multianuales$x, log = "no", main = strwrap("Periodograma - Caudales Medios Mensuales Multianuales", width = 30))
dev.off()



#PENDIENTES IMF


#=========HISTOGRAMAS===========================================
png("Histogramas.png", width = 1000, height = 800) 
par(mfrow = c(2, 2))

# Graficar histograma para la serie original
hist(caudales$Caudal, main = strwrap("Caudales Diarios", width = 30), xlab = "Caudal", ylab = "Frecuencia")

# Graficar histograma para los caudales medios mensuales
hist(caudales_medios_mensuales$x, main = strwrap("Histograma - Caudales Medios Mensuales", width = 30), xlab = "Caudal", ylab = "Frecuencia")

# Graficar histograma para los caudales medios anuales
hist(caudales_medios_anuales$x, main = strwrap("Histograma - Caudales Medios Anuales", width = 30), xlab = "Caudal", ylab = "Frecuencia")

# Graficar histograma para los caudales medios mensuales multianuales
hist(caudales_medios_mensuales_multianuales$x, main = strwrap("Histograma - Caudales Medios Mensuales Multianuales", width = 30), xlab = "Caudal", ylab = "Frecuencia")
dev.off()
#=======================BOXPLOTS=========================
png("Boxplots.png", width = 1000, height = 800) 
par(mfrow = c(2, 2))

# Graficar diagrama de caja para la serie original
boxplot(caudales$Caudal, main = strwrap("Box Plot - Caudales Diarios", width = 30), ylab = "Caudal")

# Graficar diagrama de caja para los caudales medios mensuales
boxplot(caudales_medios_mensuales$x, main = strwrap("Box Plot - Caudales Medios Mensuales", width = 30), ylab = "Caudal")

# Graficar diagrama de caja para los caudales medios anuales
boxplot(caudales_medios_anuales$x, main = strwrap("Box Plot - Caudales Medios Anuales", width = 30), ylab = "Caudal")
  
# Graficar diagrama de caja para los caudales medios mensuales multianuales
boxplot(caudales_medios_mensuales_multianuales$x, main = strwrap("Box Plot - Caudales Medios Mensuales Multianuales", width = 30), ylab = "Caudal")
dev.off()

#========PRUEBAS DE HIPOTESIS- ANÁLISIS DE SALTOS===========
file_path <- "RESULTADOS PUNTO 4.txt"
con <- file(file_path, "w")

cat("Para puntos 1,2,3 se encuentran gráficas generadas en la ruta y un archivo excel que dan solucion\n
    Se presenta a continuación los resultados obtenidos en punto 4:\n","\n Las series se han renombrado a\n S1: Diarios S2: Medios Mensuales S3: Medios anuales S4: Medios Mensuales Multianuales\n\n" ,file=con)

s1<-caudales
s2<-caudales_medios_mensuales
s3<-caudales_medios_anuales
s4<-caudales_medios_mensuales_multianuales
s2$Año <- as.numeric(s2$Año)
s3$Año <- as.numeric(s3$Año)
s4$Mes <- as.numeric(s4$Mes)

#Prueba de pettitt###########

# Prueba de Pettitt para s1
resultado_petitt_s1 <- pettitt.test(s1$Caudal)
tiempo_cambio_s1 <- resultado_petitt_s1$estimate
p_value_s1 <- resultado_petitt_s1$p.value
anio_cambio_s1 <- min(s1$Fecha) + tiempo_cambio_s1 - 1
cat("Prueba de Pettitt para s1\n", "Estadística de prueba:", resultado_petitt_s1$statistic, "\nTiempo de cambio:", anio_cambio_s1, "\nValor p:", p_value_s1, "\n\n", file = con)

# Prueba de Pettitt para s2
resultado_petitt_s2 <- pettitt.test(s2$x)
tiempo_cambio_s2 <- resultado_petitt_s2$estimate
p_value_s2 <- resultado_petitt_s2$p.value
anio_cambio_s2 <- min(s2$Año) + tiempo_cambio_s2 - 1
cat("Prueba de Pettitt para s2\n", "Estadística de prueba:", resultado_petitt_s2$statistic, "\nTiempo de cambio:", anio_cambio_s2, "\nValor p:", p_value_s2, "\n\n", file = con)

# Prueba de Pettitt para s3
resultado_petitt_s3 <- pettitt.test(s3$x)
tiempo_cambio_s3 <- resultado_petitt_s3$estimate
p_value_s3 <- resultado_petitt_s3$p.value
anio_cambio_s3 <- min(s3$Año) + tiempo_cambio_s3 - 1
cat("Prueba de Pettitt para s3\n", "Estadística de prueba:", resultado_petitt_s3$statistic, "\nTiempo de cambio:", anio_cambio_s3, "\nValor p:", p_value_s3, "\n\n", file = con)

# Prueba de Pettitt para s4
resultado_petitt_s4 <- pettitt.test(s4$x)
tiempo_cambio_s4 <- resultado_petitt_s4$estimate
p_value_s4 <- resultado_petitt_s4$p.value
anio_cambio_s4 <- min(s4$Mes) + tiempo_cambio_s4 - 1
cat("Prueba de Pettitt para s4\n", "Estadística de prueba:", resultado_petitt_s4$statistic, "\nTiempo de cambio:", anio_cambio_s4, "\nValor p:", p_value_s4, "\n\n", file = con)

#Prueba de suma de rangos################





pettitt_diarios <- pettitt.test(caudales$Caudal)
pettitt_medios_anuales <- pettitt.test(caudales_medios_anuales$x)
pettitt_medios_mensuales <- pettitt.test(caudales_medios_mensuales$x)
pettitt_medios_mensuales_multianuales <- pettitt.test(caudales_medios_mensuales_multianuales$x)



cat("CODIGO FINALIZADO EXITOSAMENTE")


