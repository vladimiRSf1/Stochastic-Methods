#Hydraulic Resources
# Análisis de estaciones

# Vladimir Sánchez Figueroa


# Eliminar variables y limpiar consola
rm(list=ls())
cat('\f')

# Configurar directorio de trabajo=======
location <- 'D:/Escritorio/MRH/TESIS/GENERADO/MANEJO_ESTACIONES/Variables'
setwd(location)


#==================LIBRERIAS==================
library(ggplot2)
library(gridExtra)
library(e1071)
library(knitr)
library(kableExtra)
library(webshot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(xlsx)
library(DescTools)
library(forecast)

# ====================LECTURA DE DATOS==========================




#===============niveles y caudal==============
ruta <- "D:/Escritorio/MRH/TESIS/VISITAS DE CAMPO/VISITA 27-04-2024/DATOS ESTACIONES/CHINGAZA/VISITA 27_04_24/14778 HIDROLOGICA_Table1_2024-04-28T10-19.dat"
datos <- read.csv(ruta, skip = 3)
HIDROMETRICA <- datos[, c("X", "Smp")]
names(HIDROMETRICA) <- c("FECHA", "NIVEL")
HIDROMETRICA$FECHA <- as.POSIXct(HIDROMETRICA$FECHA, format = "%Y-%m-%d %H:%M:%S")
HIDROMETRICA_filtrada <- HIDROMETRICA[HIDROMETRICA$NIVEL >= 0 & HIDROMETRICA$NIVEL <= 2, ]
HIDROMETRICA<-HIDROMETRICA_filtrada
rm(HIDROMETRICA_filtrada)
CAUDALES <- data.frame(FECHA = HIDROMETRICA$FECHA)
CAUDALES$CAUDAL <- 1.024 * (HIDROMETRICA$NIVEL - 0.111) ^ 0.994


#===============meteorologica==============
ruta <- "D:/Escritorio/MRH/TESIS/VISITAS DE CAMPO/VISITA 27-04-2024/DATOS ESTACIONES/CHINGAZA/VISITA 27_04_24/14014_Table10_minutos_2024-04-28T09-13.dat"
datos <- read.csv(ruta, skip = 3)
PRECIPITACION <- datos[, c("X", "Tot")]
names(PRECIPITACION) <- c("FECHA", "P")
PRECIPITACION$FECHA <- as.POSIXct(PRECIPITACION$FECHA, format = "%Y-%m-%d %H:%M:%S")
#Agregar serie de precipitacion meteorologica a diaria

PRECIPITACION$FECHA <- as.Date(PRECIPITACION$FECHA)
# Agregar los datos de precipitación por día
PRECIPITACION_agg_dia <- aggregate(P ~ FECHA, data = PRECIPITACION, FUN = sum)
PRECIPITACION <- datos[, c("X", "Tot")]
names(PRECIPITACION) <- c("FECHA", "P")
PRECIPITACION$FECHA <- as.POSIXct(PRECIPITACION$FECHA, format = "%Y-%m-%d %H:%M:%S")


Tmax <- datos[, c(1, 22)]
names(Tmax) <- c("FECHA", "Tmax")
Tmax$FECHA <- as.POSIXct(Tmax$FECHA, format = "%Y-%m-%d %H:%M:%S")
Tmin <- datos[, c(1, 24)]
names(Tmin) <- c("FECHA", "Tmin")
Tmin$FECHA <- as.POSIXct(Tmin$FECHA, format = "%Y-%m-%d %H:%M:%S")


Txn <- merge(Tmax, Tmin, by = "FECHA")

TEMPERATURA <- data.frame(
  FECHA = Txn$FECHA,
  Tmax = Txn$Tmax,
  Tmin = Txn$Tmin,
  Tmed = (Txn$Tmax + Txn$Tmin) / 2
)

WSmax <- datos[, c(1, 13)]
names(WSmax) <- c("FECHA", "WSmax")
WSmax$FECHA <- as.POSIXct(WSmax$FECHA, format = "%Y-%m-%d %H:%M:%S")

WSmin <- datos[, c(1, 15)]
names(WSmin) <- c("FECHA", "WSmin")
WSmin$FECHA <- as.POSIXct(WSmin$FECHA, format = "%Y-%m-%d %H:%M:%S")


WSxn <- merge(WSmax, WSmin, by = "FECHA")

WIND_SPEED <- data.frame(
  FECHA = WSxn$FECHA,
  WSmax = WSxn$WSmax,
  WSmin = WSxn$WSmin,
  WSmed = (WSxn$WSmax + WSxn$WSmin) / 2
)

WIND_DIR <- datos[,c(1,17)]
names(WIND_DIR) <- c("FECHA", "W_dir")

HRmax <- datos[, c(1, 34)]
names(HRmax) <- c("FECHA", "HRmax")
HRmax$FECHA <- as.POSIXct(HRmax$FECHA, format = "%Y-%m-%d %H:%M:%S")

HRmin <- datos[, c(1, 36)]
names(HRmin) <- c("FECHA", "HRmin")
HRmin$FECHA <- as.POSIXct(HRmin$FECHA, format = "%Y-%m-%d %H:%M:%S")


HRxn <- merge(HRmax, HRmin, by = "FECHA")

HUMEDAD_RELATIVA <- data.frame(
  FECHA = HRxn$FECHA,
  HRmax = HRxn$HRmax,
  HRmin = HRxn$HRmin,
  HRmed = (HRxn$HRmax + HRxn$HRmin) / 2
)

RSmax <- datos[, c(1, 3)]
names(RSmax) <- c("FECHA", "RSmax")
RSmax$FECHA <- as.POSIXct(RSmax$FECHA, format = "%Y-%m-%d %H:%M:%S")

RSmin <- datos[, c(1, 5)]
names(RSmin) <- c("FECHA", "RSmin")
RSmin$FECHA <- as.POSIXct(RSmin$FECHA, format = "%Y-%m-%d %H:%M:%S")


RSxn <- merge(RSmax, RSmin, by = "FECHA")

RADIACION_SOLAR <- data.frame(
  FECHA = RSxn$FECHA,
  RSmax = RSxn$RSmax,
  RSmin = RSxn$RSmin,
  RSmed = (RSxn$RSmax + RSxn$RSmin) / 2
)


#===============precipitación pluviometro==============
ruta <- "D:/Escritorio/MRH/TESIS/VISITAS DE CAMPO/VISITA 27-04-2024/DATOS ESTACIONES/PRECIPITACION/27042024.dat"
datos <- read.csv(ruta)
PRECIPITACION_Pmetro <-datos[, c(1, 4)]
names(PRECIPITACION_Pmetro)<-(c("FECHA", "P"))
fechas <- paste(datos[,1], datos[,2])
PRECIPITACION_Pmetro <- data.frame(FECHA = as.POSIXct(fechas, format = "%m/%d/%y %H:%M"), P = datos[,4] * 0.254)
PRECIPITACION_Pmetro <- PRECIPITACION_Pmetro[PRECIPITACION_Pmetro$FECHA >= as.POSIXct("2023-07-01"), ]
PRECIPITACION_Pmetro$FECHA <- as.Date(PRECIPITACION_Pmetro$FECHA)
# Agregar los datos de precipitación por día
PRECIPITACION_Pmetro_agg_dia <- aggregate(P ~ FECHA, data = PRECIPITACION_Pmetro, FUN = sum)



write.csv(PRECIPITACION, "PRECIPITACION.csv", row.names = FALSE)
write.csv(PRECIPITACION_Pmetro_agg_dia, "PRECIPITACION_PMETRO.csv", row.names = FALSE)
write.csv(CAUDALES, "CAUDALES.csv", row.names = FALSE)
write.csv(HIDROMETRICA, "NIVELES.csv", row.names = FALSE)
write.csv(TEMPERATURA,"TEMPERATURA.csv", row.names = FALSE)
write.csv(WIND_SPEED,"WIND_SPEED.csv", row.names = FALSE)
write.csv(WIND_DIR,"WIND_DIRECTION.csv", row.names = FALSE)
write.csv(HUMEDAD_RELATIVA,"HUMEDAD_RELATIVA.csv", row.names = FALSE)
write.csv(RADIACION_SOLAR,"RADIACION_SOLAR.csv", row.names = FALSE)





#======================GRAFICA DE PRECIPITACION================================================================


# Primer DataFrame: PRECIPITACION
plot(PRECIPITACION$FECHA, PRECIPITACION$P, type = "l", col = "blue", 
     xlab = "Fecha", ylab = "Precipitación (mm)", 
     main = "Precipitación subdiaria", 
     xlim = c(min(PRECIPITACION$FECHA), max(PRECIPITACION$FECHA)),
     ylim = c(0, max(PRECIPITACION$P) + 5))

# Segundo DataFrame: PRECIPITACION_Pmetro_agg_dia
plot(PRECIPITACION_Pmetro_agg_dia$FECHA, PRECIPITACION_Pmetro_agg_dia$P, type = "l", col = "blue", 
     xlab = "Fecha", ylab = "Precipitación (mm)", 
     main = "Precipitación diaria", 
     xlim = c(min(PRECIPITACION_Pmetro_agg_dia$FECHA), max(PRECIPITACION_Pmetro_agg_dia$FECHA)),
     ylim = c(0, max(PRECIPITACION_Pmetro_agg_dia$P) + 5))

# Tercer DataFrame: PRECIPITACION_agg_dia
plot(PRECIPITACION_agg_dia$FECHA, PRECIPITACION_agg_dia$P, type = "l", col = "blue", 
     xlab = "Fecha", ylab = "Precipitación (mm)", 
     main = "Precipitación diaria", 
     xlim = c(min(PRECIPITACION_agg_dia$FECHA), max(PRECIPITACION_agg_dia$FECHA)),
     ylim = c(0, max(PRECIPITACION_agg_dia$P) + 5))

# Tercer DataFrame: TEMPERATURA
plot(TEMPERATURA$FECHA, TEMPERATURA$Tmax, type = "l", col = "red", 
     xlab = "Fecha", ylab = "Temperatura (°C)", 
     main = "Temperatura máxima subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(TEMPERATURA$Tmax) + 5))

plot(TEMPERATURA$FECHA, TEMPERATURA$Tmin, type = "l", col = "blue", 
     xlab = "Fecha", ylab = "Temperatura  (°C)", 
     main = "Temperatura mínima subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(TEMPERATURA$Tmin) + 5))

plot(TEMPERATURA$FECHA, TEMPERATURA$Tmed, type = "l", col = "blue", 
     xlab = "Fecha", ylab = "Temperatura media (°C)", 
     main = "Temperatura media subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(TEMPERATURA$Tmed) + 5))

# Tercer DataFrame: WIND SPEED
plot(WIND_SPEED$FECHA, WIND_SPEED$WSmax, type = "l", col = "green", 
     xlab = "Fecha", ylab = "Velocidad del viento (m/s)", 
     main = "Velocidad del viento máxima subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(WIND_SPEED$WSmax) + 5))

plot(WIND_SPEED$FECHA, WIND_SPEED$WSmin, type = "l", col = "green", 
     xlab = "Fecha", ylab = "Velocidad del viento (m/s)", 
     main = "Velocidad del viento minima subdiaria", 
     xlim = c(min(WIND_SPEED$FECHA), max(WIND_SPEED$FECHA)),
     ylim = c(0, max(WIND_SPEED$WSmin) + 5))

plot(WIND_SPEED$FECHA, WIND_SPEED$WSmed, type = "l", col = "green", 
     xlab = "Fecha", ylab = "Velocidad del viento (m/s)", 
     main = "Velocidad del viento máxima subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(WIND_SPEED$WSmed) + 5))

# Tercer DataFrame: humedad relativa

plot(WIND_SPEED$FECHA, HUMEDAD_RELATIVA$HRmax, type = "l", col = "green", 
     xlab = "Fecha", ylab = "Humedad relativa (%)", 
     main = "Humedad relativa máxima subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(HUMEDAD_RELATIVA$HRmax)))

plot(WIND_SPEED$FECHA, HUMEDAD_RELATIVA$HRmin, type = "l", col = "green", 
     xlab = "Fecha", ylab = "Humedad relativa (%)", 
     main = "Humedad relativa minima subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(HUMEDAD_RELATIVA$HRmin)))

plot(WIND_SPEED$FECHA, HUMEDAD_RELATIVA$HRmed, type = "l", col = "green", 
     xlab = "Fecha", ylab = "Humedad relativa (%)", 
     main = "Humedad relativa media subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(HUMEDAD_RELATIVA$HRmed)))

# Tercer DataFrame: radiacion solar

plot(WIND_SPEED$FECHA, RADIACION_SOLAR$RSmax, type = "l", col = "yellow", 
     xlab = "Fecha", ylab = "Radiación solar (W/m²)", 
     main = "Radiación solar máxima subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(RADIACION_SOLAR$RSmax)))

plot(WIND_SPEED$FECHA, RADIACION_SOLAR$RSmin, type = "l", col = "yellow", 
     xlab = "Fecha", ylab = "Radiación solar (W/m²)", 
     main = "Radiación solar minima subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(RADIACION_SOLAR$RSmin)))

plot(WIND_SPEED$FECHA, RADIACION_SOLAR$RSmed, type = "l", col = "yellow", 
     xlab = "Fecha", ylab = "Radiación solar (W/m²)", 
     main = "Radiación solar media subdiaria", 
     xlim = c(min(TEMPERATURA$FECHA), max(TEMPERATURA$FECHA)),
     ylim = c(0, max(RADIACION_SOLAR$RSmed)))

#Tercer DataFrame: NIVELES
plot(HIDROMETRICA$FECHA, HIDROMETRICA$NIVEL, type = "l", col = "red", 
     xlab = "Fecha", ylab = "Nivel (m)", 
     main = "Nivel de la estación hidrométrica", 
     xlim = c(min(HIDROMETRICA$FECHA), max(HIDROMETRICA$FECHA)))


#===========CALCULO DE ESTADISTICOS BASICOS============

lista_datos <- list(PRECIPITACION$P, 
                        PRECIPITACION_agg_dia$P, 
                        PRECIPITACION_Pmetro_agg_dia$P, 
                        TEMPERATURA$Tmax, 
                        TEMPERATURA$Tmin, 
                        HUMEDAD_RELATIVA$HRmax,
                        HUMEDAD_RELATIVA$HRmin,
                        RADIACION_SOLAR$RSmax,
                        RADIACION_SOLAR$RSmin,
                        WIND_SPEED$WSmax,
                        WIND_SPEED$WSmin,
                        HIDROMETRICA$NIVEL
                       )


nombres <- c("P", "Pdiaria", "Ppluviometrodiaria", "Tmax", "Tmin", 
             "HRmax", "HRmin", "RSmax", "RSmin", "WSmax", "WSmin", "Nivel")
names(lista_datos) <- nombres

# Crear la tabla
tabla_informacion <- data.frame(
  Variable = c("PRECIPITACION", "PRECIPITACION_agg_dia", "PRECIPITACION_Pmetro_agg_dia", 
               "TEMPERATURA_Tmax", "TEMPERATURA_Tmin", 
               "HUMEDAD_RELATIVA_HRmax", "HUMEDAD_RELATIVA_HRmin", 
               "RADIACION_SOLAR_RSmax", "RADIACION_SOLAR_RSmin", 
               "WIND_SPEED_WSmax", "WIND_SPEED_WSmin", "HIDROMETRICA_NIVEL"),
  Longitud = sapply(lista_datos, length),
  Maximo = sapply(lista_datos, max),
  Minimo = sapply(lista_datos, min),
  Media = sapply(lista_datos, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
  Varianza = sapply(lista_datos, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA),
  Desviacion_Estandar = sapply(lista_datos, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA),
  Coeficiente_Variacion = sapply(lista_datos, function(x) if(is.numeric(x) && mean(x, na.rm = TRUE) != 0) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) else NA),
  Coeficiente_Asimetria = sapply(lista_datos, function(x) if(is.numeric(x)) skewness(x, na.rm = TRUE) else NA),
  Coeficiente_Curtosis = sapply(lista_datos, function(x) if(is.numeric(x)) kurtosis(x, na.rm = TRUE) else NA)
)

acf_list <- lapply(lista_datos, acf)
pacf_list <- lapply(lista_datos, pacf)
write.csv(tabla_informacion, file = "tabla_resumen_estadisticos.csv", row.names = FALSE)

#===================GRAFICOS ESTADISTICOS=================

# Graficar correlograma total para cada serie de tiempo
par(mfrow=c(3, 4))  # Ajusta el diseño de la grilla según el número de series de tiempo
for(i in 1:length(lista_datos)) {
  acf(lista_datos[[i]], main = paste("Correlograma total -", names(lista_datos)[i]))
}

# Graficar correlograma parcial para cada serie de tiempo
par(mfrow=c(3, 4))  # Ajusta el diseño de la grilla según el número de series de tiempo
for(i in 1:length(lista_datos)) {
  pacf(lista_datos[[i]], main = paste("Correlograma parcial -", names(lista_datos)[i]))
}


# Graficar histograma para cada serie de tiempo
par(mfrow=c(3, 4))  # Ajusta el diseño de la grilla según el número de series de tiempo
for(i in 1:length(lista_datos)) {
  hist(lista_datos[[i]], main = paste("Histograma -", names(lista_datos)[i]))
}

# Graficar diagrama de caja y bigotes para cada serie de tiempo
par(mfrow=c(3, 4))  # Ajusta el diseño de la grilla según el número de series de tiempo
for(i in 1:length(lista_datos)) {
  boxplot(lista_datos[[i]], main = paste("Diagrama de caja -", names(lista_datos)[i]))
}

#=================================================================


# Crear y guardar correlograma total para cada serie de tiempo
for(i in 1:length(lista_datos)) {
  png(paste0("Correlograma_total_", names(lista_datos)[i], ".png"))
  acf(lista_datos[[i]], main = paste("Correlograma total -", names(lista_datos)[i]))
  dev.off()
}

# Crear y guardar correlograma parcial para cada serie de tiempo
for(i in 1:length(lista_datos)) {
  png(paste0("Correlograma_parcial_", names(lista_datos)[i], ".png"))
  pacf(lista_datos[[i]], main = paste("Correlograma parcial -", names(lista_datos)[i]))
  dev.off()
}


# Crear y guardar histograma para cada serie de tiempo
for(i in 1:length(lista_datos)) {
  png(paste0("Histograma_", names(lista_datos)[i], ".png"))
  hist(lista_datos[[i]], main = paste("Histograma -", names(lista_datos)[i]))
  dev.off()
}


# Crear y guardar diagrama de caja y bigotes para cada serie de tiempo
for(i in 1:length(lista_datos)) {
  png(paste0("Diagrama_caja_", names(lista_datos)[i], ".png"))
  boxplot(lista_datos[[i]], main = paste("Diagrama de caja -", names(lista_datos)[i]))
  dev.off()
}

# Crear y guardar gráficos para cada serie de tiempo
for(i in 1:length(lista_datos)) {
  # Definir el nombre del archivo
  file_name <- paste0("Grafico_", names(lista_datos)[i], ".png")
  
  # Crear el gráfico y guardarlo en un archivo PNG
  png(file_name)
  
  # Crear el gráfico
  plot(lista_datos[[i]], type = "l", col = "blue",
       xlab = "Observación", ylab = "Valor",
       main = paste("Serie de tiempo -", names(lista_datos)[i]),
       ylim = c(min(lista_datos[[i]], na.rm = TRUE), max(lista_datos[[i]], na.rm = TRUE) + 5))
  
  # Cerrar el dispositivo gráfico
  dev.off()
}


























































