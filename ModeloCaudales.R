#Hydraulic Resources
# Análisis de estaciones

# Vladimir Sánchez Figueroa


# Eliminar variables y limpiar consola

cat('\f')

# Configurar directorio de trabajo=======
location <- 'D:/Escritorio/Trabajo/Informacion_TOTA/Modelos'
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
library(xts)
library(tseries)
library(tidyverse)
library(car)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(broom)
library(MASS)
library(parallel)
library(mlogit)
library(fpp2)
library(stats)
library(quantmod)
library(hydroGOF)
library(readxl)
library(dplyr)
library(lubridate)
#====================FUNCIONES===============================

# Definir una función para calcular el NSE
NSE <- function(observed, predicted) {
  numerator <- sum((observed - predicted)^2)
  denominator <- sum((observed - mean(observed))^2)
  nse <- 1 - (numerator / denominator)
  return(nse)
}


# ====================LECTURA DE DATOS==========================

# Ruta del archivo Excel
ruta_archivo <- "Salidas GR2M.xlsx"

# Obtener nombres de todas las hojas del archivo Excel
hojas <- excel_sheets(ruta_archivo)

# Leer y extraer información de cada hoja
series_de_tiempo <- lapply(hojas, function(hoja) {
  # Leer la hoja
  datos <- read_excel(ruta_archivo, sheet = hoja)
  
  # Extraer las columnas FECHA y Qsim(m3/s)
  serie <- datos %>%
    select(FECHA, `Qsim(m3/s)`) %>%
    mutate(FECHA = as.Date(FECHA, format = "%Y-%m-%d"))
  
  # Convertir la serie a formato time series (ts)
  start_date <- as.Date("1984-02-01")
  end_date <- as.Date("2021-12-01")
  date_seq <- seq(start_date, end_date, by = "month")
  
  # Verificar si hay fechas faltantes en la secuencia
  if (length(date_seq) != nrow(serie)) {
    warning(paste("Hay fechas faltantes en la hoja", hoja))
  }
  
  # Crear la serie de tiempo con frecuencia mensual
  ts_data <- ts(serie$`Qsim(m3/s)`, start = c(year(start_date), month(start_date)), frequency = 12)
  
  return(ts_data)
})

# Nombrar cada serie de tiempo con el nombre de la hoja correspondiente
names(series_de_tiempo) <- hojas









# Crear la carpeta para guardar las gráficas
dir.create("analisisprevio", showWarnings = FALSE)

# Data frame para almacenar los resultados del test ADF
resultados_adf <- data.frame(hoja = character(), p_value = numeric(), stringsAsFactors = FALSE)

# Generar gráficas y realizar el test ADF para cada serie de tiempo
for (hoja in hojas) {
  serie <- series_de_tiempo[[hoja]]
  
  # Generar las gráficas y guardarlas
  png(filename = paste0("analisisprevio/", hoja, ".png"), width = 800, height = 1200)
  par(mfrow = c(3, 1))
  plot(serie, main = paste("Serie de Tiempo -", hoja))
  acf(serie, main = paste("ACF -", hoja))
  pacf(serie, main = paste("PACF -", hoja))
  dev.off()
  
  # Realizar el test ADF
  adf_result <- adf.test(serie, alternative = "stationary")
  
  # Guardar el p-value en el data frame
  resultados_adf <- rbind(resultados_adf, data.frame(hoja = hoja, p_value = adf_result$p.value))
  
  # Descomposición de la serie y guardar gráfica
  png(filename = paste0("analisisprevio/", hoja, "_decompose.png"), width = 800, height = 1200)
  plot(decompose(serie))
  dev.off()
}

# Guardar los resultados del test ADF en un archivo CSV
write.csv(resultados_adf, "analisisprevio/resultados_adf.csv", row.names = FALSE)




# Crear la carpeta para guardar las matrices BIC
dir.create("BICseasonal", showWarnings = FALSE)

# Inicializar lista para almacenar parámetros óptimos
parametros_optimos <- list()

# Definir los rangos de los parámetros p, d, q, P, D, Q
p_max <- 2
d_max <- 2
q_max <- 2
P_max <- 2
D_max <- 2
Q_max <- 2
period <- 12

# Iterar sobre cada serie de tiempo y calcular las matrices BIC
for (hoja in hojas) {
  variable <- series_de_tiempo[[hoja]]
  
  # Crear una matriz para almacenar los valores de BIC
  bic_matrix <- array(NA, dim = c(p_max + 1, d_max + 1, q_max + 1, P_max + 1, D_max + 1, Q_max + 1))
  dimnames(bic_matrix) <- list(0:p_max, 0:d_max, 0:q_max, 0:P_max, 0:D_max, 0:Q_max)
  
  # Iterar sobre todos los posibles valores de p, d, q, P, D, Q y calcular BIC
  for (p in 0:p_max) {
    for (d in 0:d_max) {
      for (q in 0:q_max) {
        for (P in 0:P_max) {
          for (D in 0:D_max) {
            for (Q in 0:Q_max) {
              modelo <- tryCatch(arima(variable, order = c(p, d, q),
                                       seasonal = list(order = c(P, D, Q), period = period)), 
                                 error = function(e) NULL)
              if (!is.null(modelo)) {
                bic_matrix[p + 1, d + 1, q + 1, P + 1, D + 1, Q + 1] <- BIC(modelo)
              }
            }
          }
        }
      }
    }
  }
  
  # Convertir la matriz BIC en un data frame
  bic_df <- as.data.frame.table(bic_matrix)
  colnames(bic_df) <- c("p", "d", "q", "P", "D", "Q", "BIC")
  
  # Guardar la matriz BIC en un archivo CSV
  write.csv(bic_df, file = paste0("BICseasonal/BIC_matrix_", hoja, ".csv"), row.names = FALSE)
  
  # Encontrar el mínimo BIC y sus correspondientes parámetros p, d, q, P, D, Q
  min_bic <- min(bic_matrix, na.rm = TRUE)
  min_bic_pos <- which(bic_matrix == min_bic, arr.ind = TRUE)
  p_optimo <- min_bic_pos[1] - 1
  d_optimo <- min_bic_pos[2] - 1
  q_optimo <- min_bic_pos[3] - 1
  P_optimo <- min_bic_pos[4] - 1
  D_optimo <- min_bic_pos[5] - 1
  Q_optimo <- min_bic_pos[6] - 1
  
  # Almacenar los parámetros óptimos
  parametros_optimos[[hoja]] <- list(p = p_optimo, d = d_optimo, q = q_optimo, P = P_optimo, D = D_optimo, Q = Q_optimo, BIC = min_bic)
}

# Convertir la lista de parámetros óptimos a un data frame
parametros_optimos_df <- do.call(rbind, lapply(names(parametros_optimos), function(hoja) {
  data.frame(Hoja = hoja,
             p = parametros_optimos[[hoja]]$p,
             d = parametros_optimos[[hoja]]$d,
             q = parametros_optimos[[hoja]]$q,
             P = parametros_optimos[[hoja]]$P,
             D = parametros_optimos[[hoja]]$D,
             Q = parametros_optimos[[hoja]]$Q,
             BIC = parametros_optimos[[hoja]]$BIC)
}))

# Guardar los parámetros óptimos en un archivo CSV
write.csv(parametros_optimos_df, file = "BICseasonal/parametros_optimos.csv", row.names = FALSE)






# Crear las carpetas para guardar resultados
dir.create("tsdiag_arima", showWarnings = FALSE)
dir.create("Series_Sinteticas", showWarnings = FALSE)
dir.create("Comparacion_Modelo", showWarnings = FALSE)

# Inicializar lista para almacenar resultados
resultados <- list()

# Iterar sobre cada serie de tiempo y ejecutar los pasos
for (hoja in names(series_de_tiempo)) {
  variable <- series_de_tiempo[[hoja]]
  params <- parametros_optimos[[hoja]]
  
  # Obtener los parámetros óptimos
  p_optimo <- params$p
  d_optimo <- params$d
  q_optimo <- params$q
  P_optimo <- params$P
  D_optimo <- params$D
  Q_optimo <- params$Q
  
  # Ejecutar el modelo ARIMA con los parámetros óptimos
  modelo_optimo <- arima(variable, order = c(p_optimo, d_optimo, q_optimo),
                         seasonal = list(order = c(P_optimo, D_optimo, Q_optimo), period = 12))
  
  # Generar y guardar los diagnósticos del modelo ARIMA
  png(filename = paste0("tsdiag_arima/tsdiag_", hoja, ".png"))
  tsdiag(modelo_optimo, gof.lag = 20)
  dev.off()
  
  # Realizar el test de Ljung-Box
  ljung_box_test <- Box.test(residuals(modelo_optimo), type = "Ljung-Box")
  
  # Generar las series sintéticas
  serie_sintetica <- fitted(modelo_optimo)
  
  # Calcular RMSE y NSE
  rmse <- sqrt(mean((variable - serie_sintetica)^2))
  nse <- NSE(variable, serie_sintetica)
  
  # Guardar la serie sintética en un archivo CSV
  write.csv(data.frame(Tiempo = time(variable), Original = variable, Sintetica = serie_sintetica),
            file = paste0("Series_Sinteticas/serie_sintetica_", hoja, ".csv"), row.names = FALSE)
  
  # Almacenar los resultados
  resultados[[hoja]] <- list(p = p_optimo, d = d_optimo, q = q_optimo, P = P_optimo, D = D_optimo, Q = Q_optimo,
                             BIC = params$BIC, p_value_ljung_box = ljung_box_test$p.value, RMSE = rmse, NSE = nse)
  
  # Generar la gráfica de comparación
  png(filename = paste0("Comparacion_Modelo/comparacion_", hoja, ".png"),
      width = 1200, height = 800,)
  plot(variable, main = paste("Serie de Tiempo Original y Valores Ajustados -", hoja), ylab = "Valores")
  lines(serie_sintetica, col = "red", lwd = 2)
  legend("topright", legend = c("Original", "Ajustado"), col = c("black", "red"), lty = 1, lwd = 2)
  text(x = par("usr")[1], y = par("usr")[4], 
       labels = paste("RMSE:", round(rmse, 2), "\nNSE:", round(nse, 2)), 
       adj = c(0, 1), cex = 0.8, col = "blue")
  dev.off()
}

# Convertir la lista de resultados a un data frame
resultados_df <- do.call(rbind, lapply(names(resultados), function(hoja) {
  data.frame(Hoja = hoja,
             p = resultados[[hoja]]$p,
             d = resultados[[hoja]]$d,
             q = resultados[[hoja]]$q,
             P = resultados[[hoja]]$P,
             D = resultados[[hoja]]$D,
             Q = resultados[[hoja]]$Q,
             BIC = resultados[[hoja]]$BIC,
             p_value_ljung_box = resultados[[hoja]]$p_value_ljung_box,
             RMSE = resultados[[hoja]]$RMSE,
             NSE = resultados[[hoja]]$NSE)
}))

# Guardar los resultados en un archivo CSV
write.csv(resultados_df, file = "Series_Sinteticas/resultados_optimos.csv", row.names = FALSE)







# Crear la carpeta para guardar las predicciones
dir.create("Predicciones", showWarnings = FALSE)

# Definir el rango de predicción (hasta diciembre de 2034)
horizonte_prediccion <- (2034 - max(time(series_de_tiempo[[1]])) %/% 1) * 12

# Inicializar lista para almacenar las predicciones
predicciones <- list()

# Iterar sobre cada serie de tiempo y generar predicciones
for (hoja in names(series_de_tiempo)) {
  variable <- series_de_tiempo[[hoja]]
  params <- parametros_optimos[[hoja]]
  
  # Obtener los parámetros óptimos
  p_optimo <- params$p
  d_optimo <- params$d
  q_optimo <- params$q
  P_optimo <- params$P
  D_optimo <- params$D
  Q_optimo <- params$Q
  
  # Ajustar el modelo ARIMA con los parámetros óptimos
  modelo_optimo <- arima(variable, order = c(p_optimo, d_optimo, q_optimo),
                         seasonal = list(order = c(P_optimo, D_optimo, Q_optimo), period = 12))
  
  # Generar predicciones hasta el año 2034
  forecast_modelo <- forecast(modelo_optimo, h = horizonte_prediccion)
  
  # Guardar las predicciones en una lista
  predicciones[[hoja]] <- forecast_modelo
  
  # Guardar las predicciones en un archivo CSV
  predicciones_df <- data.frame(Tiempo = time(forecast_modelo$mean), Prediccion = as.numeric(forecast_modelo$mean))
  write.csv(predicciones_df, file = paste0("Predicciones/prediccion_", hoja, ".csv"), row.names = FALSE)
  
  # Graficar las predicciones junto con la serie de tiempo original
  png(filename = paste0("Predicciones/grafica_prediccion_", hoja, ".png"),
      width = 1200, height = 800,)
  plot(forecast_modelo, main = paste("Predicción de la serie de tiempo -", hoja), ylab = "Valores")
  lines(variable, col = "blue", lwd = 2)
  legend("topright", legend = c("Original", "Predicción"), col = c("blue", "red"), lty = 1, lwd = 2)
  dev.off()
}

# Guardar las predicciones en una tabla de resultados
resultados_predicciones <- do.call(rbind, lapply(names(predicciones), function(hoja) {
  data.frame(Hoja = hoja,
             Tiempo = time(predicciones[[hoja]]$mean),
             Prediccion = as.numeric(predicciones[[hoja]]$mean))
}))

# Guardar los resultados de las predicciones en un archivo CSV
write.csv(resultados_predicciones, file = "Predicciones/resultados_predicciones.csv", row.names = FALSE)





# ========================================================##==

