#Hydraulic Resources
# geoestatistics -TAller6 - Stochastic Methods.

# Vladimir Sánchez Figueroa


# Eliminar variables y limpiar consola
rm(list=ls())
cat('\f')

# Configurar directorio de trabajo=======
location <- 'F:/MRH/Stochastic Methods/Taller6'
setwd(location)



# Cargar bibliotecas necesarias
library(gstat)
library(sp)
library(automap)
library(gridExtra)


# Generar datos espaciales simulados con grilla
set.seed(Sys.time())
coords <- cbind(runif(50, 0, 100), runif(50, 0, 100))
values <- rnorm(50, mean = 10, sd = 3)

# Crear el objeto SpatialPointsDataFrame
data <- data.frame(values)
coordinates(data) <- coords

# Plot de los datos simulados con grilla
png(file = "Puntos simulados.png", width = 800, height = 600)
plot(data, main = "Datos espaciales simulados", pch = 16)
grid()
axis(1)
axis(2)
dev.off()

# Cálculo del semivariograma
png(file = "Semivariograma.png", width = 800, height = 600)
vgm_model <- autofitVariogram(values ~ 1, data)
plot(vgm_model)
dev.off()


# Definir la grilla de predicción
grd <- expand.grid(x = seq(0, 100, by = 5), y = seq(0, 100, by = 5))
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE

# Realizar Kriging
kriging_result <- krige(values ~ 1, data, grd, model = vgm_model$var_model)

png(file = "kriging.png", width = 800, height = 600)
spplot(kriging_result, zcol = "var1.pred", main = "Resultados de Kriging",
            scales = list(draw = TRUE), col.regions = terrain.colors(100))
dev.off()

# =============#
# Función para inclusión secuencial de puntos óptimos
sequential_inclusion <- function(data, n_additional_points) {
  current_points <- data
  additional_points <- NULL
  
  for (i in 1:n_additional_points) {
    candidate_points <- SpatialPoints(cbind(runif(100, 0, 100), runif(100, 0, 100)))
    best_point <- NULL
    min_variance <- Inf
    
    for (j in 1:length(candidate_points)) {
      temp_data <- rbind(current_points, SpatialPointsDataFrame(candidate_points[j], data.frame(values = 0)))
      temp_vgm <- autofitVariogram(values ~ 1, temp_data)
      temp_krige <- krige(values ~ 1, temp_data, grd, model = temp_vgm$var_model)
      variance <- mean(temp_krige@data$var1.var)
      
      if (variance < min_variance) {
        min_variance <- variance
        best_point <- candidate_points[j]
      }
    }
    
    current_points <- rbind(current_points, SpatialPointsDataFrame(best_point, data.frame(values = 0)))
    additional_points <- rbind(additional_points, coordinates(best_point))
  }
  
  return(SpatialPointsDataFrame(additional_points, data.frame(values = rep(0, n_additional_points))))
}

# Aplicar la función para incluir 5 puntos adicionales
additional_points <- sequential_inclusion(data, 5)

# Plot de los puntos originales y adicionales

png(file = "Puntos Adicionales.png", width = 800, height = 600)
plot(data, main = "Puntos originales y adicionales", pch = 16)
points(additional_points, col = "red", pch = 16)
grid()
axis(1)
axis(2)
dev.off()







