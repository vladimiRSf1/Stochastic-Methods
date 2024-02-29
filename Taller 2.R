#Stochastic Methods in Hydraulic Resources
# Ejercicio 02 - Probabilidad y Variables Aleatorias.

# Vladimir Sánchez Figueroa



# Eliminar variables y limpiar consola
rm(list=ls())
cat('\f')

# Configurar directorio de trabajo
location <- 'F:/MRH/Stochastic Methods/Taller2'
setwd(location)

library(msm)
library(mvtnorm)

#==============================RESULTS========================================
file_path <- "resultados.txt"
con <- file(file_path, "w")

#======================Punto 1======================================================
# Parámetros de la distribución log-normal
mu_Y <- 2.0
sigma_Y_squared <- 1.5

# Calculando la media y la varianza de K
mean_K <- exp(mu_Y + 0.5 * sigma_Y_squared)
var_K <- (exp(sigma_Y_squared) - 1) * exp(2 * mu_Y + sigma_Y_squared)

# Mostrando los resultados
cat("Punto 1","\n", file = con)
cat("Media de K:", mean_K, "\n", file = con)
cat("Varianza de K:", var_K, "\n\n", file = con)

# =================Punto 2================================================


# Parámetros del problema
media <- 10  # media de la distribución lognormal
varianza <- 200  # varianza de la distribución lognormal

# Calculando la desviación estándar a partir de la varianza
sigma <- sqrt(varianza)

# Calcula la probabilidad de que la conductividad sea mayor que 30 m/d
prob_mayor_30 <- plnorm(30, meanlog = media, sdlog = sigma, lower.tail = FALSE)

# Mostrar el resultado
cat("Punto 2","\n", file = con)
cat("La probabilidad de que la conductividad sea mayor que 30 m/d es:", prob_mayor_30, "\n\n", file=con)

# =================Punto 3================================================
# Crea el dataframe
conductividad_hidraulica <- data.frame(
  "Conductividad_Hidraulica" = c(1e-3, 1e-2, 1e-1, 1e0, 1e1, 2e1, 5e1, 1e2),
  "Arena" = c(0.0, 0.0, 0.0, 0.1, 0.4, 0.3, 0.1, 0.1),
  "Arcilla" = c(0.3, 0.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0),
  "Turba" = c(0.1, 0.3, 0.3, 0.2, 0.1, 0.0, 0.0, 0.0)
)

# Definir las probabilidades de ocurrencia de cada textura
prob_arena <- 0.7
prob_arcilla <- 0.2
prob_turba <- 0.1

# Seleccionar las columnas de conductividad para cada textura del dataframe
conductividad_arena <- conductividad_hidraulica$Arena
conductividad_arcilla <- conductividad_hidraulica$Arcilla
conductividad_turba <- conductividad_hidraulica$Turba

# Calcular la distribución de probabilidad total de la conductividad para el acuífero
conductividad_acuifero <- prob_arena * conductividad_arena +
  prob_arcilla * conductividad_arcilla +
  prob_turba * conductividad_turba

#Mostrar la distribución de probabilidad total de la conductividad para el acuífero
cat("Punto 3","\n", file = con)
distr_prob <- data.frame(
  "Conductividad_Hidraulica" = c(1e-3, 1e-2, 1e-1, 1e0, 1e1, 2e1, 5e1, 1e2),
  "Prob. Acuifero" = c(conductividad_acuifero)
)

cat("\n", file = con)

# Guarda el dataframe en el archivo "resultados.txt"
write.table(distr_prob, file = con, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
cat("\n", file = con)
# =================Punto 4================================================

# Datos
mu_1 <- 10
mu_2 <- 25
var_1 <- 300
var_2 <- 450
rho <- 0.7

# a. Calcula la covarianza entre Z1 y Z2
cov_Z1_Z2 <- rho * sqrt(var_1) * sqrt(var_2)
cat("Punto 4","\n", file = con)
cat("a. Covarianza entre Z1 y Z2:", cov_Z1_Z2, "\n", file = con)

# b. Calcula el valor esperado de Y = Z1 + Z2
mean_Y <- mu_1 + mu_2
cat("b. Valor esperado de Y = Z1 + Z2:", mean_Y, "\n", file = con)

# c. Calcula la varianza de Y = Z1 + Z2
var_Y <- var_1 + var_2 + 2 * cov_Z1_Z2
cat("c. Varianza de Y = Z1 + Z2:", var_Y, "\n\n", file = con)


# =================Punto 5================================================

# Desviaciones estándar
sigma_1 <- sqrt(var_1)
sigma_2 <- sqrt(var_2)

# Límites
lim_Z1 <- 30
lim_Z2 <- 40
lim_Y <- 50

# Calculando las probabilidades acumuladas para Z1 < 30 y Z2 < 40
prob_Z1_less_than_30 <- pnorm(lim_Z1, mean = mu_1, sd = sigma_1)
prob_Z2_less_than_40 <- pnorm(lim_Z2, mean = mu_2, sd = sigma_2)

# Calculando la media y la varianza de Y = Z1 + Z2
mu_Y <- mu_1 + mu_2
var_Y <- var_1 + var_2 + 2 * rho * sigma_1 * sigma_2
sigma_Y <- sqrt(var_Y)

# Calculando la probabilidad de Z1 + Z2 < 50
prob_Y_less_than_50 <- pnorm(lim_Y, mean = mu_Y, sd = sigma_Y)

# Calculando la probabilidad de Z1 < 30 ⋂ Z2 < 40
prob_Z1_less_than_30_and_Z2_less_than_40 <- prob_Z1_less_than_30 * prob_Z2_less_than_40

# Calculando la probabilidad de Z1 < 30 ⋃ Z2 < 40
prob_Z1_less_than_30_or_Z2_less_than_40 <- prob_Z1_less_than_30 + prob_Z2_less_than_40 - prob_Z1_less_than_30_and_Z2_less_than_40

# Mostrando los resultados
cat("Punto 5","\n", file = con)
cat("a. Probabilidad de Z1 < 30:", prob_Z1_less_than_30, "\n", file = con)
cat("b. Probabilidad de Z2 < 40:", prob_Z2_less_than_40, "\n", file = con)
cat("c. Probabilidad de Z1 + Z2 < 50:", prob_Y_less_than_50, "\n", file = con)
cat("d. Probabilidad de Z1 < 30 ⋂ Z2 < 40:", prob_Z1_less_than_30_and_Z2_less_than_40, "\n", file = con)
cat("e. Probabilidad de Z1 < 30 ⋃ Z2 < 40:", prob_Z1_less_than_30_or_Z2_less_than_40, "\n", file = con)
cat("Codigo Finalizado")




