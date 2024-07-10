# DRCAFTA_Evaluation

# Instalar y cargar los paquetes
install.packages("vars")
install.packages("readxl")
install.packages("dplyr")
install.packages("forecast")

library(vars)
library(readxl)
library(dplyr)
library(forecast)
library(tidyverse)

########## Creando datos para modelo de prueba (Datos ficticios)##################

# Fijar una semilla para reproducibilidad
set.seed(123)

# Generar años
years <- 1990:2024

# Generar datos aleatorios
data <- tibble(
  year = years,
  exportaciones = runif(length(years), min = 1000, max = 10000),
  importaciones = runif(length(years), min = 1000, max = 10000),
  pib = runif(length(years), min = 50000, max = 200000),
  pib_sector_agricola = runif(length(years), min = 5000, max = 20000),
  pib_sector_industrial = runif(length(years), min = 10000, max = 50000),
  tasa_desempleo = runif(length(years), min = 3, max = 15),
  tasa_subempleo_visible = runif(length(years), min = 5, max = 20),
  tasa_subempleo_invisible = runif(length(years), min = 5, max = 20),
  pobreza = runif(length(years), min = 10, max = 50),
  gini = runif(length(years), min = 0.3, max = 0.5),
  tratado_libre_comercio = ifelse(years >= 2006, 1, 0)
)

# Aplicar efectos del TLC
data <- data %>%
  mutate(
    exportaciones = ifelse(tratado_libre_comercio == 1, exportaciones * 2.5, exportaciones),
    importaciones = ifelse(tratado_libre_comercio == 1, importaciones * 4.3, importaciones),
    pobreza = ifelse(tratado_libre_comercio == 1, pobreza * -1.2, pobreza),
    gini = ifelse(tratado_libre_comercio == 1, gini * 1.3, gini)
  )

data<-data.frame(data)

### Transformacion a logaritmos

lexp<- log(data$exportaciones)
limp<-log(data$importaciones)
lpib<-log(data$pib)
pob<-data$pobreza
gini<-data$gini
TLC<-data$tratado_libre_comercio

data<-data.frame(lexp,limp,lpib,pob,gini, TLC)
##### Pruebo modelos lineas sencillos

m1<-lm(gini~TLC)
summary(m1)



################### Trabajando con el VAR-X ######################################

# Convertir los datos a una serie temporal
data_ts <- ts(data[,], start = c(1990, 1), frequency = 1) # Excluir la columna de año
data_ts

# Definir las variables endógenas y exógenas
endogenas <- data_ts[, c("lexp","limp","lpib","pob","gini")]
exogenas <- data_ts[, c("TLC")]


# Seleccionar el número óptimo de lags usando criterios de información
lag_selection <- VARselect(endogenas, lag.max = 10, type = "const", exogen = exogenas)
print(lag_selection)
# Ajustar el modelo VARX
modelo_varx <- vars::VAR(y = endogenas, exogen = exogenas, p = 4) # Ajustar el número de rezagos (p) según sea necesario

# Resumen del modelo
summary(modelo_varx)


# Dibujar las funciones de impulso respuesta

impulso_respuesta <- vars::irf(modelo_varx, impulse = "pob", response = c("pob"), n.ahead = 10, boot = TRUE, lag=4)

plot(impulso_respuesta)
