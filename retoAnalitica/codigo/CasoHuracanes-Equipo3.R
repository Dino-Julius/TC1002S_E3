# TC1002S.201 - Equipo 3
# Rero: Caso Huracanes
# Jesús Ángel Guzmán Ortega A01799257
# Julio Cesar Vivas Medina A01749879


# Instalación de liberías -------------------------------------------------

# Instalar el paquete dplyr si no está instalado
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}


# Liberías
library(dplyr)
# install.packages("ggplot2")
# install.packages("ggmap")
# install.packages("maps")
# install.packages("mapdata")


# Cargamos los datos
setwd("/cloud/project/retoAnalitica/codigo")
huracanes <- read.csv("CasoHuracanesCSV.csv")

# Filtrar los datos del Océano Pacífico Norte
huracanes_pacifico <- huracanes[huracanes$Ocean == "Pacific", ]

# 3.3.1 Eliminar los datos que no son reales (-999 en Wind y Pressure)
huracanes_pacifico <- huracanes_pacifico[!(huracanes_pacifico$Wind <= 0 | huracanes_pacifico$Pressure <= 0), ]

# 3. Exploración de los datos --------------------------------------------
# 3.1. Identificar el tipo de dato de tres columnas diferentes
Name_class <- class(huracanes$Name)
Name_class

Year_class <- class(huracanes$year)
Year_class

Co2_class <- class(huracanes$CO2)
Co2_class

# 3.2. Valores de los cuartiles de las variables : WIND, PRESSURE, CO2
cuartiles_wind <- quantile(huracanes_pacifico$Wind, probs = c(0.25, 0.5, 0.75))
cuartiles_wind

cuartiles_pressure <- quantile(huracanes_pacifico$Pressure, probs = c(0.25, 0.5, 0.75))
cuartiles_pressure

cuartiles_co2 <- quantile(huracanes_pacifico$CO2, probs = c(0.25, 0.5, 0.75))
cuartiles_co2


# Preparación de datos ----------------------------------------------------
# Convertir la columna "Fecha" al tipo de dato de fecha
huracanes_pacifico$Fecha <- as.Date(huracanes_pacifico$Fecha)

# 3.3.1 Eliminar la columna "Population" ya que todos los valores son 0
huracanes_pacifico <- huracanes_pacifico[, -grep("Population", names(huracanes_pacifico))]
huracanes_pacifico <- huracanes_pacifico[, !(names(huracanes_pacifico) == "Name")]

# 3.3.2 Eliminar los posibles NA
huracanes_pacifico <- na.omit(huracanes_pacifico)

# Filtrar huracanes_pacifico para incluir solo los registros donde el estatus sea "HU"
huracanes_pacifico_hu <- filter(huracanes_pacifico, Status == " HU")

# Realizar técnicas estadísticas ------------------------------------------

# 4.1 Muestra la MEDIA y el PROMEDIO de las variables : WIND y PRESSURE, CO2
media_wind_hu <- mean(huracanes_pacifico_hu$Wind)
media_wind_hu

media_pressure_hu <- mean(huracanes_pacifico_hu$Pressure)
media_pressure_hu

media_co2_hu <- mean(huracanes_pacifico_hu$CO2)
media_co2_hu

# 4.2 Indica los valores de los cuartiles de las variables : WIND, PRESSURE, CO2
cuartiles_wind_hu <- quantile(huracanes_pacifico_hu$Wind, probs = c(0.25, 0.5, 0.75))
cuartiles_wind_hu

cuartiles_pressure_hu <- quantile(huracanes_pacifico_hu$Pressure, probs = c(0.25, 0.5, 0.75))
cuartiles_pressure_hu

cuartiles_co2_hu <- quantile(huracanes_pacifico_hu$CO2, probs = c(0.25, 0.5, 0.75))
cuartiles_co2_hu

# 4.3 Emite conclusiones
# Revisión de datos
str(huracanes_pacifico)
# View(huracanes_pacifico)
