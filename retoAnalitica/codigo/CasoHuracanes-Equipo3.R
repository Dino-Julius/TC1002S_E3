# TC1002S.201 - Equipo 3
# Rero: Caso Huracanes
# Jesús Ángel Guzmán Ortega A01799257
# Julio Cesar Vivas Medina A01749879

# Cambiar directorio de trabajo y cargar el archivo CSV --------------------

setwd("/cloud/project/retoAnalitica/codigo")
huracanes <- read.csv("CasoHuracanesCSV.csv")

# Instalar y cargar librerías ----------------------------------------------
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")

# Verificar y tratar los valores nulos -----------------------------------

sapply(huracanes, function(x) sum(is.na(x)))
summary(huracanes)
View(huracanes)

# obtener todos los valores unicos de la columna Ocean
unique(huracanes$Ocean)
unique(huracanes$Name)
unique(huracanes$Status)
unique(huracanes$Latitude)


# Objetivo del análisis ---------------------------------------------------

# El objetivo de este análisis es determinar la cantidad de huracanes que han
# ocurrido en cada océano y en cada año, así como también determinar la cantidad
# de huracanes que han ocurrido en cada mes y en cada año.


# Identificación de tres tipos de datos de tres columnas diferentes -------
# 1. Cantidad de huracanes por océano
class(huracanes$Ocean)
# 2. Cantidad de huracanes por año
class(huracanes$year)
# 3. Cantidad de huracanes por
class(huracanes$CO2)

# Preparación de los datos ----------------------------------------
huracanes$Fecha <- as.Date(huracanes$Fecha, format = "%m/%d/%Y")
unique(huracanes$Fecha)
# filtrar la nueva database a una nueva variable donde solo aya huracanes en tipo HU
huracanes_hu <- huracanes[huracanes$Status == " HU",]
View(huracanes_hu)
unique(huracanes_hu$Pressure)


# Obtención de cutriles ------------------------------------------------------------
# Cuartiles de la columna Wind
cuatrilWIND = quantile(huracanes$Wind, probs=c(0.25, 0.5, 0.75))
cuatrilWIND
# Cuartiles de la columna Pressure
cuatrilPress = quantile(huracanes$Pressure, probs=c(0.25, 0.5, 0.75))
cuatrilPress
# Cuartiles de la columna CO2
cuatrilCO2 = quantile(huracanes$CO2, probs=c(0.25, 0.5, 0.75))
cuatrilCO2
# Cuartiles de la columna population
cuatrilPop = quantile(huracanes$population, probs=c(0.25, 0.5, 0.75))
cuatrilPop
unique(huracanes$population)
