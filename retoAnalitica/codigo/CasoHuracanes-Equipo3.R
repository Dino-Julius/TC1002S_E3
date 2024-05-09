# TC1002S.201 - Equipo 3
# Rero: Caso Huracanes
# Jesús Ángel Guzmán Ortega A01799257
# Julio Cesar Vivas Medina A01749879


# Instalación de liberías -------------------------------------------------


# Liberías
library(dplyr)
library(ggplot2)
library(corrplot)
library(factoextra)
#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("maps")
#install.packages("mapdata")


# Cargamos los datos
#setwd("/cloud/project/retoAnalitica/codigo")
huracanes <- read.csv("CasoHuracanesCSV.csv")

# Filtrar los datos del Océano Pacífico Norte
huracanes_pacifico <- huracanes[huracanes$Ocean == "Pacific", ]

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

# 3.3.1 Eliminar los datos que no son reales (-999 en Wind y Pressure)
huracanes_pacifico <- huracanes_pacifico[!(huracanes_pacifico$Wind <= 0 | huracanes_pacifico$Pressure <= 0), ]

# 3.3.2 Eliminar los posibles NA
huracanes_pacifico <- na.omit(huracanes_pacifico)

# Filtrar huracanes_pacifico para incluir solo los registros donde el estatus sea "HU"
# huracanes_pacifico <- filter(huracanes_pacifico, Status == "HU")

# Realizar técnicas estadísticas ------------------------------------------

# 4.1 Muestra la MEDIA y el PROMEDIO de las variables : WIND y PRESSURE, CO2
media_wind <- mean(huracanes_pacifico$Wind)
media_wind

media_pressure <- mean(huracanes_pacifico$Pressure)
media_pressure

media_co2 <- mean(huracanes_pacifico$CO2)
media_co2

# 4.2 Indica los valores de los cuartiles de las variables : WIND, PRESSURE, CO2
cuartiles_wind
cuartiles_pressure
cuartiles_co2

# 4.3 Emite conclusiones
# Revisión de datos
str(huracanes_pacifico)
# View(huracanes_pacifico)

#5.- analisis por graficas

# 5.1.1 Gráfica la variable WIND
ggplot(huracanes_pacifico, aes(x = Wind)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribución de la Velocidad del Viento",
       x = "Velocidad del Viento",
       y = "Frecuencia") +
  theme_minimal()

# 5.1.2 Gráfica años con la variable WIND
ggplot(huracanes_pacifico, aes(x = year, y = Wind)) +
  geom_point(color = "blue") +
  labs(title = "Tendencia de la Velocidad del Viento por Año",
       x = "Año",
       y = "Velocidad del Viento") +
  theme_minimal()

# 5.1.3 Gráfica años con la variable Presión
ggplot(huracanes_pacifico, aes(x = year, y = Pressure)) +
  geom_point(color = "green") +
  labs(title = "Tendencia de la Presión Atmosférica por Año",
       x = "Año",
       y = "Presión Atmosférica") +
  theme_minimal()

# 5.1.4 Elabora gráficas sobre los datos CO2 y WIND
#Popukation en el oceano es 0 por lo que se omite

# Calcula los valores normalizados para CO2 y Wind
huracanes_pacifico$CO2_norm <- huracanes_pacifico$CO2 / max(huracanes_pacifico$CO2)
huracanes_pacifico$Wind_norm <- huracanes_pacifico$Wind / max(huracanes_pacifico$Wind)

# Gráfica usando ggplot2 con datos normalizados
ggplot(huracanes_pacifico, aes(x = year)) +
  geom_line(aes(y = CO2_norm, color = "CO2")) +
  geom_line(aes(y = Wind_norm, color = "WIND")) +
  labs(x = "Años", y = "Valor normalizado", title = "CO2 y WIND") +
  scale_color_manual(values = c("blue", "red"), labels = c("CO2", "WIND")) +
  theme_minimal()
#Análisis de datos

#6.1 Prepara los datos
data_for_cor <- huracanes_pacifico[, c("Wind", "Pressure", "CO2","year","Time","Longitud","Latitude")]
correlation_matrix <- cor(data_for_cor)

#6.2 Gráfica la correlación de los datos
corrplot(correlation_matrix, method = "ellipse")

#6.3 Aplica el algoritmo de k-means
km5 <- kmeans(data_for_cor, 5)
km5

#6.3.1 Gráfica los grupos obtenidos mediante el algoritmo de k-means
fviz_cluster(km5, data = data_for_cor, repel = TRUE, geom = "point", show.clust.cent = FALSE)

#6.4 Obtén la regresión lineal para 3 pares de variables a tu elección
lm_model1 <- lm(Wind ~ Pressure, data = huracanes_pacifico)
summary(lm_model1)

lm_model2 <- lm(CO2 ~ Pressure, data = huracanes_pacifico)
summary(lm_model2)

lm_model3 <- lm(CO2 ~ Wind, data = huracanes_pacifico)
summary(lm_model3)
# Gráfica 1: Wind ~ Pressure
plot(huracanes_pacifico$Pressure, huracanes_pacifico$Wind, 
     main = "Regresión Lineal: Wind ~ Pressure", 
     xlab = "Presión", 
     ylab = "Viento")
abline(lm_model1, col = "red")

# Gráfica 2: CO2 ~ Pressure
plot(huracanes_pacifico$Pressure, huracanes_pacifico$CO2, 
     main = "Regresión Lineal: CO2 ~ Pressure", 
     xlab = "Presión", 
     ylab = "CO2")
abline(lm_model2, col = "blue")

# Gráfica 3: CO2 ~ Wind
plot(huracanes_pacifico$Wind, huracanes_pacifico$CO2, 
     main = "Regresión Lineal: CO2 ~ Wind", 
     xlab = "Viento", 
     ylab = "CO2")
abline(lm_model3, col = "green")
