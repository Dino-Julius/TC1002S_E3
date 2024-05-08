# TC1002S.201 - Equipo 3
# Actividad Evaluable 4: Análisis utilizando archivos CSV
# Jesús Ángel Guzmán Ortega A01799257
# Julio Cesar Vivas Medina A01749879

# Cambiar directorio de trabajo y cargar el archivo CSV --------------------

setwd("C:/Users/trato/Documents/oop2/TC1002S_E3/retoAnalitica/documentos/dataAct4")
tempByCountry <- read.csv("GlobalLandTemperaturesByCountry.csv")

# Verificar y tratar los valores nulos -----------------------------------

sapply(tempByCountry, function(x) sum(is.na(x)))
tempByCountry <- na.omit(tempByCountry)
summary(tempByCountry)

# Análisis General ------------------------------------------------

# Convertir la columna 'dt' a formato de fecha en R
tempByCountry$dt <- as.Date(tempByCountry$dt)

# Crear el gráfico de líneas para comparar la temperatura promedio a lo largo del tiempo
plot(tempByCountry$dt, tempByCountry$AverageTemperature, 
     type = "l", # Tipo de gráfico: líneas
     col = "purple", # Color de las líneas
     xlab = "Fecha", # Etiqueta del eje x
     ylab = "Temperatura Promedio", # Etiqueta del eje y
     main = "Temperatura Promedio Global a lo largo del tiempo" # Título del gráfico
)

# Análisis México ---------------------------------------------------------

# Filtrar los datos para México
tempMexico <- tempByCountry[tempByCountry$Country == "Mexico", ]

# Convertir la columna 'dt' a formato de fecha en R
tempMexico$dt <- as.Date(tempMexico$dt)

# Crear el gráfico de líneas para comparar la temperatura promedio a lo largo del tiempo
plot(tempMexico$dt, tempMexico$AverageTemperature, 
     type = "l", # Tipo de gráfico: líneas
     col = "blue", # Color de las líneas
     xlab = "Fecha", # Etiqueta del eje x
     ylab = "Temperatura Promedio", # Etiqueta del eje y
     main = "Temperatura Promedio en México a lo largo del tiempo" # Título del gráfico
)


# Análisis Japón ----------------------------------------------------------

# Filtrar los datos para Japón
tempJapon <- tempByCountry[tempByCountry$Country == "Japan", ]

# Convertir la columna 'dt' a formato de fecha en R
tempJapon$dt <- as.Date(tempJapon$dt)

# Crear el gráfico de líneas para comparar la temperatura promedio a lo largo del tiempo en Japón
plot(tempJapon$dt, tempJapon$AverageTemperature, 
     type = "l", # Tipo de gráfico: líneas
     col = "red", # Color de las líneas
     xlab = "Fecha", # Etiqueta del eje x
     ylab = "Temperatura Promedio", # Etiqueta del eje y
     main = "Temperatura Promedio en Japón a lo largo del tiempo" # Título del gráfico
)

# Análisis Mongolia -------------------------------------------------------

# Filtrar los datos para Mongolia
tempMongolia <- tempByCountry[tempByCountry$Country == "Mongolia", ]

# Convertir la columna 'dt' a formato de fecha en R
tempMongolia$dt <- as.Date(tempMongolia$dt)

# Crear el gráfico de líneas para comparar la temperatura promedio a lo largo del tiempo en Mongolia
plot(tempMongolia$dt, tempMongolia$AverageTemperature, 
     type = "l", # Tipo de gráfico: líneas
     col = "green", # Color de las líneas
     xlab = "Fecha", # Etiqueta del eje x
     ylab = "Temperatura Promedio", # Etiqueta del eje y
     main = "Temperatura Promedio en Mongolia a lo largo del tiempo" # Título del gráfico
)


