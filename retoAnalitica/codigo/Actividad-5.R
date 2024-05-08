# TC1002S.201 - Equipo 3
# Actividad Evaluable 5: Análisis de datos
# Jesús Ángel Guzmán Ortega A01799257
# Julio Cesar Vivas Medina A01749879

# Cargamos librerías ---------------------------------------
library(corrplot)
library(factoextra)
library(ggplot2)

# Cargamos el dataset “mtcars” --------------------------------------------
data(mtcars)

# Análisis de K-means -----------------------------------------------------

# Escalado de datos
mtcars_s <- scale(mtcars)

# Instalamos el paquete "factoextra" para poder realizar el análisis de K-means
km2 <- kmeans(mtcars_s, 2)
km2

fviz_cluster(km2, data = mtcars_s, geom = "point", ellipse.type = "convex", ellipse = TRUE)

# K-means con 3 clusters
km3 <- kmeans(mtcars_s, 3)
km3

fviz_cluster(km3, data = mtcars_s, geom = "point", ellipse.type = "convex", ellipse = TRUE)

# K-means con 4 clusters
km4 <- kmeans(mtcars_s, 4)
km4

fviz_cluster(km4, data = mtcars_s, geom = "point", ellipse.type = "convex", ellipse = TRUE)
