# TC1002S.201 - Equipo 3
# Actividad Evaluable 5: Análisis de datos
# Jesús Ángel Guzmán Ortega A01799257
# Julio Cesar Vivas Medina A01749879

# Cargamos librerías ---------------------------------------
library(corrplot)
library(factoextra)
library(ggplot2)

# Preparación de datos ----------------------------------------------------
# Cargamos el dataset
data(mtcars)

# Matriz de correlación
correlation <- cor(mtcars)

# Escalado de datos
mtcars_s <- scale(mtcars)

# Análisis de Correlación -------------------------------------------------
corrplot(correlation, method = "ellipse")

# Análisis de K-means ----------------------------------------------------
km2 <- kmeans(mtcars_s, 2)
km2

fviz_cluster(km2, data = mtcars_s, repel = TRUE)

# K-means con 3 clusters
km3 <- kmeans(mtcars_s, 3)
km3

fviz_cluster(km3, data = mtcars_s, repel = TRUE)

# K-means con 4 clusters
km4 <- kmeans(mtcars_s, 4)
km4

fviz_cluster(km4, data = mtcars_s, repel = TRUE)