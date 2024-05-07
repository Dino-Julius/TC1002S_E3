# TC1002S.201 - Equipo 3
# Actividad Evaluable 3: Análisis con Vectores
# Jesús Ángel Guzmán Ortega A01799257
# Julio Cesar Vivas Medina A01749879

# Análisis --------------------------------------------------------------

seguidores <- c(327, 648,  325, 436, 124, 651, 547, 235, 190, 182, 100, 37, 519)

sum(seguidores)

length(seguidores)

min(seguidores)
max(seguidores)

mean(seguidores)
median(seguidores)
var(seguidores)
sd(seguidores)

# install.packages("modeest")
# library(modeest)
# mfv(seguidores)

summary(seguidores)

# Graficos ----------------------------------------------------------------

sorted_seguidores <- sort(seguidores)

plot(sorted_seguidores)
hist(sorted_seguidores,col="blue")
boxplot(sorted_seguidores, col="red")

# Cuartiles ---------------------------------------------------------------

cuartil = quantile(seguidores, probs=c(0.25, 0.5, 0.75))
print(cuartil)