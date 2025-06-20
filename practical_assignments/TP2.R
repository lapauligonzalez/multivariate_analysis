library(smacof)
library(dplyr)
library(ggplot2)
library(heatmaply)
library(reshape2)
source('Librerias y Funciones.R')

set.seed(13)
spotify <- read.csv("/cloud/project/dataset.csv")
data <- spotify[sample(nrow(spotify), 100), ] %>%
  select('popularity', 'energy', 'danceability', 'time_signature', 'mode')

p <- ncol(data)
w <- rep(1,p)

data$time_signature <- as.factor(data$time_signature)
data$mode <- as.factor(data$mode)

# En esta versión modificada, para variables continuas, 
# se utiliza directamente la distancia euclidiana normalizada 
# por el rango de la variable, eliminando la conversión a distancia Gower. 
# Solo se aplica la distancia Gower para las variables categóricas, 
# donde se compara la igualdad entre categorías.

distancia_gower_modificada <- function(data, w){
  n <- nrow(data)
  p <- ncol(data)
  dist <- matrix(0, n, n) # Creo la matriz vacia
  ranges <- sapply(data, function(x) if (!is.factor(x)) diff(range(x)) else NA)
  
  for(i in 1:n){
    for(j in 1:n){ # Porque quiero crear la matriz n x n que compara las distancias entre todos
      a <- 0
      for(k in 1:p){ # Porque tengo que ir por cada variable de la tabla
        if (is.factor(data[, k])){ # Si la variable es categórica
          s <- as.numeric((data[i, k] == data[j, k]))
        } else { # Si la variable es numérica
          s <- 1 - abs(data[i, k] - data[j, k])/ranges[k]
        }
        a <- a + w[k] * s
      }
      dist[i, j] = a / sum(w) # Cada elemento de la matriz es Dij
    }
  }
  
  return(dist)
}

delta <- distancia_gower_modificada(data, w=rep(1,p))
mds <- cmdscale(as.dist(delta))
plot(mds, pch = 20, col = 'black', cex = 1.5, xlim = c(-1, 1), ylim = c(-1, 1))
text(mds,rownames(data))

mds_data <- data.frame(Dimension1 = mds[, 1], Dimension2 = mds[, 2], Nombre = rownames(data))
ggplot(mds_data, aes(x = Dimension1, y = Dimension2, label = Nombre)) +
  geom_point(shape = 20, color = 'black', size = 1.5) +
  geom_text(vjust = -0.5) +
  xlim(-0.05, 0.05) + ylim(-0.6, 0.6) +
  labs(x = "Dimension 1", y = "Dimension 2", title = "MDS en dos dimensiones")+theme(
    text = element_text(family = "mono"),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
    panel.grid.major = element_line(color = 'grey'),
    panel.grid.minor = element_line(color = 'grey'),
    axis.ticks.x = element_line(color = 'grey', size = 0.5),
    axis.text.x = element_text(size = 10)
  )

mdsConPeso <- cmdscale(as.dist(distancia_gower_modificada(data, w=c(100,50,1,1,1)))) #Quiero ver que pasa si le pongo mucho peso a la variable de IsActive
text(mdsConPeso,rownames(data),col='firebrick') #Los colores marcan los pesos cambiados

mds_data <- data.frame(Dimension1 = mdsConPeso[, 1], Dimension2 = mdsConPeso[, 2], Nombre = rownames(data))
ggplot(mds_data, aes(x = Dimension1, y = Dimension2, label = Nombre)) +
  geom_point(shape = 20, color = 'firebrick', size = 1.5) +
  geom_text(vjust = -0.5, color = 'firebrick') +
  xlim(-0.6, 0.6) + ylim(-0.6, 0.6) +
  labs(x = "Dimension 1", y = "Dimension 2", title = "MDS en dos dimensiones con pesos")+theme(
    text = element_text(family = "mono"),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
    panel.grid.major = element_line(color = 'grey'),
    panel.grid.minor = element_line(color = 'grey'),
    axis.ticks.x = element_line(color = 'grey', size = 0.5),
    axis.text.x = element_text(size = 10)
  )

stress_kruskal <- smacofSym(delta, ndim = 3, init = "random")$stress

plot3d(cmdscale(as.dist(delta),k=3),
       cube=FALSE)
plot3d(cmdscale(as.dist(distancia_gower_modificada(data, w=c(100,50,1,1,1))),k=3),
       cube=FALSE)


# Matriz de coordenadas y correlacion
datosConDummies <- data %>%
  mutate(time_signature4 = as.integer(time_signature == 3),
         time_signature5 = as.integer(time_signature == 4),
         time_signature7 = as.integer(time_signature == 5)) %>%
  select(-time_signature)
datosConDummies$mode <- as.numeric(datosConDummies$mode)

# Visualizar el heatmap de correlaciones

p <- ncol(datosConDummies)
deltaTransformado <- distancia_gower_modificada(datosConDummies, w=rep(1,p))
coordenadasMDS <- cmdscale(as.dist(deltaTransformado))
matriz_correlaciones <- cor(deltaTransformado, coordenadasMDS)

coordenadasMDS_3D <- cmdscale(as.dist(deltaTransformado), k = 3)
matriz_correlaciones_3D <- cor(deltaTransformado, coordenadasMDS_3D)

deltaMDS <- 1 - cov(datosConDummies)

mds_1 <- cmdscale(deltaMDS, k = 2, eig = TRUE)
plot(mds_1$points, col='green4', lwd=4, xlab= 'coord1', ylab = 'coord2', xlim = c(-0.6, 0.6), ylim = c(-0.4, 0.4))
text(mds_1$points, colnames(datosConDummies), pos = 2,  cex = 0.7)
mds <- mds(deltaMDS, type = 'interval')

boot <- bootmds(mds, deltaMDS)
plot(boot, col='steelblue')

#Biplot con MDS
plot(mds_1$points, col = 'red4', lwd = 4, xlab = 'coord1', ylab = 'coord2', xlim = c(-0.6, 0.6), ylim = c(-0.4, 0.4))
text(mds_1$points, colnames(datosConDummies), pos = 2, cex = 0.7)
loadings <- cor(datosConDummies) * sqrt(mds_1$eig[1])
arrows(0, 0, loadings[, 1], loadings[, 2], length = 0.1, col = 'pink3')