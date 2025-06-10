acciones <- read.csv("/cloud/project/datasets/acciones.csv")

#Es una serie de tiempo. 
#Nuestro objetivo es visualizar la reduccion de dimesion a s=2 mediante componentes principales PCA

X <- as.matrix(100*acciones)

pairs(acciones, pch=20) # Todas las combinaciones lineales deberian tener forma de pelota
#En general se ven que los datos son lindos, no tienen cosas raras
#Conviene que los datos sean lindos (normal multivariados) porque 

# -- Paso 1) Obtenemos matriz de covarianzas
sigma_hat <- cov(X)

# -- Paso 2) Obtenemos los autovectores y los autovalores
eig <- eigen(sigma_hat) #Nos tira los autovalores y autovectores.
# Un autovector son la direccion. El primero es la mejor direccion. El segundo 
# es el mejor ortogonal a la primera, y asi sucesivamente

#Queremos reducir a dos dimesiones
V <- eig$vectors[,1:2] #Solo nos tira los vectores.  Sacamos los primeros 2 porque queremos reducir a 2 dimensiones
lambda <- diag(eig$values[1:2]) #Son los autovalores que son las varianzas de las nuevas variables

# -- Paso 3) Obtener los 'escores'
Z <- X %*% V 
plot(Z, pch=20, xlab='Z1', ylab='Z2', xlim = c(-10,10),
     ylim = c(-10,10)) #Visualizamos quien es distinto de quien, es solo para ver que onda

# Analisis
cov(Z) #Es matriz diagonal
plot(eig$values, type = "h", lwd = "5") #Muestra las varianzas rankeadas.  Si la reduccion de 2 dimensiones es buena las primeras dos columnas suman mucho. 
#Calculamos el R-cuadrado
sum(eig$values[1:2])/sum(eig$values) #Como R-cuadrado nos da mayor a 0.8 entonces el modelo de 2 dimensiones sirve.

V #la primera variable describe el rendimiento general del mercado.
#La primera columna es el primer autovector con mayor varianza (Z1 = 0.22x1 + 0.30x2 + 0.15x3 + 0.63x4 + 0.65x5)
#La segunda columna es el segundo autovector (Z2)

# Ver como esta hecho cada eje - Armamos biplot
# Poder visualizar los puntos y todo junto.  Una forma para poder visualizar el resultado de un PCA.
# Proyecta los vectores canonicos la transformacion que le hicimos a los datos.
plot(Z, pch=20, xlab='Z1', ylab='Z2', xlim = c(-10,10),
     ylim = c(-10,10))

for(j in 1:5){
  segments(0,0,3*V[j,1], 3*V[j,2])
  arrows(0,0,3*V[j,1], 3*V[j,2], lenght = 0.1) #Cada flecha representa una variable
}

# Los ejes son los componentes. Para el lado que esten direccionados, implica
# cuanto explica ese componente a ese vector (por ejemplo, el componente 2 
# explica los bancos asi que su vector direccionado apunta para arriba y contiene a lo puntos
# superiores)
#Z1 representa el rendimiento del mercado
#Z2 el contraste entre banco y petrolera


#----Dataset de paises
source('Librerias y Funciones.R')

# Ejercicio paises mundo
paises_mundo <- read.csv("/cloud/project/datasets/paises_mundo.csv")

nombres <- paises_mundo$Nombre
data <- paises_mundo[,-1]

# Componentes principales (reducción a dos dimensiones)
pca <- prcomp(data)

# Somos grupo 3, por lo que tenemos que aplicar logaritmo y estandarizar
dataLog <- log(data)
dataLogEstandarizada <- scale(dataLog)

X <- as.matrix(dataLogEstandarizada)
pairs(dataLogEstandarizada, pch=20) 

# Tres variables:
# 1 - Datos originales,

# -- Paso 1) Obtenemos matriz de covarianzas
sigma_hat <- cov(X)

# -- Paso 2) Obtenemos los autovectores y los autovalores
eig <- eigen(sigma_hat)

#Queremos reducir a dos dimesiones
V <- eig$vectors[,1:2]  # Matriz de rotacion
lambda <- diag(eig$values[1:2])

# -- Paso 3) Obtener los 'escores'
Z <- X %*% V 
plot(Z, pch=20, xlab='Z1', ylab='Z2', xlim = c(-4,5),
     ylim = c(-4,4))
sum(eig$values[1:2])/sum(eig$values) # R-cuadrado nos da mayor a 0.8

# Proyecta los vectores canonicos la transformacion que le hicimos a los datos.
O <- cbind(rep(0,5), rep(0,5))
D <- 3*cbind(V[,1], V[,2])

origen_1 <- O[,1]
origen_2 <- O[,2]
destino_1 <- D[,1]
destino_2 <- D[,2]

segments(origen_1,origen_2,destino_1,destino_2)
arrows(origen_1,origen_2,destino_1,destino_2, length = 0.1, col = 'firebrick', lwd=2)
text(D, colnames(dataLogEstandarizada), offset=3, col='firebrick')
text(Z, nombres, cex = 0.7, col='grey50')

heatmap(cor(dataLogEstandarizada, Z), Rowv = NA, Colv = NA)
cor_heatmap(cor(dataLogEstandarizada, Z))

# A veces hacer reduccion antes de hacer clustering esta bueno
# 
# Todo esto puede ser reemplazado por la funcion biplot
biplot(pca)

