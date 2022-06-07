# Ejercicio del Calvo

# Cargar los datos
ventas= c( 1054, 1057, 1058, 1060, 1061, 1060, 1061, 1062, 1062, 1064, 1062, 1062, 1064, 1056, 1066, 1070)
clientes= c(63, 66, 68, 69, 68, 71, 70, 70, 71, 72, 72, 73, 73, 75, 76, 78)

# Utilizamos la función data.frame() para crear un juego de datos en R
datos <- data.frame(ventas ,clientes)

dim(datos)
str(datos)
summary(datos)

#Determinar el numero de outlier que queremos encontrar
num.outliers<-2

# Ordenar los datos de mayor a menor distancia, según la métrica de Mahalanobis.
mah.ordenacion <- order(mahalanobis(datos , colMeans( datos), cov(datos)), decreasing=TRUE)
mah.ordenacion

# Generar un vector boleano los dos valores más alejados segun la distancia Mahalanobis.
outlier2 <- rep(FALSE , nrow(datos))
outlier2[mah.ordenacion[1:num.outliers]] <- TRUE

# Resaltar con un punto relleno los 2 valores outliers.
colorear.outlier <- outlier2 * 16

# Visualizar el gráfico con los datos destacando sus outlier.
plot(datos , pch=0)
points(datos , pch=colorear.outlier)


## 2° Ejercicio

require(graphics)

ma <- cbind(1:6, 1:3)
(S <-  var(ma))
mahalanobis(c(0, 0), 1:2, S)

x <- matrix(rnorm(100*3), ncol = 3)
stopifnot(mahalanobis(x, 0, diag(ncol(x))) == rowSums(x*x))
##- Here, D^2 = usual squared Euclidean distances

Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx)
plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, n=100, p=3") ; rug(D2)
qqplot(qchisq(ppoints(100), df = 3), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'cyan')


# Ejercicio 3 con diseño propio 

# Cargar la libreria

#install.packages("datos")
library(datos)

#Cargamos la base de datos
M<-data.frame(datos::fiel)

#Utilizamos la función data.frame() para crear un juego de datos en R
M<- data.frame(M)

dim(M)
str(M)
summary(M)

#Determinar el número de outlier que queremos encontrar
num.outliers<-2

#Ordenar los datos de mayor a menor distancia, según la métrica de
Mahalanobis.
mah.ordenacion <- order(mahalanobis(M , colMeans( M), cov(M)), decreasing=TRUE)

#Generar un vector boleano los dos valores más alejados según la
#distancia Mahalanobis.
outlier2 <- rep(FALSE , nrow(M))
outlier2[mah.ordenacion[1:num.outliers]] <- TRUE

#Resaltar con un punto relleno los 2 valores outliers.
colorear.outlier <- outlier2 * 16

#Visualizar el gráfico con los datos destacando sus outlier.
plot(M , pch=0)
points(M , pch=colorear.outlier)


#Cargar la libreria

#install.packages("datos")
library(datos)

#Cargamos la base de datos
M<-data.frame(datos::fiel)

#Utilizamos la función data.frame() para crear un juego de datos en R
M<- data.frame(M)

dim(M)
str(M)
summary(M)

#Determinar el número de outlier que queremos encontrar
num.outliers<-2

#Ordenar los datos de mayor a menor distancia, según la métrica de
#Mahalanobis.
mah.ordenacion <- order(mahalanobis(M , colMeans( M), cov(M)), decreasing=TRUE)

#Generar un vector boleano los dos valores más alejados según la distancia Mahalanobis.
outlier2 <- rep(FALSE , nrow(M))
outlier2[mah.ordenacion[1:num.outliers]] <- TRUE

#Resaltar con un punto relleno los 2 valores outliers.
colorear.outlier <- outlier2 * 16

#Visualizar el gráfico con los datos destacando sus outlier.
plot(M , pch=0)
points(M , pch=colorear.outlier)
