# Análisis de Componentes Principales 

## Introducción
#El Análisis de componentes principales (**ACP**) es un método de reducción de la dimensionalidad de las variables originales.


## Matriz de trabajo
#1.- Se trabajó con la matriz flores, extraída del paquete **datos** que se encuentra precargado en R.

install.packages("datos")
library(datos)

#2.- Se selecciona la matriz flores
flo<-datos::flores

## Exploración de la matriz 

#1.- Dimensión de la matriz.
#La matriz cuenta con 150 observaciones y 5 variables.
dim(flo)

#2.- Tipo de variables.
str(flo)

#3.- Nombre de las variables.
colnames(flo)

#4.- En busca de datos perdidos.
anyNA(flo)

## Tratamiento de la matriz 
#Se genera una nueva matriz **flor** filtrada.

#2.- Selección de las variables cuantitativas
#de la especie versicolor.
flor<-flo[51:100,1:4]

# ACP paso a paso

#1.- Transformar la matriz en un data frame

flo<-as.data.frame(flor)

#2.- Definir _n_ (individuos) y _p_ (variables).
```{r}
n<-dim(flo)[1]
p<-dim(flo)[2]
```

#3.- Generación del gráfico **scaterplot**.
pairs(flor,col="chartreuse", pch=19, 
      main="Variables originales")

#4.- Obtención de la media por columna y la matriz de covarianza muestral.
mu<-colMeans(flor)
mu

s<-cov(flor)
s

#5.- Obtención de los valores y vectores propios desde la matriz de covarianza muestral.
es<-eigen(s)
es

#5.1.- Separación de la matriz de valores propios.
eigen.val<-es$values
eigen.val

#5.2.- Separación de la matriz de vectores propios.
eigen.vec<-es$vectors
eigen.vec

#6.- Calcular la proporción de variabilidad

#6.1.- Para la matriz de valores propios.
pro.var<-eigen.val/sum(eigen.val)
pro.var

#6.2.- Acumulada.
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)
pro.var.acum

#7.- Obtención de la matriz de correlaciones
R<-cor(flor)
R

#8.- Obtención de los valores y vectores propios a partir de la **matriz de correlaciones**.
eR<-eigen(R)
eR

#9.- Separación de la matriz de valores propios a partir de la matriz de correlaciones.

#9.1.- Separación de la matriz de vectores propios.
eigen.val.R<-eR$values
eigen.val.R

#9.2.- Separación de la matriz de vectores propios.
eigen.vec.R<-eR$vectors
eigen.vec.R

#10.- Cálculo de la proporción de variabilidad.

#10.1.- Para la matriz de valores propios.
pro.var.R<-eigen.val.R/sum(eigen.val.R)
pro.var.R

#10.2.- Acumulada.
#En este punto se seleccionan en número de componentes, siguiendo el criterio del 80% de la varianza explicada.
#Para este ejemplo se van a seleccionar 2 factores (0.868% de varianza explicada).

pro.var.acum.R<-cumsum(eigen.val.R)/sum(eigen.val.R)
pro.var.acum.R

#11.- Calcular la media de los valores propios.
mean(eigen.val.R)

## Obtención de coeficientes

#12.- Centrar los datos con respecto a la media
#12.1.- Construcción de matriz de 1
ones<-matrix(rep(1,n),nrow=n, ncol=1)

#12.2.- Construcción de la matriz centrada
X.cen<-as.matrix(flor)-ones%*%mu

#13.- Construcción de la matriz diagonal de las covarianzas
Dx<-diag(diag(s))
Dx

#14.- Construcción de la matriz centrada multiplicada por Dx^1/2 
Y<-X.cen%*%solve(Dx)^(1/2)

#15.- Construccion de los coeficientes o scores eigen.vec.R matriz de autovectores
#Se muestran las primeras 10 observaciones.
scores<-Y%*%eigen.vec.R
scores[1:10,]

#16.- Nombramos las columnas PC1...PC8
colnames(scores)<-c("PC1","PC2","PC3","PC4")

#17.- Visualización de los scores
scores[1:10,]

#18.-Generacion del grafico de los scores
pairs(scores, main="scores", col="blue", pch=19)

# ACP VIA SINTETIZADA

#1.- Cálculo de la varianza a las columnas (1=filas, 2=columnas)
apply(flor, 2, var)

#2.- Aplicar la función **prcomp** para reducir la dimensionalidad y centrado por la media y escalada por la desviación estandar (dividir entre sd).

acp<-prcomp(flor, center = TRUE, scale=TRUE)
acp

#3.- Generación del gráfico **screeploy**.
plot(acp, type="l")