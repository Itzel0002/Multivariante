# Cargamos librerias
install.packages("cluster.datasets")
library("cluster.datasets")

# Cargamos librerias
library("cluster")

#Importar la matriz de datos
library(readxl)
top50 <- top50_2021 <- read_excel("Estadistica multivarida/top50-2021.xlsx")
View(top50_2021)

# Cambiamos el nombre de la matriz
S=top50

# Exploración de la base de datos
dim(S)
colnames(S)
str(S)
anyNA(S)

head(S)

# Cálculo de la matriz de distancia de Mahalonobis
#Solamente a las variables de la 2 a la 6
# ya que la primera es el nombre de las canciones 
dist.S<-dist(S[,2:6])
dist.S

# Convertir los resultados del Calculo de la distancia a una matriz de datos y me indique 3 digitos.
round(as.matrix(dist.S)[1:6, 1:6],3)

# Calculo del dendrograma
dend.S<-as.dendrogram(hclust(dist.S))
dend.S

#hclust me dice que es un cluster jerárquico
# y los argumentos dice que tiene que se una
# estructura de disimilitud, ya que no podemos
#trabajar con los datos originales, tenemos
#que calcular las distancias, por eso usamos
#mahalanobis
help(hclust)

# Generacion del dendrograma
library(dendextend)

# Guardar las etiquetas en un objeto "L"
L=labels(dend.S)
labels(dend.S)=S$track_name[L]

install.packages(factoextra)
library(factoextra)
library(ggplot2)

# Cambiar el tamaño de las etiquetas
dend.S %>%
  set(what="labels_cex", 0.1) %>%
  plot(main="Dendrograma Top 50 spotify 2020")
fviz_dend(dend.S,
          k=3, k_colors = c("#1B9E77", "#7570B3", "#E7298A"), main = "Dendrograma Top 50 spotify 2021")

