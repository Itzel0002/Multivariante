# Dendrograma
# Cargamos librerias
install.packages("cluster.datasets")
library("cluster.datasets")

# Bajamos la matriz de datos
data("all.mammals.milk.1956")

# Cambiamos el nombre de la matriz
AMM=all.mammals.milk.1956
head(AMM)

# Calculo de la matriz de distancia de Mahalonobis
dist.AMM<-dist(AMM[,2:6])

# Convertir los resultados del Calculo de la distancia a una matriz de datos y me indique 3 digitos.
round(as.matrix(dist.AMM)[1:6, 1:6],3)

# Calculo del dendrograma
dend.AMM<-as.dendrogram(hclust(dist.AMM))

# Generacion del dendrograma
plot(dend.AMM)

# Agregar etiquetas al Grafico
AMM.nombres=AMM
rownames(AMM.nombres)= AMM.nombres$name
AMM.nombres=AMM.nombres[,-1]

# Construimos de nuevo el Grafico
plot(as.dendrogram(hclust(dist(AMM.nombres))))

#  Modificar el dendrograma
install.packages("dendextend")
library(dendextend)

# Guardar las etiquetas en un objeto "L"
L=labels(dend.AMM)
labels(dend.AMM)=AMM$name[L]

# Cambiar el tamaño de las etiquetas
dend.AMM %>%
  set(what="labels_col", "red") %>% #Colores etiqueta
  set(what="labels_cex", 0.8) %>%
  plot(main="Dendrograma de Mamiferos")

# Dendograma de Circulo
install.packages("circlize")
library("circlize")

circlize_dendrogram(dend.AMM,labels_track_height=NA,
                    dend_track_height=0.1)