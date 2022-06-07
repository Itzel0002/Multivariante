# Análisis factorial matriz psych
#Descarga de paquetes y librerías 

install.packages("psych")

library(psych)

install.packages("polycor")

library(polycor)

install.packages("ggcorrplot")

library(ggcorrplot)

# Extracción de datos.
x<-Garcia

# Exploración de la matriz.
dim(x)

# Tipo de variables.
str(x)

#Nombre de las variables
colnames(x)

# Creación de una nueva matriz de datos donde se incluyen las variables 1 a la 25 y las primeras 200 observaciones.
x1<-Garcia[1:100,1:4]

# Matriz de correlaciones
R<-hetcor(x1)$correlations

# Gráfico de correlaciones
ggcorrplot(R, type = "lower", hc.order=TRUE)

#Factorización de la matriz de correlaciones

#Se utiliza la prueba de esfericidad de Bartlett.

p_Bartlett<-cortest.bartlett(R)


#Visualización del p-valor

p_Bartlett$p.value

#Ho: Las variables están correlacionadas.
#Ha: Las variables no están correlacionadas.

#No rechazo Ho, ya que las variables están correlacionadas.

# Criterio Kaiser-Meyer-Olkin
#Me permite identificar si los datos que voy a analizar son adecuados para un análisis factorial.

#0.00 a 0.49 No adecuados
#0.50 a 0.59 Poco adecuados
#0.60 a 0.69 Aceptables
#0.70 a 0.89 Buenos
#0.90 a 1.00 Excelentes 

KMO(R)

# Extracción de factores 

#minres: minimo residuo
#mle: max verosimilitud
#paf: ejes principales
#alpha: alfa
#minchi: mínimos cuadrados
#minrak: rango mínimo

modelo1<-fa(R,nfactor=3,rotate = "none",fm="mle")

modelo2<-fa(R,nfactor=3,rotate = "none",fm="minres")

#Extraer el resultado de la comunidalidades, , ahí se encuentra la proporción de varianza explicada. Se interpreta de tal forma que número cercanos a 1, el factor explica mejor la  variable.

C1<-sort(modelo1$communality, decreasing = TRUE)

C2<-sort(modelo2$communality, decreasing = TRUE)

head(cbind(C1,C2))

#Extracción de unidades
#La unicidad es el cuadro del coeficiente del factor único, y se expresa como la porción de la varianza explicada por el factor único. Es decir, no puede ser explicada por otros factores.

u1<-sort(modelo1$uniquenesses, decreasing = TRUE)

u2<-sort(modelo2$uniquenesses, decreasing = TRUE)

head(cbind(u1,u2))

scree(R)


# Rotación de la matriz

install.packages("GPArotation")
library(GPArotation)


rot<-c("None", "Varimax", "Quartimax", "Promax")
bi_mod<-function(tipo){
  biplot.psych(fa(x1, nfactors = 2,  
                  fm= "minres", rotate=tipo),
               main = paste("Biplot con rotación", tipo),
               col=c(2,3,4), pch=c(21,18), group=Garcia[,"protest"])
}
sapply(rot,bi_mod)

# Interpretación 
#Para esto se utiliza el fráfico de árbol.

modelo_varimax<-fa(R,nfactor=4,
                   rotate = "varimax",
                   fm="minres")

fa.diagram(modelo_varimax)

#Visualización de la matriz de carga rotada.

print(modelo_varimax$loadings, cut=0)