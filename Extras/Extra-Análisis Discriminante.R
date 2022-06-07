# ANALISIS DISCRIMINANTE LINEA
library(MASS)

# Se cargan los datos mtautos
Z<-as.data.frame(mtautos)

# Se define la matriz de datos y la variable
x<-Z[,1:4]
y<-Z[,5]

# Definir como n y p 
n<-nrow(x)
p<-ncol(x)

# Se aplica el Analisis discriminante lineal (LDA)
# Cross validation (cv): clasificacion optima
lda.mtautos<-lda(y~.,data=x, CV=TRUE)

# lda.mtautos$class contiene las clasificaciones hechas por CV usando LDA.
lda.mtautos$class


# Creacion de la tabla de clasificaciones buenas y malas
table.lda<-table(y,lda.mtautos$class)
table.lda

# Proporcion de errores
mis.lda<- n-sum(y==lda.mtautos$class)
mis.lda/n

# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.mtautos<-c("indianred1","black")[1*(y==lda.mtautos$class)+1]
pairs(x,main="Buena Clasificacion (negro), Mala Clasificacion (rojo)",
      pch=19,col=col.lda.mtautos)


# Probabilidad de pertenencia a uno de los tres grupos
lda.mtautos$posterior


# Grafico de probabilidades
plot(1:n, lda.mtautos$posterior[,1],
     main="Probabilidades a posterior",
     pch=20, col="cyan",
     xlab="Numero de observaciones", ylab="Probabilidades")
points(1:n,lda.mtautos$posterior[,2],
       pch=20, col="green")
points(1:n,lda.mtautos$posterior[,3],
       pch=20, col="orange")