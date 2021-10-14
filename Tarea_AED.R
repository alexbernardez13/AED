#Ejercicios análisis exploratorio de datos.

#################################EJERCICI0 1####################################
#1##############################################################################
x <- c(1,3,5,7,9)

x <- seq(from=1,to=9,by=2)

#2##############################################################################
xpares <- seq(from=10,to=44,by=2)

#3##############################################################################

set.seed(1)

z <- runif(n=20,min = -5,max = 5)

?runif

#3.1

z <- sort(z, decreasing = TRUE)

#3.2

z <- z[z >= 0]
print(z)

#4##############################################################################
x<-as.factor(c(1:7))

levels(x)<-c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")

#5##############################################################################

letters
?letters
count(letters)
#5.1############################################################################
input <- 1:26

multiplo4<-input[input %% 4 == 0]

letters[c(multiplo4)]
#5.2############################################################################
letters[-c(multiplo4)]

#5.3############################################################################
?sample

set.seed(1)
x <- sample(letters,100,replace = TRUE)

x <-as.factor(x)

table(x)

paste(sample(x,2,replace = TRUE))


#6##############################################################################

for (i in 1:100) {
  if (i^2 > 290) {
    print(i)
    print(i^2)
    break
  }
  }

#7##############################################################################
?which.max

c <- seq(from=0,to=1,by=0.01)

for (i in c) {
  x <- i - i^2
  values <- print(x)
}

which.max(values)




for(i in c) {
x <- i - i^2
if(x == which.max(x)) {
print(i)
print(x)
break
}
}

########################################EJERCICIO 2#############################

#1##############################################################################
?rnorm

set.seed(123)

x <- as.vector(rnorm(20,-1,2)) #Generar 20 números de una distribución normal 

is.vector(x)

x[c(seq(from=2,to=20,by=2))] #Mostrar los índices pares 

print(x[x>0]) #Mostrar los números positivos 

#2##############################################################################

x <- c(1:10) #Generamos secuencia de números del 1 al 10

?matrix # by row llena por filas. Si es falso lo hace por columnas.

matrix1 <- matrix(x,nrow=2,ncol=5,byrow=FALSE)
print(matrix1)

matrix2 <- matrix(x,nrow=2,ncol=5,byrow=TRUE)
print(matrix2)

matrix3 <- matrix(x,nrow=5,ncol=2,byrow=FALSE)
print(matrix3)

#3##############################################################################

for(i in 1:12) {
  x[i] <- i^2
  matriz <- matrix(x,nrow=3,ncol=4,byrow=TRUE)
}

print(matriz)

#4##############################################################################

mes <- c("enero","febrero","marzo","abril","mayo")
A <- c(7,5,2,6,7)
B <- c(5,2,0,1,4)

datos <- cbind(mes,A,B)
print(datos)
summary(datos)

datos <-as.data.frame(cbind(mes,A,B))
summary(datos)

?order
?with

Datos <- datos[with(datos, order(A,B,decreasing = TRUE)),]

Datos$VT <- as.numeric(Datos$A)+as.numeric(Datos$B)

Datos <- subset(Datos, Datos$VT > 6)

?write.csv2
write.csv(Datos,"C:/Users/92pab/Documents/ficheroventas.csv",col.names = TRUE) #Retocar csv. Borrar los números de la primera columna

#5##############################################################################

?mtcars
data <- mtcars
data$am <- as.factor(data$am)

table(data$am)

data$wtkg <- data$wt * 0.45362

data1 <- data[,c(1,2,3,11)]

data1 <- subset(data1, carb > 4)
