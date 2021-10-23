#Crear variable IMC
datos <- read.csv2("niños.csv") #Cargamos los datos 
datos <- datos[with(datos,order(datos$sexo,datos$edad,decreasing = FALSE)),]
datos$IMC <- datos$peso / (datos$talla / 100)^2 

#5.1 Tabla de frecuencias con individuos a partir de 18 años.

datos$Estado <- cut(datos$IMC,breaks = c(0,25,30,Inf), labels  = c("Normal","Sobrepeso","Obesidad"))

Tabla1 <- table( datos$Estado[datos$edad >= 18] )
Tabla2 <- prop.table(Tabla1)
Tabla3 <- cbind(Tabla1,Tabla2)
colnames(Tabla3) <- c("Frecuencia Absoluta", "Frecuencia Relativa")
Tabla3


#5.2 

datos$Categoría <- cut(datos$edad,breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), labels = c("(5-6]","(6-7]","(7-8]","(8-9]","(9-10]","(10-11]","(11-12]","(12-13]","(13-14]","(14-15]","(15-16]","(16-17]","(17-18]","(18-19]"))

Tabla4 <- cbind(tapply (datos$IMC,datos$Categoría, FUN = quantile,na.rm = TRUE,p = 0.85),tapply (datos$IMC,datos$Categoría, FUN = quantile,na.rm = TRUE,p = 0.95))

colnames(Tabla4) <- c("Sobrepeso","Obesidad")

Tabla4 <- as.matrix(Tabla4) 

names(dimnames(Tabla4)) <- c("Edades","")

Tabla4


#5.3

library(tidyverse)
A <-rownames_to_column(as.data.frame(Tabla4))
A$rowname <- factor(A$rowname,levels =  c("(5-6]","(6-7]","(7-8]","(8-9]","(9-10]","(10-11]","(11-12]","(12-13]","(13-14]","(14-15]","(15-16]","(16-17]","(17-18]","(18-19]") )

A %>%
  ggplot(aes(y=as.factor(A$rowname)))+
  geom_point(aes(x=A$Sobrepeso),color = "black",size = 3)+
  geom_point(aes(x=A$Obesidad),color = "red" , size = 3)+
  geom_vline(xintercept = 25,color ="black",size = 1.5 )+
  geom_vline(xintercept = 30, color = "red",size = 1.5)+
  theme_bw()+
  expand_limits(x = c(15,30))+
  ylab("Rango de Edades")+
  xlab("IMC")+
  coord_flip()

#Los puntos negros representan el percentil 85 del IMC para cada grupo de 
#edad, mientras que los rojos, representan el percentil 95 para cada grupo.
#La línea negra representa el nivel de IMC en el cual un adulto se considera
#que padece sobrepeso mientras que la rojo indica el nivel en el que un 
#adulto padece obesidad.

