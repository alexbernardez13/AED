#Crear variable IMC
datos <- read.csv2("niños.csv") #Cargamos los datos 
datos <- datos[with(datos,order(datos$sexo,datos$edad,decreasing = FALSE)),]
datos$IMC <- datos$peso / (datos$talla / 100)^2 

#5.1 Tabla de frecuencias con individuos a partir de 18 años.

datos$Estado <- cut(datos$IMC,breaks = c(0,25,30,Inf), labels  = c("Normal","Sobrepeso","Obesidad"))

tabla1 <- table( datos$Estado[datos$edad >= 18] )
tabla2 <- prop.table(tabla1)
tabla3 <- cbind(tabla1,tabla2)
colnames(tabla3) <- c("Frecuencia Absoluta", "Frecuencia Relativa")
tabla3


#5.2 

datos$Categoría <- cut(datos$edad,breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), labels = c("(5-6]","(6-7]","(7-8]","(8-9]","(9-10]","(10-11]","(11-12]","(12-13]","(13-14]","(14-15]","(15-16]","(16-17]","(17-18]","(18-19]"))

Tabla4 <- cbind(tapply (datos$IMC,datos$Categoría, FUN = quantile,na.rm = TRUE,p = 0.85),tapply (datos$IMC,datos$Categoría, FUN = quantile,na.rm = TRUE,p = 0.95))

colnames(Tabla4) <- c("Sobrepeso","Obesidad")

Tabla4 <- as.matrix(Tabla4) 

names(dimnames(Tabla4)) <- c("Edades","")

Tabla4


#5.3

library(tidyverse)
library(ggalt)

A <-rownames_to_column(as.data.frame(Tabla4))
A$rowname <- factor(A$rowname,levels =  c("(5-6]","(6-7]","(7-8]","(8-9]","(9-10]","(10-11]","(11-12]","(12-13]","(13-14]","(14-15]","(15-16]","(16-17]","(17-18]","(18-19]") )

A %>% 
  ggplot() +
  geom_dumbbell(aes(x=Sobrepeso, xend=Obesidad, y=rowname),size_x = 3,size_xend = 3,colour_x = "black",colour_xend = "red") +
  ylab("Rango de Edades")+
  xlab("IMC")+
  coord_flip()+
  geom_vline(xintercept = 25,color ="black",size = 2 )+
  geom_vline(xintercept = 30, color = "red",size = 2)+
  expand_limits(x = c(15,30))

 
  









################################################################################
#                             BORRADORES                                       #
################################################################################
#          Se usó un loop para realizar la clasificación
################################################################################
datos$Estado <- 0

for(i in 1:length(datos$IMC)) {  
  if (datos$IMC[i] < 25) {
    datos$Estado[i] <- "Normal"
  } else { if (datos$IMC[i] >= 25 & (datos$IMC[i] < 30)) {
    datos$Estado[i] <- "Sobrepeso"
  } else {
    datos$Estado[i] <- "Obesidad"
  }
  }
}

table(datos$Estado[datos$edad > 18])




################################################################################
datos$Categoría <- 0

for(i in 1:length(datos$edad)) {
  if (datos$edad[i] > 5 & datos$edad[i] <= 6) {
    datos$Categoría[i] <- "5-6"
  } else { if (datos$edad[i] > 6 & datos$edad[i] <= 7) {
    datos$Categoría[i] <- "6-7"
  } else { if (datos$edad[i] > 7 & datos$edad[i] <= 8) {
    datos$Categoría[i] <- "7-8"
  } else { if (datos$edad[i] > 8 & datos$edad[i] <= 9) {
    datos$Categoría[i] <- "8-9"
  } else { if (datos$edad[i] > 9 & datos$edad[i] <= 10){
    datos$Categoría[i] <- "9-10"
  } else { if (datos$edad[i] > 10 & datos$edad[i] <= 11) {
    datos$Categoría[i] <- "10-11"
  } else { if (datos$edad[i] > 11 & datos$edad[i] <= 12) {
    datos$Categoría[i] <- "11-12"
  } else { if (datos$edad[i] > 12 & datos$edad[i] <= 13) {
    datos$Categoría[i] <- "12-13"
  } else { if (datos$edad[i] > 13 & datos$edad[i] <= 14) {
    datos$Categoría[i] <- "13-14"
  } else { if (datos$edad[i] > 14 & datos$edad[i] <= 15) {
    datos$Categoría[i] <- "14-15"
  } else { if (datos$edad[i] > 15 & datos$edad[i] <= 16) {
   datos$Categoría[i] <- "15-16"
  } else { if (datos$edad[i] > 16 & datos$edad[i] <= 17) {
   datos$Categoría[i] <- "16-17"
  } else { if (datos$edad[i] > 17 & datos$edad[i] <= 18) {
   datos$Categoría[i] <- "17-18"
  } else { if (datos$edad[i] > 18 & datos$edad[i] <= 19) {
   datos$Categoría[i] <- "18-19"
 }
 }
 }
 }
 }
 }
 }
 }
 }
 }
 }
 }
 }
 }  
 }

?prop.table
?addmargins


dotchart(Tabla4$Sobrepeso,labels = row.names(Tabla4))
points(Tabla4$Obesidad,1:row(Tabla4))



dotchart(Tabla4$Sobrepeso, pch = 21, labels = row.names(Tabla4), bg = "green",pt.cex = 1.5, xlim = range(Tabla4$Sobrepeso, Tabla4$Obesidad) + c(-2, 2))
points(Tabla4$Obesidad, 1:nrow(Tabla4), col = "red", pch = 19, cex = 1.5)
