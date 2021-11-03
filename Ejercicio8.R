Integral <- function(f,a,b,n=NULL,alfa=0.05,conf.level=0.95,l=0.01) {
  M <- max((seq(a,b)^2)) #Cota máxima
  Q <- qnorm(1-alfa/2) #Qnorm
  n <- ((M*(b-a)*Q)/l)^2 #Número óptimo con el intervalo 0.01
  u <- runif(n,min=a,max=b) #Vector u[a,b]
  v <- runif(n,min=0,max=M) #Vector v[0,M]
  p <- mean(v <= u^2) # Proporción de valores de u menores que V
  integral <- p*M*(b-a) #Aproximación integral
  li= p*M*(b-a)-M*(b-a)*Q*sqrt(p*(1-p)/1000) #Límite inferior 
  ls= p*M*(b-a)+M*(b-a)*Q*sqrt(p*(1-p)/1000) #Límite superior
}


# En integraln , nosotros fijamos el n
Integraln <- function(f,a,b,n,alfa=0.05,conf.level=0.95,l=0.01) {
  M <- max((seq(a,b)^2)) #Cota máxima
  Q <- qnorm(1-alfa/2) #Qnorm
  u <- runif(n,min=a,max=b) #Vector u[a,b]
  v <- runif(n,min=0,max=M) #Vector v[0,M]
  p <- mean(v <= u^2) # Proporción de valores de u menores que V
  integral <- p*M*(b-a) #Aproximación integral
  li= p*M*(b-a)-M*(b-a)*Q*sqrt(p*(1-p)/1000) #Límite inferior 
  ls= p*M*(b-a)+M*(b-a)*Q*sqrt(p*(1-p)/1000) #Límite superior
}


Integraln(a=0,b=1,n=100) # n = 100
Integraln(a=0,b=1,n=500) # n = 500
Integraln(a=0,b=1,n=1000)# n = 1000
Integraln(a=0,b=1,n=5000)# n = 5000


# Resolución de integrales (último apartado)
f1=function(x)x**2
integrate(f1,0,1)
f2=function(x) exp(-0.5*x**2)
integrate(f2,0,1.5)
