##! Script: "estadMas.r"                                         /
##- Sobre:  Calculo estadigrafos muestreo aleatorio simple      /
##+ Detalles: Para la variable volumen (m3/ha)                 /
##* Ejemplo: Datos de poblacion de juego                      /
##? Mas detalles: El script se organiza como sigue:          /
##  1. Parte seleccionando las variables de los elementos   /
## muestrales                        /
##  2. Con esos datos desarrolla los calculos  /
##! ---------------------------------------------------/ 
##                                                    /
##> Profesor: Christian Salas Eljatib                /
##? E-mail: christian.salas AT uchile DOT cl        /
## Web: https://eljatib.com                        /
##!===============================================/

#seleccionando la muestra de 12 elementos desde la poblacion
# dada en clases
sample(1:400,12)
y<-c(177,271,130,159,59,124,0,247,294,183,277,106)
length(y)
y
mean(y)
m.y<-mean(y)
m.y
sum(y)/length(y)
n<-length(y)
n
sum(y)/n
2**2
sum((y-m.y)^2)/(n-1)
var(y)
v.y<-var(y)
m.y
v.y
sqrt(v.y)
s.y<-sd(y)
100*s.y/m.y
N<-400
n
v.y
f<-n/N
f
(1-f)
sqrt( (v.y/n) * (1-f) )
se.my<-sqrt( (v.y/n) * (1-f) )
se.my
100*se.my/m.y

##valor de t
df <- n - 1
conf <- 90
alpha <- 1 - (conf/100)
alpha.2 <- alpha/2
t.value <- abs(qt(1 - alpha.2,df))
t.value

#error de muestreo
se.my*t.value
e.m <-se.my*t.value
#error de muestreo en %
100*e.m/m.y

#intervalo de confianza
lim.inf <- m.y-e.m
lim.inf
lim.sup <- m.y+e.m
c(lim.inf,lim.sup)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#*´¨)
#¸.•´¸.•*´¨) ¸.•*¨)
#(¸.•´ (¸.•` ¤ Fin del script
