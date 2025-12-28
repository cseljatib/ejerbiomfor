##! Script: "varestado6.r"                                       /
##- Sobre:  Calculo de variables de estado--de orden            /
##+ Detalles: Para la variable diametro                        /
##* Ejemplo: Datos de una parcela de muestreo (data=eucaplot2)/
##? Mas detalles: El script se organiza como sigue:          /
##  1. Calculo de variables (de estado) agregadas.          /
##  2. Calculo de variables (de estado) de orden para      /
## el diametro.                                           /
##! -----------------------------------------------------/ 
##                                                      /
##> Profesor: Christian Salas Eljatib                  /
##? E-mail: christian.salas AT uchile DOT cl          /
## Web: https://eljatib.com                          /
##!=================================================/

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos a emplear
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
data(eucaplot2)
##?eucaplot2
df<-eucaplot2
head(df)
#primero calculemos la densidad
df$fe <- 10000/500
nha <- sum(df$fe)
nha
## luego calcular el area basal de cada arbol
## puede ser mediante
(pi/40000)*df$dap^2
##- o bien con la funcion gtree() del paquete biometrics
df$g<-gtree(df$dap)
df$g
#el area basal de cada arbol expandida a la hectarea
df$garb.ha <- df$g * df$fe
gha <- sum(df$garb.ha)
gha


##? II. Segunda parte del script
##+ 1. Variables de orden para el diametro
dmed<- mean(df$dap)
dmed

##+ diametro del arbol de area basal media
sqrt((gha/nha)*(40000/pi))
##- o bien use la funcion del paquete biometrics dg.fx(), como sigue
dg<-dg.fx(n=nha,g=gha)
dg

##diametro dominante
dap.orden <- df[ order(-df$dap), "dap"]
dap.orden
ddom <- mean(dap.orden[1:5])
ddom

##diametro del arbol de area basal central
median(df$g) #esto es el area basal mediana
df.ordg.d<-df[ order(-df$g), c("g","dap")]
df.ordg.d
#buscar el diametro para el area basal mediana

#en este caso que es n impar, se puede buscar mediante
median.pos<-ceiling(nrow(df)/2)
dg.central<-df.ordg.d[median.pos,c("dap")]
dg.central

#comparando los distintos diametros
c(dmed,dg,dg.central,ddom)

#+╔═════════════════╗
#+║ Fin del script! ║
#+╚═════════════════╝
