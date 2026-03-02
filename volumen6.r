##! Script: "volumen3.r"                                          /
##- Sobre:  Ajuste y comparacion de varios modelos de volumen       /
##+ Detalles:  Uno multiple con cinco coeficientes, y el otro   /
## con variable respuesta transformada.                        /
##* Ejemplo: Datos de volumen Pinus pinaster (data=pinaster2)./
##?  A considerar:                                           /
## Se muestra una forma alternativa y mas simple de ajustar /
## modelos sin necesidad de crear nuevas columnas en la    /
## dataframe.                                             /
##! -----------------------------------------------------/ 
##                                                      /
##> Profesor: Christian Salas Eljatib                  /
##? E-mail: christian.salas AT uchile DOT cl          /
## Web: https://eljatib.com                          /
##!=================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
data("pinaster2")
#?pinaster2 #ejecutelo en la consola

data("pinaster2")
df<-pinaster2
##creando nuevas columnas (variables), para simplificar el codigo mas adelante
df$d<-df$dap
df$h<-df$atot
df$v<-df$vtcc
df$d2<-df$d^2
df$d2h2<-df$d2*df$h^2
df$h2d<-df$h^2*df$d
df$h2<-df$h^2

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste de modelos
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Modelo 1
##> mod1: v=b0+b1(d^2)+b2(d^2*h^2)+b3(d^2*h)+b4(h^2)
m1<-lm(v~I(d^2),data=df)
summary(m1)

##- Modelo 2
##> mod1: v=b0+b1(d)+b2(h)
m2<-lm(v~d+h, data=df)
summary(m2)

##- Modelo 3
##> mod3 v=b0+b1(d^2)+b2(d^2*h^2)+b3(d^2*h)+b4(h^2)
m3<-lm(v~I(d^2)+I(d^2*h^2)+I(h^2*dap)+I(h^2),data=df)
summary(m3)

##- Modelo 4
##> mod4:(d^2)/v=b0+b1(1/h)
m4 <-lm(I(d^2/v)~I(1/h),data=df)
summary(m4)


#+╔═════════════════╗
#+║ Fin del script! ║
#+║ Atte.           ║
#+║ El profesor     ║
#+╚═════════════════╝
