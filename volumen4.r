##! Script: "volumen2.r"                                          /
##- Sobre:  Ajuste y comparacion de dos modelos de volumen       /
##+ Detalles:  Uno multiple con cinco coeficientes, y el otro   /
## con variable respuesta transformada.                        /
## Se explica como obtener predicciones de la variable de
##  interes para cada modelo, es decir, volumen.
##?  A considerar:                                           /
## Se muestra una explicacion paso a paso (i.e., larga)
##* Ejemplo: Datos de volumen Pinus pinaster (data=pinaster2)./
##                                                           /
##! --------------------------------------------------------/ 
##                                                         /
##> Profesor: Christian Salas Eljatib                     /
##? E-mail: christian.salas AT uchile DOT cl             /
## Web: https://eljatib.com                             /
##!====================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
data("pinaster2")
#?pinaster2 #ejecutelo en la consola

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
m1<-lm(v~d2+d2h2+h2d+h2,data=df)
summary(m1)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Prediccion en base a  modelos ajustados
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##- Prediccion a partir de modelo 1
d.ast<-16.5; h.ast<-11.7

b0.hat<-coef(m1)[1]
b1.hat<-coef(m1)[2]
b2.hat<-coef(m1)[3]
b3.hat<-coef(m1)[4]
b4.hat<-coef(m1)[5]

##+ ejemplo para el diametro y la altura dados
b0.hat+b1.hat*d.ast^2+b2.hat*(d.ast^2*h.ast^2)+b3.hat*(h.ast^2*d.ast)+b4.hat*(h.ast^2)


##+ guardando los valores predichos en la dataframe
df$v.m1<-b0.hat+b1.hat*df$dap^2+b2.hat*(df$dap^2*df$atot^2)+b3.hat*(df$atot^2*df$dap)+b4.hat*(df$atot^2)

summary(df$v.m1)

n<-nrow(df)
##- Calculo de error en prediccion del volumen, modelo 1
df$e.aju1 <- df$v - df$v.m1
##* calculo del RMSD - modelo 1
sum(df$e.aju1^2)/n
sqrt(sum(df$e.aju1^2)/n)
rmsd.1<-sqrt(sum(df$e.aju1^2)/n)
100*rmsd.1/mean(df$v)
rmsd.1p<-100*rmsd.1/mean(df$v)

##- Modelo 2
##> mod2: (d^2)/v=b0+b1(1/h)
df$d2.v<-df$d2/df$v
df$inv.h<-1/df$h
m2<-lm(d2.v~inv.h,data=df)
summary(m2)
b0.hat.m2<-coef(m2)[1]
b1.hat.m2<-coef(m2)[2]


##- Prediccion a partir de modelo 1
##+ ejemplo para el diametro y la altura dados
b0.hat.m2+b1.hat.m2*(1/h.ast)
(d.ast^2)*(1/
           (b0.hat.m2+b1.hat.m2*(1/h.ast))
)

##+ guardando los valores predichos en la dataframe
df$y.aju2<-b0.hat.m2+b1.hat.m2*(1/df$h)
df$v.m2<- (df$d^2)*(1/df$y.aju2)


##- Calculo de error en prediccion del volumen, modelo 2
df$e.aju2 <- df$v - df$v.m2
##* calculo del RMSD - modelo 2
sum(df$e.aju2^2)/n
sqrt(sum(df$e.aju2^2)/n)
rmsd.2<-sqrt(sum(df$e.aju2^2)/n)
100*rmsd.2/mean(df$v)
rmsd.2p<-100*rmsd.2/mean(df$v)

c(rmsd.1,rmsd.2)

c(rmsd.1p,rmsd.2p)

#valor predicho por modelo 2


#+╔═════════════════╗
#+║ Fin del script! ║
#+║ Atte.           ║
#+║ El profesor     ║
#+╚═════════════════╝
