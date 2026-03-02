##! Script: "volumen5.r"                                          /
##- Sobre:  Ajuste y comparacion de dos modelos de volumen       /
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
m1<-lm(v~I(d^2)+I(d^2*h^2)+I(h^2*d)+I(h^2),data=df)
summary(m1)

##- Modelo 2
##> mod2: (d^2)/v=b0+b1(1/h)
m2<-lm(I(d^2/v)~I(1/h),data=df)
summary(m2)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Prediccion (simple) con modelo ajustados
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##? Se usa la funcion predict().

##! a). Prediccion para las observaciones en "df"

##- Prediccion a partir de modelo 1
df$v.m1<-predict(m1,newdata = df)

##- Prediccion a partir de modelo 2
df$v.m2<-(1/predict(m2,newdata = df))*df$d^2

head(df)

##! b). Prediccion para valores dados de altura y diametro dados
d.ast<-16.5; h.ast<-11.7
summary(m1)
df.ast<-data.frame(d=d.ast,h=h.ast)
df.ast

##- Prediccion a partir de modelo 1
predict(m1,newdata = df.ast)

##- Prediccion a partir de modelo 2
df.ast$d^2*(1/predict(m2,newdata = df.ast))

##y si los quiere agregar a esta dataframe

df.ast$v.m1<-predict(m1,newdata = df.ast)
df.ast$v.m2<-df.ast$d^2*(1/predict(m2,newdata = df.ast))

df.ast

#+╔═════════════════╗
#+║ Fin del script! ║
#+║ Atte.           ║
#+║ El profesor     ║
#+╚═════════════════╝
