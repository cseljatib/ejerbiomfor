##! Script: "volumen3.r"                                          /
##- Sobre:  Ajuste y comparacion de varios modelos de volumen       /
##+ Detalles:  Algunos simples, otros multiples, con
## transformaciones en variable respuesta y/o predictora(s).                        /
##* Ejemplo: Datos de volumen Pinus pinaster (data=pinaster2)./
##?  A considerar:                                           /
## a) Se muestra una forma simple de ajustar modelos, sin
## necesidad de crear nuevas columnas en la  dataframe.   /
## b) Se calculas estadisticos de prediccion.   /
##! -----------------------------------------------------/ 
##                                                      /
##> Profesor: Christian Salas Eljatib                  /
##? E-mail: christian.salas AT uchile DOT cl          /
## Web: https://eljatib.com                          /
##!=================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
library(biometrics)
data("pinaster2")
#?pinaster2 #ejecutelo en la consola

df<- pinaster2
head(df)
##creando nuevas columnas (variables), para simplificar el codigo mas adelante
df$d<-df$dap
df$h<-df$atot
df$v<-df$vtcc

descstat(df[,c("d","h","v")])

plot(v~I(d),data=df)
plot(I(log(v))~I(log(d)),data=df)
plot(v~I(d^2),data=df)
plot(v~I(h),data=df)
plot(v~I(d^2*h),data=df)
plot(I(log(v))~I(log(d^2*h)),data=df)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste de modelos
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Modelo 1
##> mod1: v=b0+b1(d^2)+b2(d^2*h^2)+b3(d^2*h)+b4(h^2)
m1<-lm(v~I(d^2),data=df)
summary(m1)

##- Modelo 2
##> mod2: ln(v)=b0+b1(ln(d^2*h))
m2<-lm(I(log(v))~I(log(d^2*h)), data=df)
summary(m2)


##- Modelo 3
##> mod3 v=b0+b1(d^2)+b2(d^2*h^2)+b3(d^2*h)+b4(h^2)
m3<-lm(v~I(d^2)+I(d^2*h^2)+I(h^2*dap)+I(h^2),data=df)
summary(m3)

##- Modelo 4
##> mod4:(d^2)/v=b0+b1(1/h)
m4 <-lm(I(d^2/v)~I(1/h),data=df)
summary(m4)



##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Prediccion (simple) para modelos ajustados
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##? Se usa la funcion predict().


##- Prediccion modelo 1
df$v.m1<-predict(m1,newdata = df)
##- Prediccion modelo 2
df$v.m2<-exp(predict(m2,newdata = df))
summary(df$v.m2)
##- Prediccion modelo 3
df$v.m3<-predict(m3,newdata = df)
##- Prediccion modelo 4
df$v.m4<-(1/predict(m4,newdata = df))*df$d^2

descstat(df[,c("d","h","v","v.m1","v.m2","v.m3","v.m4")])

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! V. Estadisticos de prediccion para cada modelo ajustado
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##? se emplea la funcion predstat() de paquete datana

predstat(obs=df$v,pre=df$v.m1)
predstat(obs=df$v,pre=df$v.m1,want.percent = T)

predstat(obs=df$v,pre=df$v.m2,want.percent = T)
predstat(obs=df$v,pre=df$v.m3,want.percent = T)
predstat(obs=df$v,pre=df$v.m4,want.percent = T)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Tarea sugerida:
## 1. Prepare un cuadro en una hoja a mano, y escriba los
## parametros estimados de cada modelo  (cada fila un modelo).
## 2. Prepare otro cuadro en una hoja a mano, y escriba los
## estadisticos de prediccion de cada modelo  (cada fila un modelo).
## 3. Compare los modelos, basado en los estadisticos de
##    prediccion calculados.
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Bonus!!:
## Una vez que realice lo anterior,
##podria comparar su ultimo cuadro con lo siguiente,
mod1<-predstat(obs=df$v,pre=df$v.m1,want.percent = T)
mod2<-predstat(obs=df$v,pre=df$v.m2,want.percent = T)
mod3<-predstat(obs=df$v,pre=df$v.m3,want.percent = T)
mod4<-predstat(obs=df$v,pre=df$v.m4,want.percent = T)

rbind(mod1,mod2,mod3,mod4)

#+╔═════════════════╗
#+║ Fin del script! ║
#+║ Atte.           ║
#+║ El profesor     ║
#+╚═════════════════╝
