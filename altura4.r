##! Script: "altura4.r"                                            /
##- Sobre:  Ajuste de dos modelos lineales, con variable          /
## respuesta transformada                                        /
##+ Detalles:  Emplea estimador de minimos cuadrados.           /
##* Ejemplo: Datos de altura-diametro (data=idahohd2).         /
##? Mas detalles: Entre otras cosas, en este ejercicio se:    / 
## + calculan valores ajustados y residuales.                /
## + representa sigma.hat.e en porcentaje.                  /
## + crea grafico con valores esperados vs diametro para   /
## los dos modelos.                                       /
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
df <- idahohd2
#?idahohd2 #ejecutelo en la consola
head(df)
dim(df)

##-Estadistica descriptiva
descstat(df[,c("dap","atot")])


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! II. Graficos de interes
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
hist(df$atot)
hist(df$dap)

##-Dispersion
plot(atot~dap, data=df)
plot(atot~dap, data=df, xlab="Diametro (cm)",
ylab="Altura (m)")
plot(atot~dap, data=df)

#compare grafico anterior, con los siguientes
plot(atot~dap, data=df, las=1)
plot(atot~dap, data=df, las=1, col="blue")
plot(atot~dap, data=df, xlab="Diametro (cm)",
     ylab="Altura (m)", las=1, col="blue")

plot(atot~dap, data=df)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste del modelo 1
## ln(h_i)=beta_0+beta_1 ln(d_i)+varepsilon_i
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
df$ln.h <- log(df$atot)
df$ln.d <- log(df$dap)
head(df)
mod1<- lm(ln.h~ln.d, data=df)
summary(mod1)
##-RMSE
rmse.m1<-summary(mod1)$sigma
rmse.m1

#almacenar la varianza de los residuales del modelo
summary(mod1)$sigma
var.hat.e.m1 <-summary(mod1)$sigma^2
var.hat.e.m1

#valor ajustado segun un diametro
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]
b0.hat
b1.hat
b0.hat + b1.hat * log(50)

exp(b0.hat + b1.hat * log(50))

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IIIa. Grafico de comportamiento
##  Modelo 1
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#creando un vector de diametros 
d.play <- 30:35
d.play
b0.hat + b1.hat * log(d.play)
#creando una columna en la dataframe con los valores
# ajustados dependiendo de los respectivos valores
# de diametro para el modelo 1 
df$aju <- exp(b0.hat+b1.hat*log(df$dap))
head(df)
#valor error prediccion
df$e.aju <- df$atot - df$aju 
head(df)


## Otra forma de obtener lo mismo anterior
df$h.aju1 <- exp(fitted(mod1))

##estadisticos de prediccion, que se deben calcular en base a
## la variable respuesta-biometrica, i.e., la altura
predstat(obs = df$atot, pre = df$h.aju1, decnum = 3)

##- el grafico de comportamiento
50:55 #secuencia de valores
exp(b0.hat + b1.hat * log(50:55))
range(df$dap)
d.test <- 10:110
length(d.test)
h.mod1 <- exp(b0.hat + b1.hat * log(d.test))
plot(atot~dap, data=df)
lines(d.test, h.mod1, col="red")


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Ajuste del modelo 2
## (1/h_i)=beta_0+beta_1(1/d_i)+varepsilon_i
## modelo del inverso del diametro
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##creando la variable X necesaria
df$inv.d <- 1/df$dap
df$inv.h <- 1/df$atot
head(df,2)
descstat(df[,c("dap","atot","inv.d","inv.h")])

##compare
plot(atot~dap, data=df)
#con este otro grafico
plot(inv.h~inv.d, data=df)

#ajustando el modelo 2
mod2<- lm(inv.h~inv.d, data=df)
b0.hat2<-coef(mod2)[1]
b1.hat2<-coef(mod2)[2]
b0.hat2
b1.hat2
#valor ajustado del modelo 2
df$h.aju2 <- 1/(fitted(mod2))
head(df)

###estadistico de validacion
predstat(obs = df$atot, pre = df$h.aju2, decnum = 3)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IVa. Grafico de comportamiento
##-  Modelos 1 y 2
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ Grafico de comportamiento para ambos modelos
h.mod2 <-1/(b0.hat2 + b1.hat2 * (1/d.test))

plot(atot~dap, data=df,xlab="Diametro (cm)",
     ylab="Altura (m)", las=1)
lines(d.test, h.mod1, col="red", lwd=3, lty=1)
lines(d.test, h.mod2, col="blue", lwd=3, lty=2)
legend("bottomright",c("ln(h)=f(d)","(1/h)=f(1/d)"), title="Modelo",
       col = c("red","blue"), lty=c(1,2), lwd=c(2,2))


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Para seguir ejercitando/estudiando:
##- 1. Escriba (en una hoja) los parametros estimados de cada modelo.
##- 2. Revise la inferencia estadistica respecto a los coeficientes
## estimados.
##- 3. Compare ambos modelos, basado en el grafico de comportamiento
## y los puntos anteriores.
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
