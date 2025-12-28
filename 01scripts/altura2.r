##! Script: "altura2.r"                                           /
##- Sobre:  Ajuste de dos modelos lineales simple (RLS)          /
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
data(idahohd2)
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
##  h_i=beta_0+beta_1(d_i)+varepsilon_i
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
mod1<- lm(atot~dap, data=df)
summary(mod1)
##-RMSE
rmse.m1<-summary(mod1)$sigma
rmse.m1

#almacenar la varianza de los residuales del modelo
summary(mod1)$sigma
var.hat.e.m1 <-summary(mod1)$sigma^2
var.hat.e.m1

#valor ajustado segun un diametro
12.39+0.3254994*50 #de 50 cm
12.39+0.3254994*20 #de 20 cm
#guardando los coeficientes en un objeto
coef(mod1)
coef(mod1)[1]
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]
b0.hat
b1.hat
b0.hat + b1.hat * 50


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IIIa. Grafico de comportamiento valor esperado altura
##  Modelo 1
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#creando un vector de diametros 
d.play <- 30:35; d.play
#valor ajustado
b0.hat + b1.hat * d.play
#creando una columna en la dataframe con los valores
# ajustados dependiendo de los respectivos valores
# de diametro para el modelo 1 
df$h.aju1 <- fitted(mod1)
# y lo mismo para los residuales
df$e.aju1 <- residuals(mod1)

##- el grafico
range(df$dap)
d.test <- 10:110
length(d.test)
h.mod1 <- b0.hat + b1.hat * d.test
plot(atot~dap, data=df)
lines(d.test, h.mod1, col="red")


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Ajuste del modelo 2
## h_i=beta_0+beta_1(1/d_i)+varepsilon_i
## modelo del inverso del diametro
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##creando la variable X necesaria
df$inv.d <- 1/df$dap
head(df,2)
descstat(df[,c("dap","atot","inv.d")])

##compare
plot(atot~dap, data=df)
#con este otro grafico
plot(atot~inv.d, data=df)

##----------
##aca una forma de ver dos graficos frente a frente
op<-par(mfrow=c(1,2))
plot(atot ~ dap, data=df)
plot(atot ~ inv.d, data=df)
par(op)
##----------

#ajustando el modelo del inverso del diametro (ver apuntes)
mod2<- lm(atot~inv.d, data=df)
summary(mod2)
b0.hat2<-coef(mod2)[1]
b1.hat2<-coef(mod2)[2]
b0.hat2
b1.hat2

df$h.aju2 <- fitted(mod2)
# y lo mismo para los residuales
df$e.aju2 <- residuals(mod2)


##- ========= 
##? Como obtener el valor ajustado para el modelo 2
# Para la variable respuesta-biometrica de interes, i.e., altura 
#*1) para un par de valores de la variable predictora-biometrica
d.play
b0.hat + b1.hat * (1/d.play)
#*2) para todos los valores a evaluar de la variable predictora-biometrica
h.mod2 <- b0.hat2 + b1.hat2 * (1/d.test)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IVa. Grafico de comportamiento
##-  Modelos 1 y 2
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ Grafico de comportamiento para ambos modelos
plot(atot~dap, data=df,xlab="Diametro (cm)",
     ylab="Altura (m)", las=1)
lines(d.test, h.mod1, col="red", lwd=3, lty=1)
lines(d.test, h.mod2, col="blue", lwd=3, lty=2)
legend("bottomright",c("Mod1","Mod2"), title="Modelo",
       col = c("red","blue"), lty=c(1,2), lwd=c(2,2))


##* =============
##! Guardando graficos realizados como archivos
##* =============
###active law siguientes 2 lineas, si quiere guardar el grafico como un archivo de imagen pdf
#dev.print(pdf,"comparaModelosHd.pdf") #
#dev.off()

###active las siguientes 6 lineas si quiere guardar el grafico como un archivo de imagen
# jpeg("comparaModelosHd.jpg")
# plot(atot~dap, data=df,xlab="Diametro (cm)",   ylab="Altura (m)", las=1)
# lines(d.fake, h.ajumod1, col="red", lwd=3, lty=1)
# lines(d.fake, h.ajumod2, col="blue", lwd=3, lty=2)
# legend("bottomright",c("Mod1","Mod2"), title="Modelo",col = c("red","blue"), lty=c(1,2), lwd=c(2,2))
# dev.off()

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Para seguir ejercitando/estudiando:
##- 1. Escriba (en una hoja) los parametros estimados de cada modelo.
##- 2. Revise la inferencia estadistica respecto a los coeficientes
## estimados.
##- 3. Compare ambos modelos, basado en el grafico de comportamiento
## y los puntos anteriores.
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
