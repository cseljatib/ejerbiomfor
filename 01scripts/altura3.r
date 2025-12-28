##! Script: "altura3.r"                                            /
##- Sobre:  Ajuste de tres modelos lineales simple (RLS), con     /
##-   transformaciones en la variable predictora.                 /
##+ Detalles:  Emplea estimador de minimos cuadrados.           /
##* Ejemplo: Datos de altura-diametro (data=idahohd2).         /
##? Mas detalles: Entre otras cosas, el este ejercicio se:    / 
## + calculan valores ajustados y residuales.                /
## + representa sigma.hat.e en porcentaje.                  /
## + crea grafico con valores esperados vs diametro para   /
## los tres modelos.                                      /
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

##- Grafico dispersion con distribucion marginal
xyhist(x=df$dap,y=df$atot)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste del modelo 1
##  h_i=beta_0+beta_1(d_i)+varepsilon_i
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
mod1<- lm(atot~dap, data=df)
summary(mod1)
summary(mod1)
#guardando los coeficientes en un objeto
coef(mod1)
coef(mod1)[1]
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]

##- ========= 
##? Como obtener el valor ajustado para el modelo 1
# Para la variable respuesta-biometrica de interes, i.e., altura 
#*1) para un par de valores de la variable predictora-biometrica
d.play<-30:35;d.play
b0.hat + b1.hat * (1/d.play)
#*2) para todos los valores a evaluar de la variable predictora-biometrica
d.test <- 10:110
h.mod1 <- b0.hat + b1.hat * (1/d.test)


#grafico de comportamiento-modelo 1
plot(atot~dap, data=df)
lines(d.test, h.mod1, col="red",lwd=2)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Ajuste del modelo 2
##  h_i=beta_0+beta_1(1/d_i)+varepsilon_i
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##creando la variable X necesaria para el modelo 2
df$inv.d <- 1/df$dap
plot(atot~inv.d, data=df)
mod2<- lm(atot~inv.d, data=df)
summary(mod2)
b0.hat2<-coef(mod2)[1]
b1.hat2<-coef(mod2)[2]
b0.hat2
b1.hat2

##- ========= 
##? Como obtener el valor ajustado para el modelo 2
# Para la variable respuesta-biometrica de interes, i.e., altura 
#*1) para un par de valores de la variable predictora-biometrica
d.play
b0.hat + b1.hat * (1/d.play)
#*2) para todos los valores a evaluar de la variable predictora-biometrica
h.mod2 <- b0.hat2 + b1.hat2 * (1/d.test)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! V. Ajuste del modelo 3
## ln(h_i)=beta_0+beta_1(e^(-0.03d_i))+varepsilon_i
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Creando la variable Y necesaria para el modelo
df$ln.h<-log(df$atot)
##creando la variable X necesaria para el modelo
df$exp.d<-exp(-0.03*df$dap)
plot(ln.h~exp.d, data=df)

descstat(df[,c("dap","exp.d","atot","ln.h")])
mod3<- lm(ln.h~exp.d, data=df)
summary(mod3)
b0.hat3<-coef(mod3)[1]
b1.hat3<-coef(mod3)[2]
b0.hat3
b1.hat3


##- ========= 
##? Como obtener el valor ajustado para el modelo 3
# Para la variable respuesta-biometrica de interes, i.e., altura 
#*1) para un par de valores de la variable predictora-biometrica
d.play
b0.hat3+ b1.hat3 * (exp(-0.03*d.play))
# en que unidad esta la variable respuesta del modelo
## estadistico ajustado?.
exp(b0.hat3+ b1.hat3 * (exp(-0.03*d.play)))

#*2) para todos los valores a evaluar de la variable predictora-biometrica
h.mod3 <- exp(b0.hat3 + b1.hat3 * exp(-0.03*d.test))

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! VI. Grafico de comportamiento para los tres modelos ajustados
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
plot(atot~dap, data=df,xlab="Diametro (cm)",
     ylab="Altura (m)", las=1, col="gray")
lines(d.test, h.mod1, col="red", lwd=2, lty=1)
lines(d.test, h.mod2, col="blue", lwd=2, lty=2)
lines(d.test, h.mod3, col="black", lwd=2, lty=1)

legend("bottomright",c("Mod1","Mod2","Mod3"), title="Modelo",
       col = c("red","blue","black"), lty=c(1,2,1), lwd=c(2,2,2))

##* =============
##- Guardando graficos realizados como archivos
##* =============
###active las siguientes 2 lineas, si quiere guardar el grafico como un archivo de imagen pdf
#dev.print(pdf,"compara3ModelosHd.pdf") #
#dev.off()

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Para seguir ejercitando/estudiando:
##+ 1. Escriba, en una hoja a mano, cada modelo estadistico
## poblacional que se ha ajustado en este script.
##+ 2. Prepare un cuadro en una hoja a mano, y escriba los
## parametros estimados para cada modelo (cada fila un modelo).
##+ 3. Compare los modelos, basado en los estadisticos calculados.
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
