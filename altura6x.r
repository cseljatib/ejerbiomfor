##! Script: "altura6.r"                                            /
##- Sobre:  Ajuste de tres modelos lineales simple (RLS), con     /
##-   transformaciones en la variable respuesta.                 /
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
##! III. Ajuste de modelos
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##  ln(h_i)=beta_0+beta_1ln(d_i)+varepsilon_i
mod1<- lm(I(log(atot))~I(log(dap)), data=df)
summary(mod1)

##  ln(h_i)=beta_0+beta_1(1/d_i)+varepsilon_i
mod2<- lm(I(log(atot))~I(1/dap), data=df)
summary(mod2)

##  (1/h_i)=beta_0+beta_1(1/d_i)+varepsilon_i
mod3<- lm(I(1/atot)~I(1/dap), data=df)
summary(mod2)

##  (1/h_i)=beta_0+beta_1(1/d_i)+varepsilon_i
mod4<- lm(I(dap/(atot^(2/5)))~dap, data=df)
summary(mod4)


#grafico de comportamiento-modelo 1
d.fake <- 10:110
length(d.fake)
h.ajumod1 <- b0.hat + b1.hat * d.fake
plot(atot~dap, data=df)
lines(d.fake, h.ajumod1, col="red",lwd=2)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! VI. Grafico de comportamiento para los tres modelos ajustados
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
plot(atot~dap, data=df,xlab="Diametro (cm)",
     ylab="Altura (m)", las=1, col="gray")
lines(d.fake, h.ajumod1, col="red", lwd=2, lty=1)
lines(d.fake, h.ajumod2, col="blue", lwd=2, lty=2)
lines(d.fake, h.ajumod3, col="black", lwd=2, lty=1)

legend("bottomright",c("Mod1","Mod2","Mod3"), title="Modelo",
       col = c("red","blue","black"), lty=c(1,2,1), lwd=c(2,2,2))

##* =============
##- Guardando graficos realizados como archivos
##* =============
###active las siguientes 2 lineas, si quiere guardar el grafico como un archivo de imagen pdf
#dev.print(pdf,"compara3ModelosHd.pdf") #
#dev.off()

##- ===================================
##? Tarea sugerida:
## 1. Prepare un cuadro en una hoja a mano, y escriba los
## parametros estimados para cada modelo (cada fila un modelo).
## 2. Compare los modelos, basado en los estadisticos calculados.
##- ===================================


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
