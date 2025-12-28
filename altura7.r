##! Script: "altura7.r"                                            /
##- Sobre:  Ajuste de modelos lineales con variables transformadas/
##+ Detalles:  Emplea sintaxis "corta" y/o mas eficiente para    /
##   ajustar esos modelos y muestra como obtener la prediccion  /
##* Ejemplo: Datos de altura-diametro (data=idahohd2).         /
##? Mas detalles: Entre otras cosas, en este ejercicio se:    /
## + revisa como obtener los valores predichos de la variable/
## biometrica-respuesta, i.e., la altura, en base a esta    /
## nueva sintaxis para ajustar modelos.                    /  
## + calculan valores ajustados y residuales.             /
## + representa sigma.hat.e en porcentaje.               /
## + crea grafico con valores esperados vs diametro para/
## los tres modelos.                                   /
##! --------------------------------------------------/ 
##                                                   /
##> Profesor: Christian Salas Eljatib               /
##? E-mail: christian.salas AT uchile DOT cl       /
## Web: https://eljatib.com                       /
##!==============================================/

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

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Valores predichos variable biometrica-respuesta: altura
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##? Valores para la variable biometrica-predictora
dap.test <- 10:110;length(dap.test)
## creando una dataframe con esos valores
df.testing<-data.frame(dap=dap.test)
head(df.testing)

##- Valores predichos-Modelo 1
h.predmod1 <- exp(predict(mod1,newdata = df.testing))
#revisarlo graficamente
plot(atot~dap, data=df)
lines(dap.test, h.predmod1, col="red",lwd=2)

##- Valores predichos-Modelo 2
h.predmod2 <- exp(predict(mod2,newdata = df.testing))

##- Valores predichos-Modelo 2
h.predmod3 <- 1/(predict(mod3,newdata = df.testing))

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! VI. Grafico de comportamiento para los tres modelos ajustados
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
plot(atot~dap, data=df,xlab="Diametro (cm)",
     ylab="Altura (m)", las=1, col="gray")
lines(dap.test, h.predmod1, col="red", lwd=2, lty=1)
lines(dap.test, h.predmod2, col="blue", lwd=2, lty=2)
lines(dap.test, h.predmod3, col="black", lwd=2, lty=1)

legend("bottomright",c("Mod1","Mod2","Mod3"), title="Modelo",
       col = c("red","blue","black"), lty=c(1,2,1), lwd=c(2,2,2))

##* =============
##- Guardando graficos realizados como archivos
##* =============
###active las siguientes 2 lineas, si quiere guardar el grafico como un archivo de imagen pdf
#dev.print(pdf,"compara3ModelosHd.pdf") #
#dev.off()


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Para seguir ejercitando/estudiando:
##- 1. Calcule los estadisticos de prediccion RMSD, DA, y DAA para
## cada modelo ajustado.
##- 2. Prepare un cuadro (e.g., en una hoja o en editor de texto), y
## escriba los estadisticos anteriores para cada modelo.
##- 3. Obtenga los valores predichos de altura para el mod4, y
## representelo en el grafico de comportamiento, mostrado en el
## presente script (con 3 modelos), es decir, con 4 modelos.
##- 4. Repita la pregunta 1 para el modelo 4 (mod4) y agregue los
## resultados en el cuadro de la pregunta 2.
##- 5. Cual es su opinion con respecto a los 4 modelos ajustados?
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
