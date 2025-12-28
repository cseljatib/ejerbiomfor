##! Script: "altura1.r"                                           /
##- Sobre:  Ajuste modelo de regresion lineal simple (RLS)       /
##+ Detalles:  Emplea estimador de minimos cuadrados.           /
##* Ejemplo: Datos de altura-diametro (data=idahohd2).         /
##? Mas detalles: Entre otras cosas, en este ejercicio se:    / 
## + calculan valores ajustados y residuales.                /
## + representa sigma.hat.e en porcentaje.                  /
## + crea grafico con valores esperados vs diametro para   /
## el modelo.                                             /
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
summary(df$dap)
summary(df$atot)
summary(df[,c("dap","atot")])
head(df)
str(df)

##-Cuadro de estadistica descriptiva para dos variables
## usando una funcion especifica
df1<- df[,c("dap","atot")]
head(df1)
descstat(df1)
head(df)


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
##! III. Ajuste del modelo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Ajuste de modelo de regresion lineal simple
##  h_i=beta_0+beta_1(d_i)+varepsilon_i
mod1<- lm(atot~dap, data=df)
summary(mod1)

##+ ----------------
##- Algunos estadisticos de interes
##- RMSE del modelo
#note que la desv. estandar de los residuales se obtiene directamente
# con lo siguiente
summary(mod1)$sigma
##un error en porcentaje del modelo
100*summary(mod1)$sigma/mean(df$atot)

##+ ----------------
##- Almacenar la varianza de los residuales del modelo
summary(mod1)$sigma
var.hat.e.m1 <-summary(mod1)$sigma^2
var.hat.e.m1

##+ ----------------
##? Valor ajustado segun un diametro
12.39+0.3254994*50 #de 50 cm
12.39+0.3254994*20 #de 20 cm

##+ ----------------
##-Guardando los coeficientes en un objeto
coef(mod1)
coef(mod1)[1]
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]
b0.hat
b1.hat
b0.hat + b1.hat * 50

##- ========= 
##? Como obtener el valor ajustado para el modelo 1
# Para la variable respuesta-biometrica de interes, i.e., altura 
#*1) para un par de valores de la variable predictora-biometrica
d.play<-30:35;d.play
b0.hat + b1.hat * (d.play)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Valores ajustados y residuales
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
df$aju <- b0.hat+b1.hat*df$d 
head(df)
##- Valor residual
df$e.aju <- df$atot - df$aju 
head(df)

##+ ---------------
##+ Otra forma de obtener lo mismo anterior
df$h.aju1 <- fitted(mod1)
df$e.aju1 <- residuals(mod1)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! V. Grafico de comportamiento
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


##? ======================================
##- Grafico de comportamiento del modelo
##+ es decir, valor esperado para la variable respuesta
##i. alternativa con funcion pre-programada en R
plot(atot~dap, data=df, xlab="Diametro (cm)", ylab="Altura (m)")
abline(mod1, col="red")

##- Compare el anterior con este grafico
plot(atot~dap, data=df, xlab="Diametro (cm)", ylab="Altura (m)",
     col="gray",las=1)
abline(mod1, col="red",lwd=2)


##? ii. alternativa mas larga, pero quizas mas transparente
##- Generando vector ficticio con la variable predictora
#*2) para todos los valores a evaluar de la variable predictora-biometrica
1:5 #una secuencia
d.test <- 10:110
h.mod1 <- b0.hat + b1.hat * (d.test)

plot(atot~dap, data=df)
lines(d.test, h.mod1, col="red")


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Para seguir ejercitando/estudiando:
##- 1. Escriba (en una hoja) los parametros estimados del modelo ajustado.
##- 2. Revise la inferencia estadistica respecto a los coeficientes
## estimados.
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
