##! Script: "volumen2.r"                                         /
##- Sobre:  Prueba de hipotesis en ajusta de modelos     /
##+ Detalles:  Emplea estimador de minimos cuadrados.          /
##* Ejemplo: Datos de volumen Pinus pinaster (data=pinaster2)./
##                                                           /
##! --------------------------------------------------------/ 
##                                                         /
##> Profesor: Christian Salas Eljatib                     /
##? E-mail: christian.salas AT uchile DOT cl              /
## Web: https://eljatib.com                             /
##!====================================================/


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
data("pinaster2")
#?pinaster2 #ejecutelo en la consola

df<-pinaster2

head(df)


##+ Definiendo el tipo de volumen a ocupar
df$v <- df$vtcc #volumen con corteza

library(datana)
descstat(df[,c("dap","atot","v")])

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! II. Graficos de interes
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
hist(df$dap)
hist(df$atot)
hist(df$v)


plot(v~dap, data=df)

##- Grafico dispersion con distribucion marginal
xyhist(x=df$dap,y=df$v)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste de modelos
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Modelo 1
##? v_i=beta_0+beta_1(d_i)+varepsilon_i
m1<-lm(v~dap, data=df)
summary(m1)

##- Modelo 2
##? v_i=beta_0+beta_1(d_i)+beta_2(h_i)+varepsilon_i
m2 <- lm(v~dap+atot, data=df)
summary(m2)

##- Modelo 3
m3 <- lm(v~dap+atot+d4, data=df)
summary(m3)


##Evalue las pruebas de hipotesis para cada modelo ajustado

## IV Intervalos confidenciales para los parametros estimados
confint(m1)
confint(m2)
confint(m3)



#+╔═════════════════╗
#+║ Fin del script! ║
#+║ Atte.           ║
#+║ El profesor     ║
#+╚═════════════════╝

