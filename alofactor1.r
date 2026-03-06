##! Script: "alofactor1.r"                                         /
##- Sobre:  Incorporando un factor a un modelo alometrico         /
##+ Detalles:  Se muestra como incorporar una variable categorica/
## o factor, a un modelo alometrico, como una variable dummy en /
## el ajuste de minimos cuadrados.
##* Ejemplo: Datos de altura-diametro (data=idahohd2).         /
##? Mas detalles: Entre otras cosas, en este ejercicio se:    /
## + revisa que existen diversas formas de incorporar una
## variable dummy en un modelo, y que tienen por lo tanto   /
## diferentes implicancias matematicas y de interpretacion /
## de sus parametros estimados, y en estadistica
## inferencial.                                   /
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
df.ori<-df <- idahohd2
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
##! III. Descripcion por especie
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Estadistica descriptiva por especie
descstat(df[,c("dap","atot")],factor = df$spp, segregated = TRUE)
##numero de observaciones (ojo que antes con descstat tambien se calculo)
table(df$spp)

##+ grafico de dispersion por especie
require(lattice)
xyplot(atot~dap|spp, data=df)

##? ===================================
##? seleccionando dos especies
df<-subset(df.ori, spp=="WC" | spp=="GF")
df$spp<-droplevels(df$spp) ## esto no es necesario
table(df$spp)

xyplot(atot~dap|spp, data=df)
table(df$spp)
xyplot(atot~dap,groups=spp, data=df)

##? ===================================

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Estategias de modelacion
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##- Se utilizara el siguiente modelo como el "base"
##  h_i=beta_0+beta_1(1/d_i)+varepsilon_i
mod0<- lm(atot~I(1/dap), data=df)
summary(mod0)

df$y.m0 <- predict(mod0,newdata = df)
predstat(obs=df$atot,pre=df$y.m0,want.percent = TRUE)  

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! V. Incluyendo a la especie como variable dummy
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


##- Variante 1 del modelo base incluyendo el factor especie.
mod1<- lm(atot~I(1/dap)+spp, data=df)
summary(mod1)

##- Variante 2 del modelo base incluyendo el factor especie.
mod2<- lm(atot~I(1/dap):spp, data=df)
summary(mod2)

##- Variante 3 del modelo base incluyendo el factor especie.
mod3<- lm(atot~I(1/dap)*spp, data=df)
summary(mod3)

##* revise la estadistica inferencia (pruebas de hipotesis) de
## cada modelo ajustado.


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! VI. Prueba de hipotesis entre dos modelos: reducido vs. full
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Prueba de F-parcial

##?-----------
##? Comparando variantes con respecto al modelo sin factor
## primero escribir el modelo reducido
##+ Variante 1 vs modelo base (sin factor) 
anova(mod0,mod1)
##+ Variante 2 vs modelo base (sin factor) 
anova(mod0,mod2)
##+ Variante 3 vs modelo base (sin factor) 
anova(mod0,mod3)

##?-----------


##?-----------
##? Comparando entre variantes
##+ Variante 1 vs variante 2 
anova(mod2,mod1)
## note que tienen la misma cantidad de parametros
##+ Variante 1 vs variante 3 
anova(mod1,mod3)
##+ Variante 2 vs variante 3 
anova(mod2,mod3)
##?-----------



##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Para seguir ejercitando/estudiando:
##- 1. Calcule los estadisticos de prediccion RMSD, DA, y DAA para
## cada modelo ajustado.
##- 2. Prepare un cuadro (e.g., en una hoja o en editor de texto), y
## escriba los estadisticos anteriores para cada modelo.
##- 3. Obtenga los valores predichos de la variable respuesta para
## cada modelo, sobre un grafico de dispersion, es decir, preparar 
## un grafico de comportamiento.
##- 4. Cual es su opinion con respecto a los 4 modelos ajustados?
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
