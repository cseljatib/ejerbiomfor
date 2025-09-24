##! Script: "sitio2.r"                                             /
##- Sobre:  Ajuste modelo crecimiento de altura e indice de sitio /
##+ Detalles: Emplea estimador de minimos cuadrados.             /
##+ Ejemplo: Modelo de Schumacher, datos Pinus taeda,           /
##+ muestra el uso del modelo ajustado, y grafica curvas       /
##+ de indice de sitio                                        /
## ----------------------------------------------------------/ 
##                                                          /
## Profesor: Christian Salas Eljatib                       /
## E-mail: christian.salas AT uchile DOT cl               /
## Web: https://eljatib.com                              /
##======================================================/


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos para ejemplo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(ptaeda2)
df<-ptaeda2
#?ptaeda2 #ejecutelo en la consola
head(df)
dim(df)
summary(df$atot)
summary(df$edad)
summary(df[,c("edad","atot")])
##cuadro de estadistica descriptiva, usando una funcion
# especifica
df1<- df[,c("edad","atot")]
head(df1)
descstat(df1)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+# II. Graficos de interes
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- (1) Dispersion
plot(atot~edad, data=df,las=1)
##- (2) Series de tiempo
library(lattice)
xyplot(atot~edad|as.factor(semilla.id),data=df, type="b")
xyplot(atot~edad,groups=as.factor(semilla.id),data=df, type="b")

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## III. Ajuste del modelo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##* Ajuste del modelo de Schumacher
m1.schu<-lm(I(log(atot))~I(1/edad), data=df)
summary(m1.schu)

##! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ Compare el ajuste anterior con un modelo donde Ud crea
##+  las variables en la dataframe
df$ln.h <- log(df$atot)
df$inv.t <- 1/df$edad
m1.schu.b<-lm(ln.h~inv.t, data=df)
summary(m1.schu.b)
##! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## IV. Grafico de comportamiento
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
##-Guardando los coeficientes en un objeto
b0.hat.schu <- coef(m1.schu)[1]; b1.hat.schu <- coef(m1.schu)[2];

plot(ln.h~inv.t, data=df)
abline(m1.schu,col="red")

##-Valores esperados segun el modelo
##usando el modelo de crecimiento ajustado
exp(b0.hat.schu+b1.hat.schu*(1/10))
exp(b0.hat.schu+b1.hat.schu*(1/15))

##el modelo de indice de sitio
##anamorfico
#ln.h=ln.s+b1(1/t - 1/tb)
##crec.en altura a los 10 anhos, en IS=25
exp(log(25)+b1.hat.schu*((1/10)-(1/20)))
##crec.en altura a los 20 anhos, en IS=25
exp(log(25)+b1.hat.schu*((1/20)-(1/20)))

##curva crec.en altura, en IS=25
t.fake<-2:40
##edad clave 20 anhos
tb<-20
h.is25<-exp(log(25)+b1.hat.schu*((1/t.fake)-(1/tb)))
plot(h.is25~t.fake,type="l")

##curva crec.en altura, en IS=22
h.is22<-exp(log(22)+b1.hat.schu*((1/t.fake)-(1/tb)))
plot(h.is22~t.fake,type="l")

##dos curvas de indice de sitio
plot(h.is25~t.fake,type="l", xlab="Edad (años)", ylab="Altura (m)",las=1)
lines(h.is22~t.fake,col="red")
abline(v=20,lty=2)

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝