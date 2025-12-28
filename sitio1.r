##! Script: "sitio1.r"                                            /
##- Sobre:  Ajuste modelo crecimiento de altura                  /
##+ Detalles: Emplea estimador de minimos cuadrados.            /
##+ Ejemplo: Modelo de Schumacher, datos Pinus taeda, y        /
##+ muestra el uso del modelo ajustado                        /
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

plot(ln.h~inv.t, data=df,las=1)
abline(m1.schu,col="red")

##-Valores esperados segun el modelo
##usando el modelo de crecimiento ajustado
exp(b0.hat.schu+b1.hat.schu*(1/10))
exp(b0.hat.schu+b1.hat.schu*(1/15))

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝