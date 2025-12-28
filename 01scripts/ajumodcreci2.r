## Script: ajumodcreci2.r
## Sobre?: Ajuste de modelo no-lineal de crecimiento en altura
##
## Ejemplo con el ajuste del modelo logistico
## -----------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/
library(datana)
df <- ptaeda2
head(df)
summary(df)

plot(atot~edad, data=df)
library(lattice)
xyplot(atot~edad|as.factor(semilla.id),data=df, type="b")

xyplot(atot~edad,groups=as.factor(semilla.id),data=df, type="b")

#ajuste del modelo logistico
logis.nls <- nls(atot ~
 alpha/(1 + exp(b0 + b1 * edad)), data = df,
start = list(alpha = 20, b0 = 1, b1 = -0.05), trace=T)

summary(logis.nls)
coef(logis.nls)

t.fake <- seq(3,30,by=0.1)
yhat<-predict(logis.nls, newdata=data.frame(edad=t.fake))

plot(atot~edad, data=df)
lines(t.fake,yhat,col="red",lwd=3)
abline(h=20,lty=2,col="blue")


##compare el ajuste de este modelo con uno lineal simple

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝