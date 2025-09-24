## Script: ajumodcreci1.r
## Sobre?: Ajuste de modelo no-lineal de crecimiento en altura
##
## Ejemplo con el ajuste del modelo de poder
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

#ajuste del modelo de poder
poder.nls <- nls(atot ~
 alpha*(edad^beta), data = df,
start = list(alpha = 20, beta= 0.1), trace=T)

summary(poder.nls)
coef(poder.nls)

t.fake <- seq(3,30,by=0.1)
yhat<-predict(poder.nls, newdata=data.frame(edad=t.fake))

plot(atot~edad, data=df)
lines(t.fake,yhat,col="red",lwd=3)


##Calcule el RMSD y DA para el modelo ajustado

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝