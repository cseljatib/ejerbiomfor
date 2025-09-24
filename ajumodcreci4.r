## Script: ajumodcreci4.r
## Sobre: Ajuste de modelo no-lineal de crecimiento en altura
##
## Ejemplo con el ajuste del modelo logistico, Michaelis-Menten y de Richards
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
 alpha/(1 + exp(beta + gamma * edad)), data = df,
start = list(alpha = 20, beta = 1, gamma = -0.05), trace=T)

summary(logis.nls)
coef(logis.nls)

t.fake <- seq(3,170,by=0.1)
yhat<-predict(logis.nls, newdata=data.frame(edad=t.fake))

plot(atot~edad, data=df)
lines(t.fake,yhat,col="red",lwd=3)
#abline(h=66)

#ajuste del modelo de Michaelis-Menten
mm.nls <- nls(atot~
 (alpha*edad)/(beta+edad), 
  data = df,
start = list(alpha = 170, beta=0.5),trace=T)

summary(mm.nls)
coef(mm.nls)

yhat2<-predict(mm.nls, newdata=data.frame(edad=t.fake))

plot(atot~edad, data=df)
lines(t.fake,yhat,col="red",lwd=3)
lines(t.fake,yhat2,col="blue",lwd=3)

#Modelo de Richards
rich.nls <- nls(atot ~
  alpha*(1 - exp(-beta * edad))^(1/gamma), data = df,
  start = list(alpha = 70, beta = .01, gamma = 1), trace=T)

summary(rich.nls)
coef(rich.nls)

yhat3<-predict(rich.nls, newdata=data.frame(edad=t.fake))

plot(atot~edad, data=df,las=2)
lines(t.fake,yhat,col="red",lwd=3)
lines(t.fake,yhat2,col="blue",lwd=3)
lines(t.fake,yhat3,col="green",lwd=2)

##- Calcule el RMSD y DA de los modelos.
##- Produzca un grafico que compare los valores esperados vs. diametro
## para ambos modelos (i.e., dos curvas)
##- Compare ambos modelos no-lineales, cual es el mejor?


#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝