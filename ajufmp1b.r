##! Script: "ajufmp1a.r"                                            /
##- Sobre:  Ajuste de una funcion de masa de probabilidades (fmp)  /
##+ Detalles: Emplea estimador "numerico" de maxima verosimilitud,/
##+   para la funcion Poisson.                                   /
##  Ejemplo: Ajuste de la funcion empleando datos de            /
##   arboles "monumentales" (o grandes).         /
## -----------------------------------------------------------/ 
##                                                           /
## Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/
### I. Empleando la funcion de Poisson
mi.lambda<-1; y<-3
factorial(3)
exp(-1)*(1^3)/factorial(3)

exp(-mi.lambda)*(mi.lambda^y)/factorial(y)

dpois(y,lambda = mi.lambda)

#varios valores de Y
y<-0:9
f.y<-dpois(y,lambda = mi.lambda)

plot(f.y~y, type="b")



### II. Ajustando un modelo de Fdp
##a. Cargando los datos
library(datana)
data(largetrees2)
#?largetrees2
df<-largetrees2
head(df)
dim(df)
hist(df$y)
table(df$y)
barplot(table(df$y))
sup.parc<-32*32;sup.parc
fe<-10000/sup.parc;fe
df$nlarge.ha<-df$y*(fe)

#Grafico de distribucion
hist(df$nlarge.ha,xlab="Arboles grandes (ind/ha)",ylab="Frecuencia (num. de parcelas)")

#Ajuste mediante maxima verosimilitud
mis.datos<-df$y
hist(mis.datos)
##valores iniciales para los parametros a ser estimados
candidatos<-seq(0,9,0.05)

try.lambda <-function(candidatos, data){
  sum(dpois(data, candidatos,log = T))}

optim.loglik.poiss<-optimize(try.lambda, candidatos, data = mis.datos, maximum = T)
optim.loglik.poiss

lambda.mle<-as.numeric(optim.loglik.poiss[1])
lambda.mle


##Grafico del modelo ajustado
##Note que la funcion de Poisson es
## para variables aleatorias discretas.
y.list<-0:9
y.tab<-table(df$y)
y.tab.rel<-y.tab/nrow(df)
plot(y.tab.rel,las=1, ylab="Probabilidad",xlab="Variable aleatoria")
par(new=T)
f.y<-dpois(y.list,lambda = lambda.mle)
lines(y.list,f.y, type="b",col="red")
legend("topright",c("Observada","Fdp de Poisson"),
         col=c("black","red"),
         lty = c(1,1))


##Tarea: comparar con el estimador
## de los momentos.

#@@@@@@@@@@@@@@@@@
#eso es todo estimad@s alumn@s
#disfRuten!
#saludos
#C
#@@@@@@@@@@@@@@@@@