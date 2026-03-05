##! Script: "ajufdp1b.r"                                            /
##* Sobre:  Ajuste de funcion de densidad de probabilidades  y    /
##    comparacion con la tabla de rodal respectiva.
##+ Detalles: Emplea estimador numerico de maxima verosimilitud,  /
##  mediante optimizacion.                                       /
##- Ejemplo: Ajuste de funcion de Weibull para datos de          /
##  diametro de arboles en un bosque.                          /
##------------------------------------------------------------/ 
##                                                           /
##> Profesor: Christian Salas Eljatib                       /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- I. Datos a emplear
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
library(datana)
head(llancahue2)
##Activar siguiente linea para ver metadata
#?llancahue2

df <- llancahue2
dim(df)
str(df)

descstat(df[,"dap"])
num.arbs<-nrow(df)

##- Densidad del rodal
sup.plot<-130*70  #en m2
sup.plot
sup.plot.ha<-sup.plot/10000 #en hectareas
sup.plot.ha
fe<-10000/sup.plot
nha<-nrow(df)*fe
nha

#-- Declarando la variable aleatoria de interes
df$y <- df$dap #la variable aleatoria
summary(df$y)

hist(df$y)

histbxp(df$y)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- II. Ajuste del modelo de fdp
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ Ajuste mediante maxima verosimilitud
## Maximizar la funcion de maxima verosimilitud de la fdp
## Aca se define (por Ud) una funcion que la calcule
loglike.wei <-function(parametros=parametros,
                        data=data){
  -sum(dweibull(data, shape=parametros[1],scale=parametros[2],log = T))
}
##la que tiene un signo negativo antes de la funcion
loglike.wei(c(1,20),df$y)
loglike.wei(c(4,30),df$y)

##valores iniciales para los parametros a ser estimados
candidatos<-c(.2,30)
candidatos

##?optim
optim.loglik.wei<-optim(c(candidatos[1],candidatos[2]),loglike.wei,data=df$y)
optim.loglik.wei
names(optim.loglik.wei)
optim.loglik.wei$par
optim.loglik.wei$value

#guardando resultados
param.wei.mle<-optim.loglik.wei$par
param.wei.mle
alpha.mle<-param.wei.mle[1]
beta.mle<-param.wei.mle[2]


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- III. Comparacion con tabla de rodal
##- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##------------------------
##Para visualizar el ajuste del modelo, se puede proceder como sigue
##(1) primero construir tabla de rodal, para mismos intervalos
##anteriores

##+ amplitud de la clase diametrica a utilizar
amp.diam <- 5
##+ crear una columna con la superficie de la parcela, en metros cuadrados
sup.plot
df$sup.plot<-sup.plot
    
##iii. Creando la tabla rodal
trod<-biometrics:::standtab(data=df,plot.id = 1, plot.area = "sup.plot",d="dap",w.amp=amp.diam)
trod

##? verificando que la suma de la densidad por clase diametrica es
##igual a la densidad total del rodal
nha
sum(trod$nha.cd)



##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- IV. Aplicando el modelo ajustado
##- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## (a) generando una tabla de rodal
cdap<-trod$dap.class
nha.cd<-trod$nha.cd
lim.inf <- cdap-(amp.diam/2);lim.inf
lim.sup <- cdap+(amp.diam/2);lim.sup
cbind(lim.inf,lim.sup)
prob.sup<-pweibull(lim.sup, shape = alpha.mle, scale = beta.mle)
prob.inf<-pweibull(lim.inf, shape = alpha.mle, scale = beta.mle)
prob.cd<-prob.sup-prob.inf
##lo que resta de probabilidades debe ser asignado
delta.prob<-1-sum(prob.cd)
#otra es asignar diferencial proporcionalmente
pondera.cd.ori<-pondera.cd<-prob.cd/sum(prob.cd)
sum(pondera.cd)
prob.cd.nogood<-prob.cd
addcd.dife.prob<-pondera.cd*delta.prob
prob.cd<-prob.cd+addcd.dife.prob

df.h<-data.frame(cdap,lim.inf,lim.sup,prob.inf,prob.sup,prob.cd)

sum(df.h$prob.cd)
head(df.h)

##+ (2) graficar ambas aproximaciones
frec.rel<-as.numeric(nha.cd/nha)
sum(frec.rel)
#para verificar
#data.frame(cds,nha.cd,dap.l,prob.cd,frec.rel)
nha.cd.esp<-prob.cd*nha
#genera tabla de rodal, observada y esperada bajo el modelo fdp
trod.espe<-data.frame(cdap,frec.rel,prob.cd,nha.cd,nha.cd.esp)
trod.espe

plot(cdap,frec.rel,col="black",type = "o",las=1,bty="l",
     ylab="Frecuencia relativa",xlab="Diametro (cm)")
lines(cdap,prob.cd,col="red",type = "o")
legend("topright",c("Observada","Fdp de Weibull"),
       col=c("black","red"),
       lty = c(1,1), pch=c(1,1))

##+ (3) Calculo de estadisticos predictivos
head(trod.espe)
predstat(obs=trod.espe$nha.cd,pre = trod.espe$nha.cd.esp)
predstat(obs=trod.espe$nha.cd,pre = trod.espe$nha.cd.esp,want.percent = TRUE)

message("Si ves este mensaje, estamos OK!!")
##╔══════════════════════╗
##║ Estimad@ estudiante: ║
##║ DisfRute el ejemplo! ║
##║ El profesor     ╔════╝
##╚═════════════════╝
