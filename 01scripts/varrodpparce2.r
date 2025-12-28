##* ==================================================*#
##! Script: "varrodpparce2.r"
##+ In short: 'variables de rodal por parcela'       *#
##* Sobre:  Calcula variables de estado de rodal     *#
##  a partir de datos a nivel de arbol en varias    *#
##  parcelas de muestreo. El script se organiza como*#
##  sigue:                                          *#
##-  1. Calculo de densidad y area basal por parcela.*#
##+  2. Ajusta un modelo de altura-diametro          *#
##-  3. Predice altura, con modelo anterior, a los   *# 
##  arboles no-muestra.                             *#
##-  4. Predice el volumen individual, con un modelo*#
##  para la especie                                 *# 
##  arboles no-muestra.                             *#
##-  5. Calculo de alt. media y volumen por parcela. *#
##                                                  *#
##> Profesor: Christian Salas Eljatib                *#
## E-mail: cseljatib AT gmail DOT com               *#
##? Web: https://eljatib.com                         *#
##* ==================================================*#
#~~~~~~~
#Datos a emplear
library(biometrics)
head(radiatapl2)
#?radiatapl2
#~~~~~~~

sup.plantacion<-45 #ha
#asignacion de dataframe a objeto "df"
df<-radiatapl2

#~~~~
#revisando los datos
str(df)
head(df)
names(df)

#~~~~
#cuantas arboles hay?
n.arb <- nrow(df)

#~~~~
#cuantas parcelas hay?
unique(df$parce)
table(df$parce)
#en que se diferencian las dos previas lineas?

n.parcelas<-length(unique(df$parce))
n.parcelas
sup.parce<-150

#==============================
#1) Calculo de N y G por parcela
#==============================
#1a) densidad por parcela
#i. factor de expansion
df$fe <- 10000/sup.parce
head(df)

#ii. la densidad a nivel "de rodal/ha" (N), por parcela
nha.ppar<-tapply(df$fe,df$parce,sum)
nha.ppar

nha.med<-mean(nha.ppar)

#1b) area basal por parcela
#i. area basal de cada arbol
df$g <- (pi/40000)*df$dap^2
#ii. area basal de cada arbol expandida a la hectarea
df$garb.ha <- df$g * df$fe
#iii. area basal por parcela
gha.ppar<-tapply(df$garb.ha,df$parce,sum)
gha.ppar
gha.med<-mean(gha.ppar)

#==============================
#1c) Creando una dataframe con los resultados
# de Variables de estado de rodal por parcela
#==============================
#tenemos los siguientes resultados, y los 
# asignaremos a objetos con nombres estandares
N<-nha.ppar;G<-gha.ppar;
Dg<-dg.fx(g=G,n=N)
Dg
dg.med<-mean(Dg)

varest.ppar<-data.frame(N,G,Dg)
varest.ppar

tab.out<-varest.ppar
row.names(tab.out)
Parcela <- row.names(tab.out)
Parcela
tab.out<-cbind(Parcela,varest.ppar)
tab.out

library(datana)
descstat(tab.out[,c("N","G","Dg")],3)


#==============================
#2) Ajuste modelo altura-diametro
#==============================
#datos...
descstat(df[,c("dap","atot")])
#5a) Ajuste de modelo de altura
#solo arboles con registro de altura
df1 <- na.omit(df)
head(df1)
#grafico de dispersion arboles muestra
plot(atot~dap,data=df1)
plot(atot~dap,data=df1,xlab="Diametro (cm)",ylab="Altura (m)",
     las=1,col="blue")
#el efecto de la escala en los graficos, compare lo siguiente
plot(atot~dap,data=df1,xlim=c(0,30),     ylim=c(0,25))

n.ajumodh<-nrow(df1); n.ajumodh

##- Recuerde, dos formas de ajustar un modelo estadistico
## h_i=beta_0 + beta_1 ln(1/(1+(d/10)))+varepsilon_i
##+ (i) alternativa que crea columnas del modelo en los datos
df1$transf.d<-log(1/(1+(df1$dap/10)))
m1.largo<- lm(atot~transf.d,data=df1)
summary(m1.largo)
sigma.hat.1<-summary(m1.largo)$sigma
e.media.gral<-100*sigma.hat.1/mean(df1$atot)
e.media.gral

##+ (ii) alternativa que no necesita que se creen
# las columnas del modelo en los datos
m1 <- lm(atot ~ I(log(1/(1+(dap/10)))), data=df1)
summary(m1)

#==============================
#6) Predecir altura a arboles no muestra
#==============================
#predecir las alturas con el modelo
##modelo dendrometrico: conservar las alturas medidas, y solo
# predecir a las observaciones no medidas.
#como logramos lo anterior?
df$h.aju <- predict(m1, newdata = df)
summary(df$h.aju)
df$h.final <- df$h.aju

#porcion de arboles donde no se midio la altura
df0 <- df[is.na(df$atot),]
dim(df0)
head(df0)
tail(df0)
#porcion de arboles donde si se midio la altura
df1 <- df[!is.na(df$atot),]
dim(df1)
head(df1)
df1$h.final <- df1$atot
#finalmente tenemos el tree list o listado de arboles con todas las variables
# necesarias
trl <- rbind(df0,df1)
dim(trl)

###termino de prediccion segun modelo dendrometrico
# establecido para la altura

summary(trl$h.final)

#==============================
#7. Calculo de alt. media y volumen por parcela.
#==============================
#7a). altura media (Hm) por parcela
hmed.ppar<-tapply(trl$h.final,trl$parce,mean)
hmed.ppar
h.med<-mean(hmed.ppar)

#7b) volumen por parcela
#i. estimacion de volumen (alternativa simple, factor de forma)
ff <- 0.3 #factor de forma
#ii. calculo del volumen de cada arbol
trl$v <- trl$g*trl$h.final*ff
#iii. volumen de cada arbol expandido a la hectarea
trl$varb.ha <- trl$v * trl$fe
#iv. volumen basal por parcela
vha.ppar<-tapply(trl$varb.ha,trl$parce,sum)
vha.ppar
vha.med<-mean(vha.ppar)

#variables de estado de rodal por parcela
Hm<-hmed.ppar;V<-vha.ppar
varrod.ppar<-cbind(tab.out,Hm,V)
varrod.ppar

descstat(varrod.ppar[,2:ncol(varrod.ppar)])


##+ ===============================
##*Tarea, preguntas, y asi reforzar lo estudiado:
##- Si se varia el modelo dendrometrico, y se emplea el modelo 
## ajustado de altura, para todos los arboles del muestreo. Como varia
## la estimacion del volumen por parcela, y el volumen medio de
## todas las parcelas, con respecto a los obtenidos hasta ahora.
##- Ajuste un modelo lineal simple de altura (h=b0+b1(1/d)), y
## compare sus resultados a nivel del muestreo con los obtenidos
## hasta ahora. Por ejemplo, la altura media de cada parcela entre
## los dos modelos ajustados. 
##- Calcule la altura dominante para cada parcela.
##- En cuanto varia el volumen agregado (V), si se emplea el
## siguiente modelo de volumen
## v=b0+b1*(d^2*h), donde
## b0.hat=0.0195213, b1.hat=0.0000282, d es el dap en cm, h es la
## altura total en m, y v es el volumen total en m3scc.
##+ ===============================

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
