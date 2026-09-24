## Script: "compe2.r"            /
## Sobre: Calculo del BAL, usando una funcion.   /
## BAL es el area basal en los arboles de mayor diametro que el sujeto.
#
#------------------------------------------------------------/ 
##
## Profesor: Christian Salas Eljatib
## E-mail: cseljatib AT gmail DOT com
## Web: https://eljatib.com
#======================================

##@@@Atencion en esta seccion de lectura de los datos
###Ud. debe activar la siguiente linea
#dbh<-read.csv("dlist.csv")
#y desactivar esta
df <- read.csv(paste(data.dir,"/dlist.csv",sep=""))
head(df)
##@@@Fin a seccion de lectura de los datos

##@@@Atencion en esta seccion sobre cargar un script con funciones
###Ud. debe activar la siguiente linea
#source("fxBal.R")
#y desactivar esta
source(paste(r.dir,"/fxBal.r",sep=""))
##@@@Fin a seccion de cargado de un script

plotsize <- 250
df$g <- (pi/40000) * (df$dbh^2)
df$fe <- 10000/plotsize
df$xarea <- 1/df$fe
df$garb.ha <- df$g*df$fe
head(df)
gha<-sum(df$garb.ha); gha
#aplicando la funcion "bal" para calcular el bal
df$bal <- bal(df$g, df$xarea)
head(df)
####
#datos ordenados segun el area basal de cada arbol
df0 <- df[with(df, order(g)),]
df0

plot(bal~dbh,data=df0,type="b",las=1)
abline(h=0,col="gray")
abline(h=gha,col="gray",lty=2)

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
