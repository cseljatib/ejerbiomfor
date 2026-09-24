## Script: "compe1.r"                        /
## Sobre: Calculo del BAL                         /
## BAL es el area basal en los arboles de mayor  /
##  diametro que el sujeto.                     /
#                                              /
#---------------------------------------------/ 
##
## Profesor: Christian Salas Eljatib
## E-mail: cseljatib AT gmail DOT com
## Web: https://eljatib.com
#======================================

## I. Datos
##@@@Atencion en esta seccion de lectura de los datos
###Ud. debe activar la siguiente linea
#dbh<-read.csv("dlist.csv")
#y desactivar esta
df <- read.csv(paste(data.dir,"/dlist.csv",sep=""))
head(df)
##@@@Fin a seccion de lectura de los datos

sup.parce <- 250
df$g <- (pi/40000) * (df$dbh^2)

#distribucion diametrica
hist(df$dbh)

#distribucion del area basal
hist(df$g)

#expandiendo a la hectarea
df$fe <- 10000/sup.parce
df$g.ha <- df$g*df$fe
df0<-df[,c("dbh","g","g.ha")]
####
df2 <- df0[with(df0, order(g.ha)),]
df2

#el area basal acumulada 
df2$acu.g.ha <-cumsum(df2$g.ha)
#el area basal del rodal, en m2/ha
gha <- max(df2$acu.g.ha); gha
#lo mismo se puede obtener con
sum(df2$g.ha)
#proporcion del area basal acumulada-ordenada
df2$p.acu.g.ha <- df2$acu.g.ha/gha
#el BAL
df2$bal <- gha*(1-df2$p.acu.g.ha)
#todo ordenado
df2


####
plot(bal~dbh,data=df2,type="b",las=1)
abline(h=0,col="gray")
abline(h=gha,col="gray",lty=2)

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
