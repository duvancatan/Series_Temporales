#############################
#### HECHOS ESTILIZADOS #####
#############################

#EJEMPLO 1#
#####################
#Grafica de la Serie#
#####################
IBV=read.csv(file="ibv.csv",header=T, sep=";")
View(IBV)
attach(IBV)
head(IBV)
par(mfrow=c(1,2))
rt<-diff(log(Close.ts))
plot(Close.ts, ylab="Cierre", xlab="", main="Serie Original de Cierre")
plot.ts(rt, ylab="Log-Retornos", xlab="",axes=T)

#Autocorrelaciones
par(mfrow=c(1,2))
acf(rt,ylab="FAC", xlab="lag")
pacf(rt,ylab="FAC", xlab="lag")

#Distribucion
par(mfrow=c(2,2))
qqnorm(rt,xlab="Quantis Te??ricos",ylab="Quantis Amostrais",main="Q-Q plot dos log-retornos")
qqline(rt, col="red")
hist(rt,freq=F, col="green",breaks=20, ylim=c(0,30),main="Histograma dos log-retornos", ylab="Densidade", xlab="log-retornos")
plot(function(x) dnorm(x,mean(rt,na.rm=T),sd(rt,na.rm=T)),min(rt,na.rm=T),max(rt,na.rm=T),add=T)
legend("topright",c("Normal"),lty=1,cex=0.75)

#Medidas
summary(rt)
mean(rt)
library(e1071)                    
kurtosis(rt)              
skewness(rt)

#Autocorrelaciones del cuadrado de los retornos
par(mfrow=c(1,2))
acf(rt^2, main="ACF Cuadrado de Retornos")
pacf(rt^2, main="PACF Cuadrado de Retornos")

#################################################################
#################################################################
#################################################################

#EJEMPLO 2#
#####################
#Grafica de la Serie#
#####################

s1<-ts(Open)
plot.ts(s1, main="Serie Open"); grid()
summary(s1)
library(car)
(s1_BC=powerTransform(s1))
Trans<-boxcox(s1 ~ 1, lambda = seq(-2, 2, 1/10), plotit = TRUE, xlab = expression(lambda),ylab = "log-Likelihood")
S1<-s1


#Calcular os log-retornos
lrS1<-diff(log(S1))
plot.ts(lrS1)


#Para lrS1 
par(mfrow=c(2,2))
plot.ts(S1)
plot.ts(lrS1)
qqnorm(lrS1);qqline(lrS1, col = 2)
#truehist()libreria MASS
densidad=density(lrS1) #Para esto me sirvi?? el attach para llamar Price#
truehist(lrS1)
lines(densidad,col="red",lwd=2) #Sobrepone la densidad, le coloca esa aproximaci??n roja#

summary(lrS1)
var(lrS1)
sd(lrS1)
(cv=sd(lrS1)/abs(mean(lrS1)))
skewness(lrS1)
kurtosis(lrS1)
