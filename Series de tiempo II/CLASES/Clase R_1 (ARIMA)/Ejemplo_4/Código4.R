#############
## GRAFICA ##
#############
z<-scan(dec=",")
z<-as.ts(z)
length(z)
ts.plot(z, main="Serie", ylab="");grid()
par(mfrow=c(1,2))
acf(z, lag.max=500, main="ACF")
pacf(z,lag.max=500, main="PACF")

#############################################
## SI ES NECESARIO ESTABILIZAR LA VARIANZA ##
#############################################
library(MASS)
boxcox(z ~ 1, lambda = seq(-2, 2, 1/10), plotit = TRUE, xlab = expression(lambda),
       ylab = "log-Likelihood")
library(car)
(tbc=powerTransform(z))

lz<-log(z)
ts.plot(lz)


########################
## PRIMERA DIFERENCIA ##
########################
rt<-diff(lz)
plot(rt, ylab="", main="Log Retornos");grid()
par(mfrow=c(1,2))
acf(rt, lag.max=50, main="")
pacf(rt, lag.max=50, main="")




#######################################
## ESTIMACION DE PARAMETROS IMA(1,1) ##
#######################################

# Estimacion ML exacta#
mod_1=arima(rt, c(15, 0 ,0), method = c("ML"), fixed = c(NA,NA,NA,0,0,0,0,0,0,NA,0,0,0,0,NA,NA))
res_1=residuals(mod_1)


################################################## 
## PRUEBAS DIAGNOSTICO - ANALISIS DE RESIDUALES ##
##################################################
par(mfrow=c(3,2))
plot.ts(res_1, main="Residuales", ylab="")
hist(res_1, main="Histograma de Residuales")
acf(res_1, lag.max=25, main="ACF de Residuales")
pacf(res_1, lag.max=25, main="PACF de Residuales")

Box.Test = function(x, lag = 25, main = "p values for Ljung-Box statistic"){
  B<-vector("numeric")
  for(i in 1:lag){
    B[i]<-Box.test(x, lag = i, type = "Ljung-Box")$p.value
  }
  A<-matrix(cbind(c(1:lag),B), nrow = lag, ncol = 2, byrow=F, dimnames = list(NULL, c("lag", "p.value")))
  plot(A[,1], A[,2], ylim = c(0, max(0.051,(max(A[,2])+.01))), 
       ylab = "p-value", xlab = "Lag", main = main, lwd = 2)
  abline(0.05, 0, col = 4, lty = 2)
}
Box.Test(res_1)

shapiro.test(res_1)
qqnorm(res_1, main="QQ de los Residuales")
qqline(res_1)

