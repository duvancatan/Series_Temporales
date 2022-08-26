########################
####### EJEMPLO ########
########################
library(astsa)
plot.ts(varve)
length(varve)

#Logaritmo
plot.ts(log(varve))
X = acf(log(varve), 100, plot=FALSE)


#Quitanto la media
library(fracdiff)
lvarve = log(varve)-mean(log(varve));plot.ts(log(varve)-mean(log(varve)))
plot.ts(lvarve)

#ACF y PACF
ac = acf(log(varve), 100, plot=FALSE, ci=0)
par(mfrow=c(2,1))
plot(ac[1:100], ylim=c(-.1,1), main="log(varve)", ci=0) # Quitar lag 0
pacf(log(varve), lag.max = 100)

#Estimacion
varve.fd = fracdiff(lvarve, nar=0, nma=0, M=30)
summary(varve.fd)
varve.fd$d


#Comparacion ARFIMA y ARIMA
res.fd = diffseries(lvarve, varve.fd$d) # Residuo Fraccional
res.arima = resid(arima(lvarve, order=c(0,1,1))) # Residuo arima(1,1,1)

par(mfrow=c(2,1))
#RESIDUOS DE ARIMA#
acf(res.arima, 100, xlim=c(4,97), ylim=c(-.2,.2), main="ARIMA")
pacf(res.arima, 100, xlim=c(4,97), ylim=c(-.2,.2), main="ARIMA")

#RESIDUOS DE ARFIMA#
acf(res.fd, 100, xlim=c(4,97), ylim=c(-.2,.2), main="ARFIMA")
pacf(res.fd, 100, xlim=c(4,97), ylim=c(-.2,.2), main="ARFIMA")


