#EJEMPLO 1# # ARCH(2)

serie1<-read.table("rt1.txt", dec=".", sep="")
rt1<-serie1$V2
plot.ts(rt1)

#Modelo Media Condicional#
par(mfrow=c(2,1))
acf(rt1)
pacf(rt1)

med1<-arima(rt1, c(4,0,0))
at1<-med1$residuals

acf(at1)
pacf(at1)



#Cuadrado de los Retornos#
par(mfrow=c(2,1))
acf(at1^2)
pacf(at1^2)

#Ajustes de Modelos# Distribucion Normal
M1n<-garchFit(~arma(4,0)+garch(1,0), data=rt1)
M2n<-garchFit(~arma(4,0)+garch(2,0), data=rt1)

summary(M1n)
M1n@

predict(M1n, n.ahead = 2)
predict(M2n, n.ahead = 2)


#Ajustes de Modelos# Distribucion t
M1t<-garchFit(~arma(4,0)+garch(1,0), data=rt1, cond.dist='std')
M2t<-garchFit(~arma(4,0)+garch(2,0), data=rt1, cond.dist='std')

summary(M1t)

M1t@fit
plot.ts(M1t@sigma.t) # Volatilidades estimadas
plot.ts(rt1)

plot.ts(M2t@sigma.t) # Volatilidades estimadas
plot.ts(rt1)







#EJEMPLO 2#
# ARMA(1,2)-GARCH(1,1) - list(ar = 0.5, ma = c(0.3, -0.3))

serie2<-read.table("rt3.txt", dec=".", sep="")

rt2<-serie2$V2
plot.ts(rt2)

#Media Condicional#
acf(rt2)
pacf(rt2)

acf(rt2^2)
pacf(rt2^2)



#Modelo ARMA-GARCH
mod2<-garchFit(~arma(1,2)+garch(1,1))
summary(mod2)
mod2@fit
plot.ts(mod2@sigma.t)
plot.ts(rt2)

predict(mod2, 4)






#EJEMPLO 3# OPEN

serie3<-scan(dec=",")
plot.ts(serie3)
rt3<-returns(serie3)
plot.ts(rt3)

rt3<-na.omit(rt3)
par(mfrow=c(2,1))
acf(rt3)
pacf(rt3)


acf(rt3^2)
pacf(rt3^2)


M31<-?garchFit(~garch(3,0), data=rt3)
M32<-garchFit(~garch(3,0), data=rt3, cond.dist='std')

summary(M31)
M31@fit

summary(M32)
M32@fit

predict(M32, 5)
plot.ts(M32@residuals)







#EJEMPLO 4#
dir()
intel<-read.table("intel_diario.txt", dec=".", sep="")
names(intel)
rt<-intel$V1 
plot.ts(rt)

acf(rt)
pacf(rt)



