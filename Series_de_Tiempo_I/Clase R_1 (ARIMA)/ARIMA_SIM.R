# Simulacion de procesos ARIMA(p,d,q)

#---------------------------------------------------------------------------
# Simulacion de procesos ARMA(1,1) at Normal
#---------------------------------------------------------------------------
# definicion de los parametros del proceso
numsim=500
theta0_arma11=5
phi1_arma11=0.5
theta1_arma11=0.7
var_RB_arma11=2


# simulacion de un proceso ARMA(1,1) con at N(0,2)
z_arma11=arima.sim(n = numsim, list(ar=phi1_arma11, ma = theta1_arma11), mean=theta0_arma11,  sd = sqrt(var_RB_arma11), n.start=50)

# grafico de la serie simulada
ts.plot(z_arma11)

# obtencion de la ACF y PACF y presentacion en un solo grafico
par(mfrow=c(2,1)) 
pacf(z_arma11, lag.max=100, ci=0)
acf(z_arma11, lag.max=100, ci=0)


#######################################################################################
#######################################################################################
###################### REPRESENTACION MA(infty) DE UN ARMA ############################
library(stats)
ARMAtoMA(ar=0.7, ma=-0.4, 50)

#######################################################################################
#######################################################################################


#---------------------------------------------------------------------------
# Simulacion de procesos ARIMA(0,1,0) wt Normal sin deriva
#---------------------------------------------------------------------------
# Definicion de los parametros del proceso
numsim=500
theta0_i=0
phi1_i=0
theta1_i=0
var_RB_i=2


# Simulacion de un proceso ARIMA(0,1,0) con wt N(0,2)
z_i <- arima.sim(n = numsim, list(order = c(0,1,0)), sd = sqrt(var_RB_i))
ts.plot(z_i)



# obtencion de la ACF y PACF y presentacion en un solo grafico
par(mfrow=c(2,1)) 
pacf(z_i, lag.max=30, ci=0)
acf(z_i, lag.max=30, ci=0)


#######################################################################################
#######################################################################################


#---------------------------------------------------------------------------
# Simulacion de procesos ARIMA(0,1,0) wt Normal con deriva
#---------------------------------------------------------------------------
# Definicion de los parametros del proceso
numsim=500
theta0_i=3
phi1_i=0
theta1_i=0
var_RB_i=50


# Simulacion de un proceso ARIMA(0,1,0) con wt N(0,2)
z_i <- arima.sim(n = numsim, list(order = c(0,1,0)), mean=theta0_i, sd = sqrt(var_RB_i))
ts.plot(z_i)

dz<-diff(z_i)
ts.plot(dz)
acf(dz, lag.max = 30)
pacf(dz, lag.max = 30)



#######################################################################################
#######################################################################################



#---------------------------------------------------------------------------
# Simulacion de procesos ARIMA(0,1,1) wt Normal
#---------------------------------------------------------------------------
# Definicion de los parametros del proceso
numsim=500
theta0_ima=1
phi1_ima=0
theta1_ima=0.7
var_RB_ima=2


# Simulacion de un proceso ARIMA(0,1,1) con wt N(0,2)
z_ima <- arima.sim(n = numsim, list(order = c(0,1,1), ma = theta1_ima), sd = sqrt(var_RB_ima))
ts.plot(z_ima)



# obtencion de la ACF y PACF y presentacion en un solo grafico
par(mfrow=c(2,1)) 
pacf(z_ima, lag.max=100, ci=0)
acf(z_ima, lag.max=100, ci=0)


dz_ima=diff(z_ima)

ts.plot(dz_ima)
acf(dz_ima)
pacf(dz_ima)


#######################################################################################
#######################################################################################



#---------------------------------------------------------------------------
# Simulacion de procesos ARIMA(1,1,0) wt Normal
#---------------------------------------------------------------------------
# Definicion de los parametros del proceso
numsim=500
theta0_ari=1
phi1_ari=0.6
theta1_ari=0
var_RB_ari=2


# Simulacion de un proceso ARIMA(0,1,1) con wt N(0,2)
z_ari <- arima.sim(n = numsim, list(order = c(1,1,0), ar = phi1_ari), sd = sqrt(var_RB_ari))
ts.plot(z_ari)



# obtencion de la ACF y PACF y presentacion en un solo grafico
par(mfrow=c(2,1)) 
pacf(z_ima, lag.max=100, ci=0)
acf(z_ima, lag.max=100, ci=0)


#######################################################################################
#######################################################################################



#---------------------------------------------------------------------------
# Simulacion de procesos ARIMA(1,1,1) wt Normal
#---------------------------------------------------------------------------
# Definicion de los parametros del proceso
numsim=500
theta0_arima=2
phi1_arima=0.6
theta1_arima=0.7
var_RB_arima=2


# Simulacion de un proceso ARIMA(0,1,1) con wt N(0,2)
z_arima <- arima.sim(n = numsim, list(order = c(1,1,1), ar = phi1_arima, ma=theta1_arima), sd = sqrt(var_RB_arima))
ts.plot(z_arima)



# obtencion de la ACF y PACF y presentacion en un solo grafico
par(mfrow=c(2,1)) 
pacf(z_arima, lag.max=100, ci=0)
acf(z_arima, lag.max=100, ci=0)


#######################################################################################
#######################################################################################
#EJEMPLO#

## GRAFICA ##
#############
z<-scan(dec=",")
z<-as.ts(z)
length(z)
ts.plot(z, main="Serie", ylab="");grid()
par(mfrow=c(1,2))
acf(z, lag.max=100, main="ACF")
pacf(z,lag.max=100, main="PACF")


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

par(mfrow=c(2,1))
acf(lz, lag.max = 500)
pacf(lz)

plot(z^(0.1188407))

########################
## PRIMERA DIFERENCIA ##
########################
rt<-diff(lz) # Retorno
plot(rt, ylab="", main="Log Retornos", type="l");grid()
par(mfrow=c(1,2))
acf(rt, lag.max=50, main="")
pacf(rt, lag.max=50, main="")

