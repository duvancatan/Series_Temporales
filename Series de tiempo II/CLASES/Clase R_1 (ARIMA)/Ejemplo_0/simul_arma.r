# En R el proceso ARMA(p,q) con media cero esta definido por:
# X[t] = a[1]X[t-1] + ... + a[p]X[t-p] + e[t] + b[1]e[t-1] + ... + b[q]e[t-q]
#
# Simulacion de procesos Autorregresivos
# numero de observaciones a simular
numsim=500
#---------------------------------------------------------------------------
# simulacion de un proceso AR(1) con at Normal
#---------------------------------------------------------------------------
# definicion de los parametros del proceso
theta0_ar1=5 #Media
phi1_ar1=0.8 #Parametro autorregresivo
var_RB_ar1=2 #Var del RB

# Chequeo de la estacionaridad del modelo: usa la libreria fArma
library(fArma)
coefar1=c(phi1_ar1)
armaRoots(coefar1)

# Simulacion: usa la libreria stats
z_ar1=arima.sim(n = numsim, list(ar = phi1_ar1), mean=theta0_ar1, sd = sqrt(var_RB_ar1), n.start=50)

# Grafico de la serie simulada
plot.ts(z_ar1)

# Obtencion de la ACF y PACF y presentacion en un solo grafico
library(astsa)
acf2(z_ar1) #Graficar simultaneamente ACF y PACF

pacf(z_ar1, lag.max=15)
acf(z_ar1, lag.max=15, ci=0)


# Verdadera ACF y PACF: usa la libreria fArma 
model_ar1 = list(ar = phi1_ar1)
par(mfrow=c(2,1)) 
armaTrueacf(model_ar1, lag.max=15, type = c("both"))

# ACF y PACF verdaderas y muestrales
par(mfrow=c(1,4)) 
armaTrueacf(model_ar1, lag.max=15, type = c("both"))
pacf(z_ar1, lag.max=15, ci=0, main="PACF Muestral")
acf(z_ar1, lag.max=15, ci=0, main="ACF Muestral")


#---------------------------------------------------------------------------
# Simulacion de un proceso AR(2) con at Normal
#---------------------------------------------------------------------------
# Definicion de los parametros del proceso
theta0_ar2=5 # Media
phi1_ar2=0.8 # Parametro autorregresivo 1 
phi2_ar2=-0.65 # Parametro autorregresivo 2
var_RB_ar2=2 # Var del RB

# Chequeo de la estacionaridad del modelo
coefar2=c(phi1_ar2, phi2_ar2)
armaRoots(coefar2)

# Simulacion
z_ar2=arima.sim(n = numsim, list(ar = c(phi1_ar2, phi2_ar2)), mean=theta0_ar2, sd = sqrt(var_RB_ar2), n.start=20)

# Grafico de la serie simulada
ts.plot(z_ar2)

# Obtencion de la ACF y PACF y presentacion en un solo grafico
par(mfrow=c(2,1)) 
pacf(z_ar2, lag.max=15, ci=0)
acf(z_ar2, lag.max=15, ci=0)

# Verdadera ACF y PACF 
model_ar2 = list(ar = c(phi1_ar2, phi2_ar2))
par(mfrow=c(2,1)) 
armaTrueacf(model_ar2, lag.max=15, type = c("both"))

############################################################################
#---------------------------------------------------------------------------
# Simulacion de un proceso AR(3) con at Normal
#---------------------------------------------------------------------------
# Definicion de los parametros del proceso
theta0_ar2=5   # Media
phi1_ar2=0     # Parametro autorregresivo 1 
phi2_ar2=0    # Parametro autorregresivo 2
phi2_ar3=0.7   # Parametro autorregresivo 3
var_RB_ar2=2   # Var del RB

# Chequeo de la estacionaridad del modelo
coefar3=c(phi1_ar2, phi2_ar2, phi2_ar3)
armaRoots(coefar3)

# Simulacion
z_ar3=arima.sim(n = numsim, list(ar = c(phi1_ar2, phi2_ar2, phi2_ar3)), mean=theta0_ar2, sd = sqrt(var_RB_ar2), n.start=20)

# Grafico de la serie simulada
ts.plot(z_ar3)

# Obtencion de la ACF y PACF y presentacion en un solo grafico
par(mfrow=c(2,1)) 
pacf(z_ar3, lag.max=15)
acf(z_ar3, lag.max=15, ci=0)


# Verdadera ACF y PACF 
model_ar3 = list(ar = c(phi1_ar2, phi2_ar2, phi2_ar3))
par(mfrow=c(2,1)) 
armaTrueacf(model_ar3, lag.max=15, type = c("both"))
############################################################################
############################################################################
############################################################################

# Simulacion de procesos de Medias Moviles
#---------------------------------------------------------------------------
# simulacion de un proceso MA(1) con at Normal
#---------------------------------------------------------------------------
# Definicion de los parametros del proceso
theta0_ma1=5
theta1_ma1=0.7
var_RB_ma1=2

# chequeo de la invertibilidad del modelo
coefma1=c(theta1_ma1)
armaRoots(coefma1)

# simulacion
z_ma1=arima.sim(n = numsim, list(ma = theta1_ma1), mean=theta0_ma1, sd = sqrt(var_RB_ma1), n.start=50)
summary(z_ma1)

# grafico de la serie simulada
ts.plot(z_ma1)

# obtenci?n de la ACF y PACF y presentaci?n en un solo gr?fico
par(mfrow=c(2,1)) 
pacf(z_ma1, lag.max=15, ci=0)
acf(z_ma1, lag.max=15, ci=0)

# verdadera ACF y PACF 
model_ma1 = list(ma = theta1_ma1)
par(mfrow=c(2,1))
armaTrueacf(model_ma1, lag.max=15, type = c("both"))

#---------------------------------------------------------------------------
# simulacion de un proceso MA(2) con at Normal
#---------------------------------------------------------------------------
# definicion de los par?metros del proceso
theta0_ma2=5
theta1_ma2=0.4
theta2_ma2=-0.3
var_RB_ma2=2

# chequeo de la invertibilidad del modelo
coefma2=c(theta1_ma2, theta2_ma2)
armaRoots(coefma2)

# simulacion
z_ma2=arima.sim(n = numsim, list(ma = c(theta1_ma2, theta2_ma2)), mean=theta0_ma2, sd = sqrt(var_RB_ma2), n.start=50)

# grafico de la serie simulada
ts.plot(z_ma2)

# obtencion de la ACF y PACF y presentacion en un solo grafico
par(mfrow=c(2,1)) 
pacf(z_ma2, lag.max=15, ci=0)
acf(z_ma2, lag.max=15, ci=0)

# verdadera ACF y PACF 
model_ma2 = list(ma = c(theta1_ma2, theta2_ma2))
par(mfrow=c(2,1)) 
armaTrueacf(model_ma2, lag.max=15, type = c("both"))

#---------------------------------------------------------------------------
# Simulacion de procesos ARMA(1,1) at Normal
#---------------------------------------------------------------------------
# definicion de los parametros del proceso
theta0_arma11=5
phi1_arma11=0.5
theta1_arma11=0.7
var_RB_arma11=2

# chequeo de la estacionaridad del modelo
coef_ar=phi1_arma11
armaRoots(coef_ar)
# chequeo de la invertibilidad del modelo
coefma=theta1_arma11
armaRoots(coefma)

# simulacion de un proceso ARMA(1,1) con at N(0,2)
z_arma11=arima.sim(n = numsim, list(ar=phi1_arma11, ma = theta1_arma11), mean=theta0_arma11,  sd = sqrt(var_RB_arma11), n.start=50)

# grafico de la serie simulada
ts.plot(z_arma11)

# obtencion de la ACF y PACF y presentacion en un solo grafico
par(mfrow=c(2,1)) 
pacf(z_arma11, lag.max=15, ci=0)
acf(z_arma11, lag.max=15, ci=0)


# verdadera ACF y PACF 
model_arma11 = list(ar=phi1_arma11, ma = theta1_arma11)
par(mfrow=c(1,2)) 
armaTrueacf(model_arma11, lag.max=15, type = c("both"))
pacf(z_arma11, lag.max=15, ci=0)
acf(z_arma11, lag.max=15, ci=0)

#######################################################################################
#######################################################################################
###################### REPRESENTACION MA(infty) DE UN ARMA ############################
library(stats)
ARMAtoMA(ar=0.7, ma=-0.4, 10)

#######################################################################################
#######################################################################################





