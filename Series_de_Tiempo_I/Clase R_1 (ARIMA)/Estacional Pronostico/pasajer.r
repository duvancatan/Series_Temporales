# Lectura de los datos
(pasaj = ts(scan("pasaj.txt"), start=1949, frequency=12))

# Graficar la serie
ts.plot(pasaj, ylab="Pasajeros", main="N?mero de pasajeros")

# Tabla Buys-Ballot
library(pastecs)
buysbal(pasaj, y=NULL, frequency=12,dateformat="m/Y")

# Graficos estacionales para observar la evolucion de la componente estacional
# usando las ibrer?as stats, forecast, uroot
library(forecast)
par(mfrow=c(3,1))
ts.plot(pasaj,  main="Numero de pasajeros")
monthplot(pasaj, main="Numero de pasajeros para cada mes con el promedio")  #stats
seasonplot(pasaj, main="Perfil estacional")

# Transformacion de Box y Cox
# Alternativa 1
library(car, help)
(pasaj_BC=powerTransform(pasaj))

# Alternativa 2
library(MASS, help)
boxcox(pasaj ~ 1, lambda = seq(-2, 2, 1/10), plotit = TRUE, xlab = expression(lambda),
            ylab = "log-Likelihood")

# La transformacion logaritmica estabiliza la varianza
pasajt=log(pasaj)

# Graficar serie original y en logaritmos
par(mfrow=c(2,1))
plot(pasaj, main="Serie Original") 
plot(log(pasaj), main="Logaritmo de la serie") 

# Graficos estacionales
par(mfrow=c(3,1))
ts.plot(pasajt,  main="Log N?mero de pasajeros")
monthplot(pasajt, main="Log N?mero de pasajeros para cada mes con el promedio")
seasonplot(pasajt, main="Log Perfil estacional")

# ACF y PACF muestrales en un solo grafico
par(mfrow=c(2,1)) 
acf(pasajt, lag.max=48)     # ACF hasta el rezago 48
pacf(pasajt, lag.max=48)    # PACF hasta el rezago 48

# ACF y PACF muestrales de la primera diferencia ordnaria, (1-B)
par(mfrow=c(2,1)) 
acf(diff(pasajt), lag.max=48)     # ACF hasta el rezago 48
pacf(diff(pasajt), lag.max=48)    # PACF hasta el rezago 48

# ACF y PACF muestrales de la primera diferencia estacional (1-B^12)
par(mfrow=c(2,1)) 
acf(diff(pasajt, lag=12), lag.max=48)     # ACF hasta el rezago 48
pacf(diff(pasajt, lag=12), lag.max=48)    # PACF hasta el rezago 48

# ACF y PACF muestrales de (1-B)(1-B^12)
d1pasajt=diff(pasajt)
par(mfrow=c(2,1)) 
acf(diff(d1pasajt, lag=12), 48)     # ACF hasta el rezago 48
pacf(diff(d1pasajt, lag=12), 48, ci=0)    # PACF hasta el rezago 48
# Conclusion: Hay evidencia de un arima(0,1,1)X(0,1,1) con S=12

# Identificacion usando criterios de informacion
# entre los ordenes maximos de los polinomios
p=1
q=1
P=1
Q=1

# Entre los ordenes de diferenciacion
d=1
D=1
(maxfilas=(p+1)*(q+1)*(P+1)*(Q+1))
ic_mod=matrix(rep(-99, times=maxfilas*6), nrow=maxfilas, ncol=6)

k=1
for(i in 0:p) {
for(j in 0:q) {
for(s in 0:P) {
for(m in 0:Q) {
modelo=Arima(pasajt, order = c(i, d, j),
     seasonal = list(order = c(s, D, m)), method = c("CSS-ML"))

     numpar=length(coef(modelo))
     T=length(residuals(modelo))
     ic_mod[k,1]=i
     ic_mod[k,2]=j
     ic_mod[k,3]=s
     ic_mod[k,4]=m
     ic_mod[k,5]=-2*(modelo$loglik/T)+(2*numpar)/T      # AIC
     ic_mod[k,6]=-2*(modelo$loglik/T)+(numpar*log(T))/T  # SIC
k=k+1
}
}
}
}
p_=ic_mod[,1]
q_=ic_mod[,2]
P_=ic_mod[,3]
Q_=ic_mod[,4]
AIC=ic_mod[,5]
SIC=ic_mod[,6]

# Tabla con ordenes de los modelos ajustados y los criterios de informacion
# ---------------------------------------------------------------------------
(Crit_Inf=cbind(p_, q_, P_, Q_, AIC, SIC))

# ordenamiento de los criterios de informaci?n e menor a mayor
Crit_Inf=data.frame(Crit_Inf)
(aic=Crit_Inf[order(Crit_Inf$AIC),])
(sic=Crit_Inf[order(Crit_Inf$SIC),])
# Conclusion: De nuevo hay evidencia de un arima(0,1,1)X(0,1,1) con S=12

# Especificaci?n y estimacion del modelo
(mod1=arima(pasajt, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12),
      method = c("CSS-ML")))


# Algunos otros diagnosticos
# chequeo si los residuales son ruido blanco y posibles observaciones extremas
tsdiag(mod1)

# Otra forma de verificar si los residuales son ruido blanco
resmod1=residuals(mod1)
par(mfrow=c(2,1)) 
acf(resmod1)
pacf(resmod1)

# Prueba de Ljung-Box
Box.test(resmod1, lag = 36, type = "Ljung-Box")  # hay que corregir los grados
                                              # de libertad como 24-# de par?metros
                                              # Arma del modelo estimado
# Calculo del valor cr?tico para la prueba de Ljung-Box
qchisq(.95, df=34)

# Chequeo preliminar de observaciones at?picas
# estandarizacion de los residuales
res_est=resmod1/(mod1$sigma2^.5)
res_est=mod1$residuals
plot.ts(res_est)

# Chequeo de normalidad
# grafico cuantil-cuantil
qqnorm(res_est,  xlab = "Cuantiles Te?reticos", ylab = "Cuantiles Muestrales")
qqline(res_est)
 

# Prueba de normalidad
shapiro.test(res_est)                #prueba de Shapiro-Wilks
# Conclusio: Se puede no rechazar normalidad a un nivel de significancia de 0.05.

# Valores ajustados
ajust=pasajt-resmod1

# Grafico para los valores ajustados
ts.plot(pasajt, ajust)
lines(pasajt, col="black")
lines(ajust, col="red")

# Pron?sticos para la serie transformada
(pasajt.pred=predict(mod1, n.ahead = 24, se.fit = TRUE))

# Grafico
plot(pasajt,xlim=c(1949,1962), ylim=c(4,7))
pasajt.pred<-predict(mod1,n.ahead=24)
lines(pasajt.pred$pred,col="blue")
lines(pasajt.pred$pred+2*pasajt.pred$se,col="red",lty=3)
lines(pasajt.pred$pred-2*pasajt.pred$se,col="red",lty=3)

# Intervalos de prediccion para la serie transformada
pronost=pasajt.pred$pred
pronost
stdev=pasajt.pred$se
stdev
liminft=pronost-2*stdev
limsupt=pronost+2*stdev

# Pronosticos e intervalos de prediccion para la serie original
# con correccion por sesgos de retransformacion
pronos_orig=exp(pronost+.5*stdev^2)
pronos_orig
liminf=exp(liminft+.5*stdev^2)
limsup=exp(limsupt+.5*stdev^2)
ts.plot(pasaj, pronos_orig, liminf, limsup) 
lines(pasaj,col="black")
lines(pronos_orig,col="blue")
lines(liminf,col="red",lty=3)
lines(limsup,col="red",lty=3)

# Listado de los pron?sticos e intervalos de prediccion
pronosticos=cbind(pronos_orig, liminf, limsup)
pronosticos




