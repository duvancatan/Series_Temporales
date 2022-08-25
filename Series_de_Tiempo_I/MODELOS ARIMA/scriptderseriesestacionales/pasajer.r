# lectura de los datos
# para leer los datos del archivo, colocarlos en un objeto de series de tiempo
# con calendario mensual y listarlos simultáneamente:
(pasaj = ts(scan("pasaj.txt"), start=1949, frequency=12))

# Si los datos no comenzaran en el primer mes, sino en el tercero de 1949,
# entonces se pueden leer de la siguiente forma:  ts(x, start=c(1949,3), frequency=12)

# para graficar la serie
ts.plot(pasaj, ylab="Pasajeros", main="Número de pasajeros")

# otro gráfico de la serie
plot(pasaj, type="o", col="blue", lty="dashed")

# tabla Buys-Ballot
library(pastecs)
buysbal(pasaj, y=NULL, frequency=12,dateformat="m/Y")

# Gráficos estacionales para observar la evolución de la componente estacional
# usando las ibrerías stats, forecast, uroot
library(forecast)
par(mfrow=c(3,1))
ts.plot(pasaj,  main="Número de pasajeros")
monthplot(pasaj, main="Número de pasajeros para cada mes con el promedio")  #stats
seasonplot(pasaj, main="Perfil estacional")

# transformación de Box y Cox
# alternativa 1
library(car, help)
(pasaj_BC=powerTransform(pasaj))

# alternativa 2
library(MASS, help)
boxcox(pasaj ~ 1, lambda = seq(-2, 2, 1/10), plotit = TRUE, xlab = expression(lambda),
            ylab = "log-Likelihood")

# La transformación logaritmica estabiliza la varianza
pasajt=log(pasaj)

# para graficar la serie en logaritmos
plot(log(pasaj), main="logaritmo de la serie") 

# gráficos estacionales
par(mfrow=c(3,1))
ts.plot(pasajt,  main="Log Número de pasajeros")
monthplot(pasajt, main="Log Número de pasajeros para cada mes con el promedio")
seasonplot(pasajt, main="Log Perfil estacional")

# ACF y PACF muestrales en un solo gráfico
par(mfrow=c(2,1)) 
acf(pasajt, 48, ci=0)     # ACF hasta el rezago 48
pacf(pasajt, 48, ci=0)    # PACF hasta el rezago 48

# ACF y PACF muestrales de la primera diferencia ordnaria, (1-B)
par(mfrow=c(2,1)) 
acf(diff(pasajt), 48, ci=0)     # ACF hasta el rezago 48
pacf(diff(pasajt), 48, ci=0)    # PACF hasta el rezago 48

# ACF y PACF muestrales de la primera diferencia estacional (1-B^12)
par(mfrow=c(2,1)) 
acf(diff(pasajt, lag=12), 48, ci=0)     # ACF hasta el rezago 48
pacf(diff(pasajt, lag=12), 48, ci=0)    # PACF hasta el rezago 48

# ACF y PACF muestrales de (1-B)(1-B^12)
d1pasajt=diff(pasajt)
par(mfrow=c(2,1)) 
acf(diff(d1pasajt, lag=12), 48)     # ACF hasta el rezago 48
pacf(diff(d1pasajt, lag=12), 48, ci=0)    # PACF hasta el rezago 48
# Conclusión: Hay evidencia de un arima(0,1,1)X(0,1,1) con S=12

# identificación usando criterios de información
# entre los órdenes máximos de los polinomios
p=1
q=1
P=1
Q=1

# entre los órdenes de diferenciación
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

# tabla con órdenes de los modelos ajustados y los criterios de información
# ---------------------------------------------------------------------------
(Crit_Inf=cbind(p_, q_, P_, Q_, AIC, SIC))

# ordenamiento de los criterios de información e menor a mayor
Crit_Inf=data.frame(Crit_Inf)
(aic=Crit_Inf[order(Crit_Inf$AIC),])
(sic=Crit_Inf[order(Crit_Inf$SIC),])
# Conclusión: De nuevo hay evidencia de un arima(0,1,1)X(0,1,1) con S=12

# especificación y estimación del modelo
(mod1=arima(pasajt, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12),
      method = c("CSS-ML")))

# diagnósticos para el modelo estimado
# raíces de los polinomios
library(fArma)
coeficma=c(coef(mod1)[1])   # vector con el coeficiente ma de la parte ordinaria
armaRoots(coeficma)
coeficsma=c(0,0,0,0,0,0,0,0,0,0,0,coef(mod1)[2]) # vector con el coeficiente ma de la parte estacional
armaRoots(coeficsma)

# algunos otros diagnósticos
# chequeo si los residuales son ruido blanco y posibles observaciones extremas
tsdiag(mod1)

# Otra forma de verificar si los residuales son ruido blanco
resmod1=residuals(mod1)
par(mfrow=c(2,1)) 
acf(resmod1)
pacf(resmod1)
# prueba de Ljung-Box
Box.test(resmod1, lag = 36, type = "Ljung-Box")  # hay que corregir los grados
                                              # de libertad como 24-# de parámetros
                                              # Arma del modelo estimado
# cálculo del valor crítico para la prueba de Ljung-Box
qchisq(.95, df=34)

# chequeo preliminar de observaciones atípicas
# estandarización de los residuales
res_est=resmod1/(mod1$sigma2^.5)
plot.ts(res_est)

# chequeo de normalidad
# gráfico cuantil-cuantil
qqnorm(res_est,  xlab = "Cuantiles Teóreticos", ylab = "Cuantiles Muestrales")
qqline(res_est)
 
# histograma, densidad kernel y gráfico normal
plot(density(res_est))
mu<-mean(res_est)
sigm<-sd(res_est)
x<-seq(-4,4,length=100)
y<-dnorm(x,mu,sigm)
hist(res_est,prob=T,ylim=c(0,.7),xlim=c(-5,5),col="yellow")
lines(density(res_est))
lines(x,y,lwd=2,col="blue")
# conclusión: No Se detecta alejamiento severo de la normalidad

# prueba de normalidad
shapiro.test(res_est)                #prueba de Shapiro-Wilks
library(fArma)
jarqueberaTest(res_est)              # prueba de Jarque-Bera, librería fBasics
normalTest(res_est, method=("jb"))
qchisq(.95, df=2)
# conclusión: Se puede no rechazar normalidad a un nivel de significancia de 0.05.

# valores ajustados
ajust=pasajt-resmod1

# gráfico para los valores ajustados
ts.plot(pasajt, ajust)
lines(pasajt, col="black")
lines(ajust, col="red")
# otro gráfico
plot(as.vector(pasajt), as.vector(ajust), type="p")
points(pasajt, ajust,cex=.8)
(length(res_est))

# detección de observaciones atípicas
ind=(abs(res_est)>2.5)
obs=c(1:144)
(grupo=cbind(res_est, obs, ind))
# las observaciones 29, 62 y 135 parecen ser atípicas

# generación de las variables indicadoras
ind29=rep(0, times=144)
ind29[29]=1

ind62=rep(0, times=144)
ind62[62]=1

ind135=rep(0, times=144)
ind135[135]=1

atip=as.matrix(cbind(ind29, ind62, ind135))

# estimacion del modelo con observaciones atípicas
# especificación y estimación del modelo
(mod1c=arima(pasajt, xreg=atip, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12),
      method = c("CSS-ML")))

# diagnósticos
# chequeo si los residuales son ruido blano y posibles observaciones extremas
tsdiag(mod1c)

# chequeo de normalidad
# gráfico cuantil-cuantil
# estandarización de los residuales
resmod1c=residuals(mod1c)
res_est1c=resmod1c/(mod1c$sigma2^.5)
qqnorm(res_est1c,  xlab = "Cuantiles Teóricos", ylab = "Cuantiles Muestrales")
qqline(res_est1c)
 
# histograma, densidad kernel y gráfico normal
plot(density(res_est1c))
mu<-mean(res_est1c)
sigm<-sd(res_est1c)
x<-seq(-4,4,length=100)
y<-dnorm(x,mu,sigm)
hist(res_est1c,prob=T,ylim=c(0,.7),xlim=c(-5,5),col="yellow")
lines(density(res_est1c))
lines(x,y,lwd=2,col="blue")
# conclusión: No se detecta alejamiento severo de la normalidad

# prueba de normalidad
shapiro.test(res_est1c)                #prueba de Shapiro-Wilk
library(fArma)
jarqueberaTest(res_est1c)              # prueba de Jarque-Bera, librería fBasics
normalTest(res_est1c, method=("jb"))
# conclusión:hay normalidad

# pronósticos para la serie transformada
(pasajt.pred=predict(mod1, n.ahead = 24, se.fit = TRUE))
# grafico
plot(pasajt,xlim=c(1949,1962), ylim=c(4,7))
pasajt.pred<-predict(mod1,n.ahead=24)
lines(pasajt.pred$pred,col="blue")
lines(pasajt.pred$pred+2*pasajt.pred$se,col="red",lty=3)
lines(pasajt.pred$pred-2*pasajt.pred$se,col="red",lty=3)

# intervalos de predicción para la serie transformada
pronost=pasajt.pred$pred
pronost
stdev=pasajt.pred$se
stdev
liminft=pronost-2*stdev
limsupt=pronost+2*stdev

# pronósticos e intervalos de predicción para la serie original
# con corrección por sesgos de retransformación
pronos_orig=exp(pronost+.5*stdev^2)
pronos_orig
liminf=exp(liminft+.5*stdev^2)
limsup=exp(limsupt+.5*stdev^2)
ts.plot(pasaj, pronos_orig, liminf, limsup) 
lines(pasaj,col="black")
lines(pronos_orig,col="blue")
lines(liminf,col="red",lty=3)
lines(limsup,col="red",lty=3)

# listado de los pronósticos e intervalos de predicción
pronosticos=cbind(pronos_orig, liminf, limsup)
pronosticos

# Capacidad de pronóstico
# -------------------------------------------------------------------------
# definición de la serie para estimar el modelo
(pasajt_=subset(pasajt, obs<133))
atip2=as.matrix(cbind(ind29, ind62))
(atip2_=subset(atip2, obs<133))

(mod2_=arima(pasajt_, xreg=atip2_, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12),
      method = c("CSS-ML")))

# pronósticos e intervalos de predicción para la serie transformada
# construcción de los valores de las atípicas para pronosticar
atip_futur=matrix(rep(0, times=24), nrow = 12, ncol=2)
(pasajt_.pred<-predict(mod2_, newxreg=atip_futur, n.ahead=12))

(pronost_=pasajt_.pred$pred)
(stdev_=pasajt_.pred$se)

liminft_=pronost_-2*stdev_
limsupt_=pronost_+2*stdev_

# pronósticos e intervalos de predicción para la serie original
# con corrección por sesgos de retransformación
(pronos_orig_=exp(pronost_+.5*stdev_^2))
(liminf_=exp(liminft_+.5*stdev_^2))
(limsup_=exp(limsupt_+.5*stdev_^2))

pronos_orig_=ts(pronos_orig_, start=1960, frequency=12)
liminf_=ts(liminf_, start=1960, frequency=12)
limsup_=ts(limsup_, start=1960, frequency=12)

# gráfica de toda la serie de tiempo con los pronósticos e intervalos
# de predicción
ts.plot(pasaj, pronos_orig_,liminf_, limsup_)
lines(pasaj,col="black")
lines(pronos_orig_,col="blue")
lines(liminf_,col="red",lty=3)
lines(limsup_,col="red",lty=3)

# gráfica de los valores reales que se pronosticaron, los pronósticos 
# e intervalos de predicción
(pasaj_f=subset(pasaj, obs>132))
ts.plot(pasaj_f, pronos_orig_,liminf_, limsup_)
lines(pasaj_f,col="black")
lines(pronos_orig_,col="blue")
lines(liminf_,col="red",lty=3)
lines(limsup_,col="red",lty=3)

# listado de resultados
(result=cbind(pasaj_f, pronos_orig_, error=pasaj_f-pronos_orig_, liminf_, limsup_))
plot(pasaj_f, pronos_orig_)     # grafico de dispersión de reales vs pronósticos 
abline(0,1, col="blue")

# descomposición del error cuadrático medio
(ecm=sum((err=pasaj_f-pronos_orig_)^2)/length(pasaj_f)) # cálculo del error cuadrático medio
(recm=ecm^.5)

# proporción de sesgo
(prop_sesgo=(mean(pronos_orig_)-mean(pasaj_f))^2/ecm) 

# cálculo de varianza sesgadas
sigmap=((length(pasaj_f)-1)/length(pasaj_f)*var(pronos_orig_))^.5 
sigmar=((length(pasaj_f)-1)/length(pasaj_f)*var(pasaj_f))^.5

# proporción de varianza
(prop_var=(sigmap-sigmar)^2/ecm)         

# proporción de covarianza
(prop_covar=2*(1-cor(pronos_orig_, pasaj_f))*sigmap*sigmar/ecm)



