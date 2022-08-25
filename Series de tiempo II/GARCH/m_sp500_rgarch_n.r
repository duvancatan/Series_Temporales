library(rgarch)
# library(help="rgarch")
library(fBasics)

# lectura de los datos
(sp500 = ts(scan("g:/Unal/series de tiempo II/m_sp500.txt")))
sp500=as.vector(sp500)
# gr�fico de la serie
plot.ts(sp500, main="Rendimientos de SP500")
par(mfrow=c(2,1))
acf(sp500, lag.max=20)
pacf(sp500, lag.max=20)

par(mfrow=c(2,1))
acf(sp500^2, lag.max=20)
pacf(sp500^2, lag.max=20, ci=0)

# estimaci�n de un modelo GARCH(1,1)
spec1=ugarchspec(variance.model=list(model="sGARCH", 
garchOrder=c(1,1)),mean.model=list(armaOrder=c(3,0),
include.mean=TRUE),distribution.model="std")

fit1=ugarchfit(sp500, spec1)
fit1

# redefinici�n del modelo
spec1=ugarchspec(variance.model=list(model="sGARCH", 
garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0),
include.mean=TRUE),distribution.model="sged")

fit1=ugarchfit(sp500, spec1)
fit1
plot(fit1)                      # gr�ficos producidos en la estimaci�n: pide selecci�n
1plot(fit1, which=9)             # gr�ficos producidos en la estimaci�n: selecciona el 1
plot(fit1, which="all")         # gr�ficos producidos en la estimaci�n: selecciona todos

coefic=coef(object=fit1)         # obtenci�n de los coeficintes del modelo
fit1_series=as.data.frame(fit1)  # extrae los datos, valores ajustados, residuales y SD condicional
plot.ts(fit1_series[,4])         # gr�fico de la SD condicional
plot.ts(fit1_series[,3])         # grafico de residuales
res_est=as.ts(fit1_series[,3]/fit1_series[,4])  # obtenci�n residuales estandarizados
plot.ts(res_est)                 # grafico de residuales estandarizados

par(mfrow=c(2,1))
acf(res_est)
acf(res_est^2)

pron=ugarchforecast(fit1, n.ahead = 8)  # pron�sticos 
plot(pron,which="all")                  # gr�fico de los pron�ticos

# Prueba de asimetr�a: efecto leverage
# correlaci�n cruzada entre residuales est. cuadrados  y residuales est.
ccf(res_est^2, res_est)

# prueba general con un rezago
prueba_gener=lm(res_est^2~lag(res_est, -1))
summary(prueba_gener)           # parece que existen efectos leverage

# Prueba de Engel y NG simple
dt=(res_est<0)
dt=dt*1
prueba_Eng_NG=lm(res_est^2~lag(dt, -1))
summary(prueba_Eng_NG)           # parece que existen efectos leverage

# Prueba de Engel y NG ampliada
dt_res=dt*res_est
cdt_res=(1-dt)*res_est

(m=cbind(res_est^2, lag(dt, -1), lag(dt_res, -1), lag(cdt_res, -1), lag(res_est, -1)))
prueba_Eng_NG_amp=lm(res_est^2~lag(dt, -1)+lag(dt_res, -1)+lag(cdt_res, -1))
summary(prueba_Eng_NG_amp)        # Parece que existen efectos leverage

(innov_noneg=c(1:5))
(innov_neg=c(-1:-5))

efecto_neg=coef(prueba_Eng_NG_amp)[2]+coef(prueba_Eng_NG_amp)[3]*innov_neg
efecto_noneg=coef(prueba_Eng_NG_amp)[4]*innov_noneg

efectos=c(efecto_neg, efecto_noneg)
innov=c(innov_neg, innov_noneg) 
plot(innov, efectos)
(cbind(innov_neg, efecto_neg, innov_noneg, efecto_noneg))

# estimaci�n de un modelo IGARCH(1,1)
spec2=ugarchspec(variance.model=list(model="iGARCH", 
garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0),
include.mean=TRUE),distribution.model="ged")

fit2=ugarchfit(sp500, spec2)
fit2
plot(fit2)
plot(fit2, which="all")

coefic2=coef(fit2)               # obtenci�n de los coeficientes del modelo
fit2_series=as.data.frame(fit2)  # extrae los datos, valores ajustados, residuales y SD condicional
plot.ts(fit2_series[,4])         # gr�fico de la SD condicional
plot.ts(fit2_series[,3])         # grafico de residuales
res_est2=fit2_series[,3]/fit2_series[,4]  # obtenci�n residuales estandarizados
plot.ts(res_est2)                 # grafico de residuales estandarizados

par(mfrow=c(2,1))
acf(res_est2) 
acf(res_est2^2)

pron2=ugarchforecast(fit2, n.ahead = 20)  # pron�sticos 
plot(pron2,which="all")                   # gr�fico de los pron�ticos

# modelo con premio al riesgo: GARCH-M
spec3=ugarchspec(variance.model=list(model="iGARCH", 
garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0),
include.mean=TRUE, garchInMean = TRUE, inMeanType = 1), 
distribution.model="ged")

fit3=ugarchfit(sp500, spec3)
fit3
plot(fit3)
plot(fit3, which="all")

coefic3=coef(fit3)               # obtenci�n de los coeficientes del modelo
fit3_series=as.data.frame(fit3)  # extrae los datos, valores ajustados, residuales y SD condicional
plot.ts(fit3_series[,4])         # gr�fico de la SD condicional
plot.ts(fit3_series[,3])         # grafico de residuales
res_est3=fit3_series[,3]/fit3_series[,4]  # obtenci�n residuales estandarizados
plot.ts(res_est3)                 # grafico de residuales estandarizados

par(mfrow=c(2,1))
acf(res_est3) 
acf(res_est3^2)

pron3=ugarchforecast(fit3, n.ahead = 20)  # pron�sticos 
plot(pron3,which="all")                   # gr�fico de los pron�ticos




