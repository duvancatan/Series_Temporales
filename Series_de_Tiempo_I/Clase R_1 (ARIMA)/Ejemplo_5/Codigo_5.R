###############
## EJEMPLO 5 ##
###############
dir()
s5<-ts(read.table("nasdaq.txt", header = FALSE, dec="."))
x<-s5[1500:2600]
length(x)
#############
## GRAFICA ##
#############
plot.ts(x, main="Nasdaq", ylab="");grid()
par(mfrow=c(1,2))
acf(x, lag.max = 100)
pacf(x, lag.max = 100)


#############################################
## SI ES NECESARIO ESTABILIZAR LA VARIANZA ##
#############################################
library(MASS)
boxcox(x ~ 1, lambda = seq(-2, 5, 1/10), plotit = TRUE, xlab = expression(lambda),
       ylab = "log-Likelihood")
library(car)
(tbc=powerTransform(x))

plot.ts((x^(2.57)-1)/2.57)


########################
## PRIMERA DIFERENCIA ##
########################
dx<-diff(x)
plot.ts(dx, ylab="", main="Serie x Diferenciada");grid()
par(mfrow=c(1,2))
acf(dx, lag.max=40, main="")
pacf(dx, lag.max=40, main="")


#######################################################
## FUNCION QUE BUSCA EL MEJOR MODELO SEGUN AIC Y BIC ##
#######################################################
sarima.list = function(datos, p, d = 0, q, P = 0, D = 0, Q = 0, S = NA, include.mean = F, criterio = ""){
  M <- matrix(ncol = 10,nrow = (p+1)*(q+1)*(P+1)*(Q+1),dimnames=list(NULL,c("p","d","q","P","D","Q","S","converge","AIC", "BIC")))
  k <- 1 
  n <- length(datos)
  for(i in 0:p){
    for(j in 0:q){
      for(l in 0:P){
        for(m in 0:Q){
          if ((i==0)&&(j==0)&&(l==0)&&(m==0)) next #Continua con la siguiente iteracion 
          fit <- arima(datos, order = c(i, d, j),seasonal = list(order = c(l, D, m), period = S), include.mean = include.mean)
          M[k,1]  <- i
          M[k,2]  <- d
          M[k,3]  <- j
          M[k,4]  <- l
          M[k,5]  <- D
          M[k,6]  <- m
          M[k,7]  <- S
          M[k,8]  <- fit$code  # 0: Convergencia, 1: No Convergencia
          M[k,9]  <- AIC(fit)  # AIC
          M[k,10] <- AIC(fit, k = log(length(datos)))  # BIC
          k <- k+1 
        } 
      } 
    }
  }
  if(criterio == "AIC"){
    M <- M[order(M[,9]),]
  }
  if(criterio == "BIC"){
    M <- M[order(M[,10]),]
  }
  if(criterio == ""){
    M <- M
  }
  rownames(M) = rep("", (p+1)*(q+1)*(P+1)*(Q+1))
  return(M[1:((p+1)*(q+1)*(P+1)*(Q+1)-1),]) 
}

sarima.list(dx, p=5, d = 0, q=5, P = 0, D = 0, Q = 0, S = NA, include.mean = F, criterio = "")

modex<-arima(dx)
AIC(modex) # AIC
AIC(modex, k = log(length(modex)))  # BIC



#######################################
## ESTIMACION DE PARAMETROS IMA(1,1) ##
#######################################
# Estimacion ML condicional# 
(mod_1=arima(s5, c(0, 1, 1), method = c("CSS"))) 
res_1=residuals(mod_1)
# Estimacion ML exacta#
(mod_2=arima(s5, c(0, 1, 1), method = c("ML"))) 
res_2=residuals(mod_2)
# Estimacion ML exacta con valores iniciales dados por la estimacion condicional#
(mod_3=arima(s5, c(0, 1, 1), method = c("CSS-ML"))) 
res_3=residuals(mod_3)


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

library(nortest)
lillie.test(res_1)
library(stats)
ks.test(res_1,"pnorm",mean(res_1), sd(res_1))

library(forecast)
r<-auto.arima(s5)$residuals
hist(r)
shapiro.test(r)
Box.test(r, type = c("Box-Pierce", "Ljung-Box"))


################
## PRONOSTICO ##
################

# Pronosticos para la serie transformada
mod<-arima(s5, order = c(0, 1, 1),method = c("CSS-ML"))
forecast(mod)
plot(forecast(mod), ylab="H02 sales (million scripts)", xlab="Year")

(s5.pred<-predict(mod, n.ahead = 10, se.fit = T))

# grafico
plot(s5,xlim=c(0,180), ylim=c(9,16),ylab="", main="Prediccion Serie 5");grid()
s5.pred<-predict(mod,n.ahead=30)
lines(s5.pred$pred,col="blue", lty=3, lwd=2)
lines(s5.pred$pred+2*s5.pred$se,col="red",lty=3, lwd=2)
lines(s5.pred$pred-2*s5.pred$se,col="red",lty=3, lwd=2)

(predict(mod, n.ahead = 10, se.fit = F))
(predict(mod, n.ahead = 10, se.fit = F))

cbind(s5.pred$pred+2*s5.pred$se, s5.pred$pred-2*s5.pred$se)