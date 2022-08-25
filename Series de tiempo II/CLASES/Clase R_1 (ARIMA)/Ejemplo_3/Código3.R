dir()
library(matrixcalc)
z<-vec(t(as.matrix(read.table("Sere3.txt", dec=".", sep=""))))

#############
## GRAFICA ##
#############
plot.ts(z, main="Serie", ylab="");grid()
par(mfrow=c(1,2))
acf(z, lag.max=20, main="ACF")
pacf(z,lag.max=20, main="PACF")
library(TSA)
eacf(z, ar.max = 7, ma.max = 15)


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
sarima.list(z, p=3, d = 0, q=3, P = 0, D = 0, Q = 0, S = NA, include.mean = F, criterio = "")



#######################################
## ESTIMACION DE PARAMETROS ARMA(2,2) ##
#######################################

# Estimacion ML exacta con valores iniciales dados por la estimacion condicional#
(mod_1=arima(z, c(2, 0, 2), method = c("CSS-ML"))) 
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

library(nortest)
lillie.test(res_1)
library(stats)
ks.test(res_1,"pnorm",mean(res_1), sd(res_1))



################
## PRONOSTICO ##
################

# Pronosticos para la serie transformada
mod<-arima(z, order = c(2, 0, 2), method = c("CSS-ML"))
library(forecast)
forecast(mod)
plot(forecast(mod), ylab="H02 sales (million scripts)", xlab="Year")

(z.pred<-predict(mod, n.ahead = 50, se.fit = T))

# grafico
plot.ts(z, xlim=c(0,700), main="Prediccion Serie 5");grid()
z.pred<-predict(mod,n.ahead=100)
lines(z.pred$pred,col="blue", lty=3, lwd=2)
lines(z.pred$pred+2*z.pred$se,col="red",lty=3, lwd=2)
lines(z.pred$pred-2*z.pred$se,col="red",lty=3, lwd=2)

(predict(mod, n.ahead = 10, se.fit = F))
(predict(mod, n.ahead = 10, se.fit = F))

cbind(z.pred$pred+2*z.pred$se, z.pred$pred-2*z.pred$se)
