#############
## GRAFICA ##
#############
z<-accion
length(z)
ts.plot(z)
plot.ts(z, main="Serie", ylab="");grid()
par(mfrow=c(1,2))
acf(z, lag.max=50, main="ACF")
pacf(z,lag.max=50, main="PACF")




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

sarima.list(z, p=5, d = 0, q=5, P = 0, D = 0, Q = 0, S = NA, include.mean = F, criterio = "")


#######################################
## ESTIMACION DE PARAMETROS AR(5) ##
#######################################

# Estimacion ML exacta#

#Sin Fijar parametros#
mod_1<-arima(z, c(5, 0 ,0), method = c("ML"))
mod_1
res_1=residuals(mod_1)


#Fijando parametros#
mod_2222=arima(z, c(5, 0 ,0), fixed = c(0,0,0,0,NA,0))
mod_2222
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

library(fBasics)
jarqueberaTest(res_1)

library(stats)
ks.test(res_1,"pnorm",mean(res_1), sd(res_1))
