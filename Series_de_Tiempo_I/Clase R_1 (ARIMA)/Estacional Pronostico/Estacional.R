library(fArma)
library(astsa)

######################################
#Simulacion SARIMA(0,0,0)x(1,0,0)S=12#
######################################
AR12<-arima.sim(n=400, list(ar = c(rep(0,11),0.9)),sd = sqrt(0.3))
plot.ts(AR12)
acf(AR12, ci=0, lag.max = 100)
pacf(AR12, lag.max = 100)

#ACF y PACF Verdaderas#
model_AR12 = list(ar = c(rep(0,11),0.9))
par(mfrow=c(1,2))
armaTrueacf(model_AR12, lag.max=100, type = c("both"))



######################################
#Simulacion SARIMA(0,0,0)x(0,0,1)S=12#
######################################

MA12<-arima.sim(n=400, list(ma = c(rep(0,11),0.8)),sd = sqrt(0.3))
plot.ts(MA12)
acf(MA12,  lag.max = 100)
pacf(MA12,ci=0, lag.max = 100)

#ACF y PACF Verdaderas#
model_MA12 = list(ma = c(rep(0,11),0.8))
par(mfrow=c(1,2))
armaTrueacf(model_MA12, lag.max=100, type = c("both"))


######################################
#Simulacion SARIMA(1,0,0)x(1,0,0)S=12#
######################################

AR112=arima.sim(n=400, list(ar=c(0.4,rep(0,11),0.7, -0.28)),sd = sqrt(0.1796))
plot.ts(AR112)
acf(AR112, lag.max = 100)
pacf(AR112, lag.max = 100)

#Verdaderas#
par(mfrow=c(1,2))
model_AR112 = list(ar=c(0.4,rep(0,11),0.7, -0.28))
armaTrueacf(model_AR112, lag.max=100, type = c("both"))



######################################
#Simulacion SARIMA(1,0,0)x(1,0,0)S=12#
######################################

ARMA1=arima.sim(n=400, list(ar=c(rep(0,11),0.7), ma=c(0.9)),sd = sqrt(0.3))
plot.ts(ARMA1)
ARMA1=arima.sim(n=400, list(ar=c(rep(0,11),0.7), ma=0.9),sd = sqrt(0.3))
plot.ts(ARMA1)
acf(ARMA1, lag.max = 100)
pacf(ARMA1, lag.max = 100)

#Verdaderas#
par(mfrow=c(1,2))
model_ARMA1 = list(ar=c(rep(0,11),0.7), ma=0.9)
armaTrueacf(model_ARMA1, lag.max=100, type = c("both"))



#####################
#### PRONOSTICOS ####
#####################

#EJEMPLO 1#

dir()
s5<-ts(read.table("serie14-5.txt", header = FALSE, dec="."))

#############
## GRAFICA ##
#############
plot(s5, main="Serie", ylab="");grid()
par(mfrow=c(1,2))
acf(s5)
pacf(s5)



########################
## PRIMERA DIFERENCIA ##
########################
ds5<-diff(s5)
plot(ds5, ylab="", main="Serie 5 Diferenciada");grid()
par(mfrow=c(1,2))
acf(ds5, lag.max=25, main="")
pacf(ds5, lag.max=25, main="")



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

sarima.list(s5, p=2, d = 1, q=2, P = 0, D = 0, Q = 0, S = NA, include.mean = F, criterio = "")





#######################################
## ESTIMACION DE PARAMETROS IMA(1,1) ##
#######################################
# USANDO FUNCION >arima() #
# Estimacion ML exacta con valores iniciales dados por la estimacion condicional#
(mod=arima(s5, c(0, 1, 1), method = c("CSS-ML"))) 
res=residuals(mod)

# USANDO FUNCION >sarima() #

modsar<-sarima(s5, p=0, d=1, q=1)
names(modsar)
modsar$fit
ress<-modsar$fit$residuals




################################################## 
## PRUEBAS DIAGNOSTICO - ANALISIS DE RESIDUALES ##
##################################################
par(mfrow=c(3,2))
plot.ts(ress, main="Residuales", ylab="")
hist(ress, main="Histograma de Residuales")
acf(ress, lag.max=25, main="ACF de Residuales")
pacf(ress, lag.max=25, main="PACF de Residuales")

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
Box.Test(ress)

shapiro.test(ress)
qqnorm(ress, main="QQ de los Residuales")
qqline(ress, col="red")

library(nortest)
lillie.test(ress)

library(stats)
ks.test(ress,"pnorm",mean(ress), sd(ress))

library(fBasics)
jarqueberaTest(ress)


#Pronostico del ARIMA(0,1,1)#
?sarima.for()
prono=sarima.for(s5, 10, 0,1,1)
prono$pred
prono$se





###############
## EJEMPLO 2 ##
###############

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

# USANDO FUNCION >sarima() #
modsarma<-sarima(s5, p=2, d=0, q=2)
names(modsarma)
modsarma
modsarma$fit
ress<-modsarma$fit$residuals


################################################## 
## PRUEBAS DIAGNOSTICO - ANALISIS DE RESIDUALES ##
##################################################
par(mfrow=c(3,2))
plot.ts(ress, main="Residuales", ylab="")
hist(ress, main="Histograma de Residuales")
acf(ress, lag.max=25, main="ACF de Residuales")
pacf(ress, lag.max=25, main="PACF de Residuales")

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
Box.Test(ress)

shapiro.test(ress)
qqnorm(ress, main="QQ de los Residuales")
qqline(ress, col="blue")

library(nortest)
lillie.test(ress)

library(stats)
ks.test(ress,"pnorm",mean(ress), sd(ress))



################
## PRONOSTICO ##
################

#Pronostico del ARIMA(0,1,1)#
?sarima.for()
prono=sarima.for(z, 10, 1,0,1)
prono$pred
prono$se




