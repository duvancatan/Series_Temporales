library(fracdiff)

# Datos
petrobras<-read.table("datos_0.txt", sep = "", dec=".", header = T)
head(petrobras)
attach(petrobras)
z<-petrobras$petro
length(z)
par(mfrow = c(3, 1)) 
plot(as.ts(z), main="Retornos Petrobras");grid()
acf(z, lag.max=500)
spec.pgram(z, log="no")


# Procedimiento de Identificacion
# Paso 1
modelo1<-fracdiff(z, nar = 8, nma = 0, dtol = NULL, drange = c(0, 0.5), M = 100)
summary(modelo1)
(d_est=coef(modelo1)[1])
#resid=residuals(modelo1)


# Paso 2: Del paso 1 se obtuvo un d estimado almacenado en d_est
df_z <- diffseries(z, d_est) #Diferencia fraccionaria

# Paso 3. Chequear la acf y pacf del ruido fraccional
par(mfrow = c(2, 1))
acf(df_z, ci=0, lag.max =100)
pacf(df_z, lag.max =100)
# Los resultados son consistentes con un AR(1).


# Paso 4 # Estimaci??n del modelo identificado como ARFIMA(1,d,0)
modelo2 <- fracdiff(z, nar = 1, nma = 0,dtol = NULL, drange = c(0, 0.5), M = 100) 
summary(modelo2) 

# Paso 5 Analisis de Reiduales
res.df=residuals(modelo2)
acf(res.df)
pacf(res.df)
qqnorm(res.df, ylim=c(-3,3))
qqline(res.df, col="red")
library(MASS)
truehist(res.df)
shapiro.test(res.df)


#Paso 6. Pronostico
library(forecast)
pronostico=forecast(modelo2, h=20)
plot(pronostico)

############################################################################
  