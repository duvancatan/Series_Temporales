#############################
## Generalidades de la ACF ##
h<-0:500
d=0.4
f<-h^(2*d-1)
plot(h,f, type="h") # Decaimiento Hiperbolico 
#########################

library(fracdiff)

# Simulacion del modelo ARFIMA
w<-fracdiff.sim(600, ma=0.7 , d=0.3) 
plot(as.ts(w$series))
acf(w$series, lag.max=150)

# Verificaci??n te??rica del procedimiento 
df_w<-diffseries(w$series, 0.35) 
par(mfrow = c(3, 1))
plot(df_w, type="l")
acf(df_w)
pacf(df_w, ci=0)

# Procedimiento de Identificaci??n
# Paso 1
modelo1<-fracdiff(w$series, nma = 1, dtol = NULL, drange = c(0, 0.5), M = 100)
summary(modelo1)
(d_est=coef(modelo1)[1])
#resid=residuals(modelo1)

# Paso 2: del paso 1 se obtuvo un d estimado almacenado en d_est
df_w <- diffseries(w$series, d_est)

# paso 3
par(mfrow = c(2, 1))
acf(df_w, ci=0)
pacf(df_w, ci=0)

# los resultados son consistentes con un MA(1).
# paso 4 # Estimaci??n del modelo identificado como ARFIMA(1,d,0)
modelo2 <- fracdiff(w$series, nar = 0, nma = 1,dtol = NULL, drange = c(0, 0.5), M = 100) 
summary(modelo2) 
############################################################################
