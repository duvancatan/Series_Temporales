library(fracdiff)

# Simulación del modelo ARFIMA
z <- fracdiff.sim(600, ar=0.7, d=.25)
par(mfrow = c(2, 1))
plot(as.ts(z$series))
acf(z$series, lag.max=50, ci=0)

# Verificación teórica del procedimiento
df_z <- diffseries(z$series, 0.25)
par(mfrow = c(3, 1))
plot(df_z, type="l")
acf(df_z, ci=0)
pacf(df_z, ci=0)

# Procedimiento de Identificación

# paso 1
modelo1 <- fracdiff(z$series, nar = 8, nma = 0,dtol = NULL, drange = c(0, 0.5), M = 100)
summary(modelo1)
d_est=coef(modelo1)[1]

# paso 2: del paso 1 se obtuvo un d estimado almacenado en d_est
df_z <- diffseries(z$series, d_est)

# paso 3
par(mfrow = c(2, 1))
acf(df_z, ci=0)
pacf(df_z, ci=0)
# los resultados son consistentes con un AR(1). 

# paso 4 # Estimación del modelo identificado como ARFIMA(1,d,0)
modelo2 <- fracdiff(z$series, nar = 1, nma = 0,dtol = NULL, drange = c(0, 0.5), M = 100)
summary(modelo2)
