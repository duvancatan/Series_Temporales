library(fracdiff)
# Lectura de datos
(serie=read.table("arfima_noestac_norev_n.txt", header=TRUE))
attach(serie)
# grafica de la serie y ACF
par(mfrow=c(2,1))
plot.ts(Z)
acf(Z, lag.max=100, ci=0)

# Identificacion A.
# Cuando la serie es claramente no estacionaria
# 1. Diferencie una vez la serie, i.e. use el operador (1-B)
dZ=diff(Z)
par(mfrow=c(2,1))
plot.ts(diff(Z))
acf(diff(Z), lag.max=100, ci=0)

# 2. Obtenga una estimacion de d usando GPH y pruebe la hipotesis
#    de memoria larga
(d_GPH=fdGPH(dZ))
(t_GPH=d_GPH$d/d_GPH$sd.as)
(perc_normal_05=qnorm(0.95))
#Conclusion: se rechaza Ho, i.e. hay memoria larga
# 3. Diferencie fraccionalemnete la serie grafique la serie diferenciada 
# obtenga la ACF y PACF
df_dZ=diffseries(dZ,d_GPH$d)
par(mfrow=c(3,1))
plot.ts(df_dZ)
acf(df_dZ, lag.max=20, ci=0)
pacf(df_dZ, lag.max=20)
#Conclusi?n: El modelo parece ser ARFIMA(1,d,0) con d>1, es decir no estacionario
#y sin reversi?n a la media.

# Identificacion B.

# Tiene las mismas etapas que la identificacion A, pero en lugar de usar el 
# estimador GPH se estima un modelo ARFIMA(p*,d,0) para obtener un 
# estimador preliminar de d.

mod_arfi=fracdiff(dZ, nar=5) 
summary(mod_arfi) # Se observa que el esimador de d es positivo y 
                  # estad?sticamente significativo 
(d_CGG=coef(mod_arfi)[1])

df_dZ_CGG=diffseries(dZ,d_CGG)
par(mfrow=c(3,1))
plot.ts(df_dZ_CGG)
acf(df_dZ_CGG, lag.max=20, ci=0)
pacf(df_dZ_CGG, lag.max=20, ci=0)
# Conclusion: Con mas evidencia se ve que el modelo parece ser ARFIMA(1,d,0) con d>1,
# es decir no estacionario y sin reversi?n a la media.




memory.long <- fracdiff.sim(80, d = 0.3)
mGPH <- fdGPH(memory.long$series)
r <- diffseries(memory.long$series, d = mGPH$d)
#acf(r) # shouldn't show structure - ideally






library(help=fracdiff)
fdGPH                   Geweke and Porter-Hudak Estimator for ARFIMA(p,d,q)
fdSperio 
