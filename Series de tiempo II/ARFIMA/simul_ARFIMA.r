# simulación de un modelo ARFIMA

library(fracdiff)

# proceso de ruido blanco fraccional
# -----------------------------------------------------------------------------
d(z1)<-fracdiff.sim(500, m=3, d=0.4)
par(mfrow=c(2,1))
plot(as.ts(z1$series)) 
acf(z1$series, lag.max=50, ci=0)

# proceso de ruido AR fraccional
# -----------------------------------------------------------------------------
z2<-fracdiff.sim(500, ar=0.8, mu=2, d=0.35)
plot(as.ts(z2$series)) 
par(mfrow=c(2,1))
plot(as.ts(z2$series)) 
acf(z2$series, lag.max=50, ci=0)

# proceso de ruido MA fraccional
# -----------------------------------------------------------------------------
z3<-fracdiff.sim(500, ma=-0.5, mu=2, d=0.35)
plot(as.ts(z3$series)) 
par(mfrow=c(2,1))
plot(as.ts(z3$series)) 
acf(z3$series, lag.max=50, ci=0)

# proceso de ruido ARMA fraccional
# -----------------------------------------------------------------------------
z4<-fracdiff.sim(500, ar=0.8, ma=-0.5, mu=2, d=0.35)
plot(as.ts(z4$series)) 
par(mfrow=c(2,1))
plot(as.ts(z4$series)) 
acf(z4$series, lag.max=50, ci=0)





