#################################################################################
library(fArma)

# 1 #
X1<-farimaSim(n = 2000, model = list(ar = c(0.5, -0.5), d = 0.4, ma = 0.1),method = c("freq", "time"))
plot.ts(X1)
acf(X1, lag.max = 500)
spec.pgram(X1,log="no")

# 2 #
acft<-farimaTrueacf(2000, H=0.9) # H = d + 0.5
plot(acft, type="h")

acft1<-farimaTrueacf(200,H=0.4) # H = d + 0.5
plot(acft1, type="h")

# 3 #
rsFit(X1)
perFit(X1)

#################################################################################
#################################################################################
library(fracdiff)

# 1 #
Z1<-fracdiff.sim(3000, ar = .2, ma = .4, d = .3)
par(mfrow=c(1,2))
acf(Z1$series, lag.max = 700)
spec.pgram(X1,log="no")

Z2<-fracdiff.sim(3000, ar = c(0.7, -0.1), ma =c(-0.5, 0.4), d = .25)
par(mfrow=c(1,2))
acf(Z2$series, lag.max = 700)
spec.pgram(X1,log="no")

# 2 #
df<-fracdiff(Z1$series, nar = 1, nma = 1)

# 3 #
acf(diffseries(Z1$series,df$d))
pacf(diffseries(Z1$series,df$d))

# 4 #
fdGPH(Z1$series,bandw.exp = 0.5)








