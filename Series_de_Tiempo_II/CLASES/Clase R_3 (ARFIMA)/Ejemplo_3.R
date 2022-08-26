######################
###### EJEMPLO #######

#Datos
petro<-scan(dec = ",")
length(petro)

#Grafica
plot.ts(petro)

#Estabilizar Varianza
lpetro<-log(petro)
plot.ts(lpetro)
ac<-acf(lpetro, lag.max = 700)
plot(ac[1:700], ylim=c(-0.1,1), main="log(petro)") # quitando lag 0
spec.pgram(lpetro, log="no")

#Retornos (Direncia 1)
rpetro<-diff(lpetro)
plot.ts(rpetro)
acf(rpetro)
pacf(rpetro)


#Diferencia Fraccional
petro.fd = fracdiff(lpetro, nar=0, nma=0, M=30)
summary(petro.fd)
dfl<-diffseries(lpetro, petro.fd$d)
acf(dfl)
pacf(dfl)

