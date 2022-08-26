################################
########## EJEMPLO_3 ###########
################################
 
library(astsa)
soi # Conjunto de Datos
plot(soi, main="SOI", ylab="");grid() # Serie Original
mod<-lm(soi~time(soi))
abline(mod, lwd="2")
summary(mod)
des<-soi-(mod$coefficients[1]+time(soi)*mod$coefficients[2])
plot(des, main="SOI", ylab="");grid() # Serie con tendencia removida 
par(mfrow=c(1,3))
plot.ts(des)
acf(des)
pacf(des)


##################################################
## UTILIZANDO LA TRANSFORMADA FINITA DE FOURIER ##
##################################################

I = abs(fft(des))^2/453 # Periodograma
P = (4/453)*I[1:226] #  Periodograma escalado
f = 0:225/453 # Frecuencias
plot(f, P, type="l", xlab="Frequency", ylab="", main="Periodograma");grid()
n=226
#plot(f, I[1:226], type="l", xlab="Frequency", ylab="", main="Periodograma");grid()
(PXF<-matrix(c(P,f), ncol = 2)) # Periodo X Frecuencias


############################################
## BUSQUEDA DE FRECUENCIAS SIGNIFICATIVAS ##
############################################

(P1=which.max(P)) # Frecuencia Principal
(f1=f[P1])
g1<-(max(P))/(sum(P))
(pv1<-n*(1-g1)^(n-1)) # (p-valor) Periodo significativo
(T1=1/f1) # Ciclo anual (MESES)

(P2=which.max(P[1:25])) #Frecuencia Secundaria
(f2=f[P2])
(g2<-(max(P[1:15])/(sum(P)-max(P))))
(pv2<-(n-1)*(1-g2)^(n-2)) # (p-valor) Periodo significativo
(T2=1/f2) # Ciclo debido al fenomeno del nino (MESES)


###########################################
## GRAFICO DE FRECUENCIAS SIGNIFICATIVAS ##
###########################################

plot(f, P, type="l", xlab="Frequency", ylab="", main="Periodograma");grid()
points(x=f1, y=max(P), pch = 21, bg="white", lwd=3)
points(x=f2, y=max(P[1:15]), pch = 21, bg="black", lwd=3)








