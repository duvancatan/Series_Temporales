########################
###### SIMULACION ######
########################

x<-1:1024
a1<-1.5*sin(pi*x/6) #T1=2pi/(pi/6)=12
a2<-3*sin(pi*x/12)  #T2=2pi/(pi/12)=24
a3<-5*sin(pi*x/24)  #T3=2pi/(pi/24)=48
par(mfrow=c(2,2))
plot.ts(a1, ylim=c(-5,5))
plot.ts(a2, ylim=c(-5,5),col="blue")
plot.ts(a3, ylim=c(-5,5),col="red")
plot.ts(a1, ylim=c(-5,5))
lines(a2, col="blue")
lines(a3, col="red")

#########################################################
#### Analisis de Fourier para serie "suma" SIN RUIDO ####
#########################################################

suma<-a1+a2+a3
par(mfrow=c(2,1))
plot.ts(suma)
library(astsa)
acf(suma,lag.max=100, ci=0 )

per<-spec.pgram(suma, log="no") # Grafica la densidad espectral estimada
names(per)
M1<-matrix(c(per$spec,per$freq),ncol=2) # Frecuencias X Espectro
 


#############################
## BUSQUEDA DE FRECUENCIAS ##
#############################

f1<-which.max(per$spec) #Posicion de la frecuencia
wmax1<-per$freq[f1] # En cual freq aalcanza el maximo
(T1<-1/wmax1) #Para calcular el periodo principal

f2<-which.max(per$spec[-c(f1)])
wmax2<-per$freq[f2] # Em qual freq atinge o maximo
(T2<-1/wmax2) #Para calcular o periodo principal

#Busqueda localizada
xx1<-per$freq[70:100]
yy1<-per$spec[70:100]
plot(xx1,yy1, type="l")

f3<-which.max(yy1)
wmax3<-xx1[f3] # En cual freq alcanza el maximo
(T3<-1/wmax3) #Para calcular el periodo principal



############################################################
#### Analisis de Fourier para la serie "suma" CON RUIDO ####
############################################################

suma2<-a1+a2+a3+rnorm(1024, sd=5)
par(mfrow=c(2,1))
plot.ts(suma2)
acf(suma2,lag.max=100, ci=0 )
per<-spec.pgram(suma2, log="no", kernel("modified.daniell", c(1,2))) # Densidade Espectral Estimada Suavizada
# En kernel("modified.daniell", c(1,2)), n,m=1,2,3,... . Entre mas grandes, mayor sera la suavidad del espectro.  
# per<-spec.pgram(suma2, log="no") # Densidad Espectral Estimada sin suavizar. 
M2<-matrix(c(per$spec,per$freq),ncol=2) # Frecuencias X Espectro

f1<-which.max(per$spec) #Posicion de la frecuencia
wmax1<-per$freq[f1] # En cual freq alcanza el maximo
(T1<-1/wmax1) #Para calcular el periodo principal


zz1<-per$freq[25:100]
ww1<-per$spec[25:100]
plot(zz1,ww1, type="l")
f2<-which.max(ww1)
wmax2<-zz1[f2] # En cual freq alcanza el maximo
(T2<-1/wmax2) #Para calcular el periodo secundario

#Busqueda localizada
xx1<-per$freq[70:100]
yy1<-per$spec[70:100]
plot(xx1,yy1, type="l")
f3<-which.max(yy1)
wmax3<-xx1[f3] # En cual freq alcanza el maximo
(T3<-1/wmax3) #Para calcular el tercer periodo principal

#######################################################
#######################################################


#Grafico de Combinacion
par(mfrow=c(3,1))
plot.ts(a1+a2+a3)
plot.ts(a1+a3)
B=rep(0,1024)
B[1:512]<-(a1+a2+a3)[1:512]
B[513:1024]<-(a1+a3)[1:512]
plot.ts(B)

############################################
#### Analisis de Fourier para serie "B" ####
############################################

plot.ts(B)
per1<-spec.pgram(B, log="no") # Grafico da densidad espectral estimada
par(mfrow=c(3,1)) # No podemos identificar cambio en las frecuancias
plot.ts(B)
per<-spec.pgram(suma, log="no") 
per1<-spec.pgram(B, log="no") 










