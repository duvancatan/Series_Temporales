#ANALISIS OMITIENDO LOS DATOS FALTANTES

#Lectura
dir()
datos<-read.table("julio.txt", dec = ",", header = FALSE, sep = "")
names(datos)
plot.ts(datos)
head(datos)
dim(datos)
attach(datos)
length(V1)

#Graficos
plot.ts(V1)
acf(V1)
length(which(is.na(V1)==TRUE)) #Numero de dados faltantes
X<-na.omit(V1) #Omitiendo datos faltantes
length(X)
plot.ts(X)

#ACF PACF sin datos faltantes
acf(X, lag.max = 5000)
pacf(X, lag.max = 100)

plot.ts(X[1:100])
plot.ts(V1[1:100])
plot.ts(V1[100:200])
plot.ts(V1[200:300])
plot.ts(V1[300:400])
plot.ts(V1[400:500])
plot.ts(V1[500:600])
V1[1:100]
library(stats)
per1<-spec.pgram(X, log="no",kernel("daniell", c(5,7))) # Grafica la densidad espectral estimada
#?spec.pgram # library(stats)
names(per1)
per1$freq # Freqs donde se calculan las Densidades espectrales  
per1$spec # Densidades espectrales estimadas 
wmax1<-per1$freq[which.max(per1$spec)] # En cual freq alcanza el maximo
(s1<-1/wmax1) #Para calcular el periodo principal
(sort(per1$spec))[c(1:100)]
(sort(per1$spec))[c(11000:11250)]

# NOTA1: De aqu?? podemos decir que el primer periodo relevante es P1=22,91242.
#        Las dem??s frequencias en una vecidad de ella son muy parecidas.

#Tiramos os primeiros 1200 elemento del periodograma para localizar el segundo grupo de frequencias altas.
XXX<-per1$freq
YYY<-per1$spec
length(XXX)
plot(XXX[-c(0:1200)],YYY[-c(0:1200)], type = "l")
x1<-XXX[-c(0:1200)]
y1<-YYY[-c(0:1200)]

max2<-x1[which.max(y1)]
(ss1<-1/max2)

#NOTA2: De aqu?? podemos decir que el segundo periodo relevante es P2=11,46789.
#       Las dem??s frequencias en una vecidad de ella son muy parecidas.

#NOTA3: Se puede observar que (2*ss1 \aprox s1) (2*11.46789 \aprox 22.91242), o sea,
#       el periodo principal es dos veces el periodo secundario.


#PRUEBAS DE HIPOTESIS

#Para Frecuencia Principal
n=length(X)/2
g1<-max(per1$spec)/(sum(per1$spec[1:length(X)/2]))
(pv1=n*(1-g1)^n)

#Para Frecuencia Secundario
g2<-max(y1)/(sum(per1$spec[1:length(X)/2])-max(per1$spec))
(pv2=(n-1)*(1-g2)^(n-2))







