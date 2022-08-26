#Leitura
dir()
datos<-read.table("julio.txt", dec = ",", header = FALSE, sep = "")
names(datos)
plot.ts(datos)
attach(datos)
length(V1)

#Graficos
plot.ts(V1)
length(which(is.na(V1)==TRUE)) #Numero de dados faltantes
X<-na.omit(V1) # Omitiendo dados faltantes
length(X)
plot.ts(X) #Grafica sem dados faltantes 


#ACF PACF sem datos faltantes
acf(X, lag.max = 100)
pacf(X, lag.max = 100)

#Grafica de algumas subamostras 
plot.ts(X[1:1000]) 
plot.ts(V1[1:100])
plot.ts(V1[100:200])
plot.ts(V1[200:300])
plot.ts(V1[300:400])
plot.ts(V1[400:500])
plot.ts(V1[500:600])


#Periodograma
library(stats)
per1<-spec.pgram(X, log="no") # Grafica la densidad espectral estimada
per1$freq # Freqs onde se calculam as Densidades espectrais  
per1$spec # Densidades espectrais estimadas 
wmax1<-per1$freq[which.max(per1$spec)] # Em qual freq atinge o maximo
(s1<-1/wmax1) #Para calcular o periodo principal

# NOTA1: Podemos dizer que o primeiro periodo relevante P1 = 22,91242.
#        As demais frequencias numa vizinhanca dela sao muito similares.

#Tiramos os primeiros 1200 elemento del periodograma para localizar el segundo grupo de frequencias altas.
X2<-per1$freq
Y2<-per1$spec
length(X2)
plot(X2[-c(0:1200)],Y2[-c(0:1200)])
z<-X2[-c(0:1200)]
w<-Y2[-c(0:1200)]
max2<-z[which.max(w)] #Frequencia em que atinge o maximo
(ss1<-1/max2) # Periodo em que atinge o maximo

#NOTA2: Podemos dizer que o segundo periodo relevante P2 = 11,46789.
#       As demais frequencias numa vizinhanca dela sao muito similares.

#NOTA3: Pode-se observar que (2*ss1 \aprox s1) (2*11.46789 \aprox 22.91242), ou seja,
#       o periodo principal e dois vezes o periodo secundario.




