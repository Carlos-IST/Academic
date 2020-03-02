#Ejercicio 1
rm(list=ls())
m<-2^16; b<-3^8; a<-7^2; 
X0<-15
X<-c()
U<-c()
X[1]=X0
U[1]<-X[1]/m
for (i in 1:(m-1)){
  X[i+1]=((a*X[i]+b)%%m)
  U[i+1]<-X[i+1]/m
}
Smirnov<-ks.test(U,"punif")
rachas<-ifelse(U<=median(U),1,-1)
library(tseries)
runs.test(factor(rachas))

u1=U[1:(m-2)]##tomar los datos 1 al n-1 del vector u
u2<-U[2:(m-1)]
u3<-U[3:m]
u11=U[1:(m-1)]##tomar los datos 1 al n-1 del vector u
u22<-U[2:m]
par(mfrow=c(2,2),pty="r")
hist(U,freq=F,breaks="FD")
abline(h=1, lty=20,col="red")
plot(u11,u22,pch=23,xlim=c(0,0.1),ylim=c(0,0.1))
library(rgl)#libreria para visualizacion 3d
plot3d(u1,u2,u3,pch=23,xlim=c(0,0.5),ylim=c(0,0.5),zlim=c(0,0.5),col="yellow")
play3d(spin3d(rpm=20),duration=15)
#1.2
rm(list=ls())
m<-3^10; b<-5^4; a<-7^2; 
X0<-9
X<-c()
U<-c()
X[1]=X0
U[1]<-X[1]/m
for (i in 1:(m-1)){
  X[i+1]=((a*X[i]+b)%%m)
  U[i+1]<-X[i+1]/m
}
Smirnov<-ks.test(U,"punif")
rachas<-ifelse(U<=median(U),1,-1)
library(tseries)
runs.test(factor(rachas))
u1=U[1:(m-2)]##tomar los datos 1 al n-1 del vector u
u2<-U[2:(m-1)]
u3<-U[3:m]
u11=U[1:(m-1)]##tomar los datos 1 al n-1 del vector u
u22<-U[2:m]
par(mfrow=c(1,2),pty="s")
hist(U,freq=F,breaks="FD")
abline(h=.5, lty=20,col="red")
plot(u11,u22,pch=23,xlim=c(0,0.1),ylim=c(0,0.1))
plot3d(u1,u2,u3,pch=23,xlim=c(0,.3),ylim=c(0,.3),zlim=c(0,.3))
play3d( spin3d(  ), duration = 15 )
m<-length(X)
counter1=1
counter2=1
while (counter1<m) {
  counter1=counter1+1
  counter2=counter2+1
  if (X[1]==X[counter2]){
    counter1=m
    counter2=counter2-1
  }
}
if (counter2==m){
  print("es ciclo maximo")
}else {
  print("No es ciclo maximo")
}

#==============================================================================================================

#Ejercicio #2
rm(list=ls())
media=c()
m<-512; b<-1; a<-5; 
X<-c()
U<-c()
n<-1000
for (j in 1:n)
{
  X0<-sample(1:10000,1,replace=F)%%m
  X[1]=X0
  U[1]<-X[1]/(m)
  for (i in 1:(500)){
    X[i+1]=(a*X[i]+b)%%m
    U[i+1]<-X[i+1]/m
  }
media[j]=mean(X)
}
mediamedias<-mean(media)
desvmedias<-sd(media)

hist(media,probability=T,breaks="FD",col="royal blue")
abline(v=mediamedias,lty=22,col="yellow")
curve(dnorm(x,mediamedias,desvmedias),lty=22,col="red",add=T)

ks.test(media,"pnorm",mean=mediamedias,sd=desvmedias)

normal<-c((media-mediamedias)/desvmedias)
hist(normal,probability=T,breaks="FD",col="royal blue")
curve(dnorm(x),col="red",add=T)
X

ks.test(normal,"pnorm")

m<-length(X)
counter1=1
counter2=1
while (counter1<m) {
  counter1=counter1+1
  counter2=counter2+1
  if (X[1]==X[counter2]){
    counter1=m
    counter2=counter2-1
  }
}
if (counter2==m){
  print("es ciclo maximo")
}else {
  print("No es ciclo maximo")
}

#============================================================================================================

#ejercicio 3
rm(list=ls())
a<-2^16+3;m<-2^31
X0<-15
X<-c()
U<-c()
X[1]=X0
U[1]<-X[1]/m
for (i in 1:(m-1)){
  X[i+1]=((a*X[i])%%m)
  U[i+1]<-X[i+1]/m
}
u1=U[1:(m-1)]##tomar los datos 1 al n-1 del vector u
u2<-U[2:(m-1)]
u3<-U[3:m]
plot3d(u1,u2,u3,xlim=c(0,.5),ylim=c(0,.5),zlim=c(0,.5),col="red")

ks.test(U,"punif")
cortes<-seq(from=0,to=1,by=0.1)#es para formar los extremos de las clases
intervalos<-cut(U,breaks=cortes)
clases<-table(intervalos)
k<-length(clases)
frec.observadas<-c()

for(i in 1:k){
  frec.observadas[i]=clases[i]
}
probabilidades=c()
for(i in 1:k)
{
  probabilidades[i]=cortes[i+1]-cortes[i]
}
chisq.test(frec.observadas,p=probabilidades)
##Se contradice la aleatoriedad viendo los planos de Marsaglia

# =====================================================================================================================


#Ejercicio 4
a<-7^5;m<-2^31-1
X0=100
X<-c()
U<-c()
X[1]=X0
U[1]=X[1]/m
for (i in 1:(m-1)){
  X[i+1]=(a*X[i])%%m
  U[i+1]=X[i+1]/m
}
u1<-U[1:(m-2)]
u2<-U[2:(m-1)]
u3<-U[3:(m)]
plot3d(u1,u2,u3,xlim=c(0,.25),ylim=c(0,.25),zlim=c(0,.25),col="blue")
play3d(spin3d(rpm=20),duration=15)
##los planos de marsaglia del generador RAND muestran una aleatoriedad mucho mas alta que los de RANDU

# ====================================================================================================================

#Ejercicio 5
#Metodo de huecos
#Remover variables


U=c(4, 1, 3, 5, 1, 7, 2, 8, 2, 0, 7, 9, 1, 3, 5, 2, 7, 9, 4, 1, 6, 3,3, 9, 6, 3, 4, 8, 2, 3, 1, 9, 4, 4, 6, 8, 4, 1, 3, 8, 9, 5, 5, 7, 3, 9, 5, 9, 8, 5, 3, 2, 2, 3, 7, 4, 7, 0, 3, 6, 3, 5, 9, 9, 5, 5,5, 0, 4, 6, 8, 0, 4, 7, 0, 3, 3, 0, 9, 5, 7, 9, 5, 1, 6, 6, 3, 8, 8, 8, 9, 2, 9, 1, 8, 5, 4, 4, 5, 0, 2, 3, 9, 7, 1, 2, 0, 3, 6, 3)
E=c()
m=c()
Fac=unique(U)
n=length(Fac)

for(i in 1:n){
  #Construir las rachas
  #En cu??les entradas son iguales a cierto n??mero
  a=which(U==Fac[i])
  f=(a[2:length(a)] - a[1:(length(a)-1)])-1
  m=c(m,length(f))
  E=c(E,f)
}
m
#Formar los extremos de las clases
cortes=seq(from=min(E),to=max(E),by=(max(E)-min(E))/12); cortes
#Que corto y cada cuanto
intervalos=findInterval(E,vec=cortes,rightmost.closed=TRUE, all.inside=TRUE)

#Pone en una tabla a los intervalos o clases
clases=table(intervalos); clases
k=length(clases); k

#Frecuencias observadas
frec_observada_relativa=c()
frec_observada_relativa[1]=clases[1]/length(E)
for(i in 1:(k-1)){
  frec_observada_relativa[i+1]=frec_observada_relativa[i]+(clases[i+1]/length(E))
}
frec_observada_relativa

#Frecuencias esperadas
frec_esperada=c()
for(i in 1:k){
  frec_esperada[i]=1-0.9^(cortes[i+1]+1)
}
frec_esperada

#Realizar el ajuste 
#Estadistico
estadistico.T=sum(((frec_observada_relativa-frec_esperada)^2)/frec_esperada); estadistico.T

#Realizar la prueba de hipotesis calculando la Ji cuadrada con nivel de significancia alfa (por defecto es de cola izquierda)
alpha=0.05
cuantil=qchisq(1-alpha,k-1); cuantil

# como nuestro estadistico es mucho menor que el estadistico entonces nuestra muestra si se distribuye uniformenente
pvalor=1-pchisq(estadistico.T,k-1); pvalor

# ====================================================================================================================

#Ejercicio 6
rm(list=ls())
a<-2^16+3;m<-2^31
X<-c()
U<-c()
n<-1000
Estadist<-c()
pval<-c()
for (j in 1:n)
{
X0<-sample(1:2^20,1,replace=F)
X[1]=X0
U[1]<-X[1]/m
for (i in 1:(500-1)){
  X[i+1]=((a*X[i])%%m)
  U[i+1]<-X[i+1]/m
}
cortes<-seq(from=0,to=1,by=0.1)#es para formar los extremos de las clases
intervalos<-cut(U,breaks=cortes)
clases<-table(intervalos)
k<-length(clases)
frec.observadas<-c()

for(i in 1:k){
  frec.observadas[i]=clases[i]
}
probabilidades=c()
for(i in 1:k)
{
  probabilidades[i]=cortes[i+1]-cortes[i]
}
CHI<-chisq.test(frec.observadas,p=probabilidades)
Estadist[j]<-CHI[1]
pval[j]<-CHI[3]
}

DF=unname(as.numeric(CHI[2]))
pval=c(as.numeric(unlist(pval)))
Estadist=c(unname(as.numeric(unlist(Estadist))))
hist(Estadist,freq=F,breaks="FD",col="yellow",ylim=c(0,.1))
curve(dchisq(x,(DF)),col="red",add=T)
hist(pval,freq=F,breaks="FD",col="yellow")
curve(dunif(x),col="red",add=T)

# ====================================================================================================================

#Ejercicio 7
#El generador Mersenne-Twister es el generador m??s usado para generar n??meros pseudoaleatorios
#Funciona en muchos sistemas de programacion, como R, SAS, SPSS, y se creo definiendo el periodo
#con base en un numero primo de Mersenne, el cual tiene la forma 2^n-1, el que mas 
#comunmente se usa es Mn=2^19937-1

# ====================================================================================================================

#Ejercicio 8
rm(list=ls())
N<-c()
producto<-c()
options(scipen=100)
for(j in 1:100000)
{
  for(i in 1:100){
    ran<-runif(4)
    multi<-prod(ran)
    producto[i]<-multi
  }
N[j]<-max(producto)
}
mean(N)
##Converge a 0.5

# ===================================================================================================================

#Ejercicio 9
ran<-runif(1000000)
Expon<-exp(ran)
media1<-mean(ran)
media2<-mean(Expon)
vari<-c()
for(i in 1:1000000)
{
  vari[i]=(ran[i]-media1)*(Expon[i]-media2)
}
covarsim<-mean(vari)
covar<-cov(ran,exp(ran))
covar-covarsim

# =)================================================================================================

#Ejercicio 10
par(mfrow=c(1,1))
rm(list=ls())
R<-runif(100000)
phi<-2*pi*R
SEN<-sin(phi)
COS<-cos(phi)
COS1<-R*COS
SEN1<-R*SEN
plot(SEN1,COS1,xlim=c(-1,1),ylim=c(-1,1))

# =====================================================================================================================

#10 Dado un conjunto U1, U2,... de n??meros pseudoaleatorios distribu??dos uniformemente sobre el 
#inyervalo (0,1), se desea generar n??meros dispersos sobre el c??rculo de radio unitario sobre 
#el or??gen. Se sugiere generar las variables aleatorias R=Ui, fi=2*pi*Ui+1 donde R es la distancia 
#aleatoria al or??gen y fi es un ??ngulo aleatorio. Las coordenadas del punto son R*cos(fi) y R*sin(fi)
#??Es aceptable el m??todo comparado con el empleado para estimar pi, en el que se emplea un cuadrado 
#de [-1,1]x[-1,1]? Realice las pruebas pertinentes.

rm(list = ls())
u<-runif(10000,min = 0, max = 1)
n<-length(u)
R<-u[-length(u)]
fi<-2*pi*u[2:length(u)]
u1<-R*cos(fi)
u2<-R*sin(fi)

a<-ifelse((u1^2+u2^2)<=1,1,0)
p<-sum(a)/n
pi1<-p*4
pi1

plot(u1,u2,pch=20, asp=1, xlim = c(-1,1),ylim = c(-1,1))

#Este m??todo no es aceptable, pues los pares ordenadados no se distribuyen de una manera uniforme
#en el cuadrado [-1,1]x[-1,1]. si no que la mayor??a queda en el circulo de radio 1 con centro en el
#origen



