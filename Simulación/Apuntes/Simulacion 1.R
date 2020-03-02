#remover variables
ls()
x=2
ls()
rm(list=ls())
rm(x)
#Generador lineal
X=c()
u=c()
X[1]=7
a=7
b=3
M=2^10
for(i in 1:M) {
  X[i+1]=(a*X[i]+b) %% M
  u=c(u,X[i+1]/M)#diferente manera de entrar un vector
}
#X
#u
hist(u,freq=FALSE)
/////
  ##2do programa introductorio
rm(list=ls())##generar lista de todos los elementos y eliminarlos
X0<-7; a<-1093;b<-18257;M<-86436
X<-c()##vector vacio
u<-c()
X[1]<-X0#entrada del vector
u[1]<-X[1]/M
n<-M
for (i in 1:(M-1)) {
  X[i+1]<-(a*X[i]+b)%%M
  u[i+1]<-X[i+1]/M
}##ciclo
u1=u[1:(n-1)]##tomar los datos 1 al n-1 del vector u
u2<-u[2:n]
par(mfrow=c(1,2),pty="r")##poner mas de 1 grafica al mismo tiempo ##pty=plot type mfrow= numero de filas y columnas
hist(u,probability=T,nclass=10,col="powderblue")##nclass=numero de clases
abline(h=1,lty="twodash",col=33)#poner una linea h horizontal v vertical
##colors() despliega todos los colores
plot(u1,u2,pch=23,xlim=c(0,0.1),ylim=c(0,0.1))

#Prueba de ciclo maximo
M<-length(X)
print(X)
counter1=1
counter2=1
while (counter1<M) {
  counter1=counter1+1
  counter2=counter2+1
  if (X[1]==X[counter2]){
    counter1=M
    counter2=counter2-1
  }
}
if (counter2==M){
  print("es ciclo maximo")
}else {
  print("No es ciclo maximo")
}

rm(list=ls())
#Secuencia de pseudoaleatorios
U=c(.28,.42,.14,.16,.37,.83,.92,.87,.99,.55)
n=length(U)
U=sort(U);U #ordena en forma ascendente, a menos de decirle lo contrario
#grafica de la distribucion empirica y teorica

dist.emp=ecdf(U) #Calcula la distribucion acumulada
plot(dist.emp, col="blue", lwd=2,xlim=c(0,1))
abline(0,1,lty=45, col="red",lwd=2)

#construir el estadistico 
#i/n
dist.teoMAS=seq(from=0.1, to=1,by=.1);dist.teoMAS
#(i-1)/n
dist.teoMENOS=seq(from=0,to=0.9, by=.1);dist.teoMENOS#este punto y coma imprime lo que est?? despues
#DMENOS
DMenos=U-dist.teoMENOS;
DMenos
#DMAS
DMas=dist.teoMAS-U
for (i in 1:n) {
 DMas[i]=max(0,DMas[i])
}

DMas
D=max(DMenos,DMas);D
#comparar con el comando de R
kolmogorov.smirnov<-ks.test(U,"punif");kolmogorov.smirnov
#p-value=probabilidad de que la distancia dealejamiento supere al estadistico, es alfa
DMas




X0<-7; a<-1093;b<-18257;M<-100
X<-c()##vector vacio
u<-c()
X[1]<-X0#entrada del vector
u[1]<-X[1]/M
n<-M
for (i in 1:(M-1)) {
  X[i+1]<-(a*X[i]+b)%%M
  u[i+1]<-X[i+1]/M
}##ciclo


mi_generador<-function(n,X0,a,b,M)
{
  X=c()
  U=c()
  X[1]=X0
  U[1]=X[1]/M
  for (i in 1:(n-1)) {
    X[i+1]<-(a*X[i]+b)%%M
    U[i+1]<-X[i+1]/M
  }
  return(U) ##en una funci??n se tiene que pedir el valor obtenido
}
U=mi_generador(n=100,X0=7,a=1093,b=18257,M=86436)
U
n=length(U)
hist(U,freq=F,nclass=10,col="blue")
abline(h=1,lty=45,col="red",lwd=4)
#Prueba de Ji cuadrada para bondad de ajuste
cortes<-seq(from=0,to=1,by=0.1)#es para formar los extremos de las clases
intervalos<-cut(U,breaks=cortes)
clases<-table(intervalos)
k<-length(clases)
##frecuencias observadas
frec.observadas<-c()

for(i in 1:k){
  frec.observadas[i]=clases[i]
}
frec.observadas
#frecuencias esperadas
#Pj=nj/n  --->  E[nj]=n*Pj
frec.esperadas<-c()
for(i in 1:k)
{
  frec.esperadas[i]=n*(cortes[i+1]-cortes[i])
}
frec.esperadas
#Realizar el ajuste
EstT<-sum((frec.observadas-frec.esperadas)^2/frec.esperadas)
EstT
#Calcular Ji cuadrada con nivel de significancia alfa
alpha=.05
cuantil=qchisq(alpha,k-1,lower.tail=F)
if(EstT<cuantil)
{
  print("No se rechaza la hipotesis nula, la muestra tiene distribucion uniforme")
  
}else
{
  print("Se rechaza la hipotesis nula, la muestra no tiene distribucion uniforme")
}
p.valor=pchisq(EstT,k-1,lower.tail=F)
if(alpha<p.valor)
{
  print("No se rechaza la hipotesis nula, la muestra tiene distribucion uniforme")
  
}else
{
  print("Se rechaza la hipotesis nula, la muestra no tiene distribucion uniforme")
}
###?chisq.test
#comprobando
probabilidades=c()
for(i in 1:k)
{
  probabilidades[i]=cortes[i+1]-cortes[i]
}
chisq.test(frec.observadas,p=probabilidades)#prueba de bondad de ajuste chi cuadrada


r<-c(-3,1,3,-5,-.0001,.0001)
for (i in 1:6)
  {
  if (r[i]<0)
  {
    r[i]=0
  }
}

U2=mi_generador(n=100,X0=7,a=1093,b=18257,M=86436)
##Hacer las rachas para la prueba
rachas=ifelse(U2<=.5,-1,1)##comando if unicamente para vectores
rachas
n=length(rachas)
?which
which(rachas==1) ##dice cuales elementos del vector cumplen la caracter??stica
rachas[which(rachas==1)]
n1=length(rachas[which(rachas==-1)])
n2=n-n1
R=1 #inicio del conteo de rachas
for(i in 1:(n-1))
{
  if (rachas[i]!=rachas[i+1])
  {
    R=R+1
  }
}
R
media<-1+2*n1*n2/n
varianza<-2*n1*n2*(2*n1*n2-n)/(n^2*(n-1))
Z<-(R-media)/sqrt(varianza)
Z
##contraste
alpha=0.05

if(Z<0)
{
  z.a=qnorm(alpha/2)
}else
{
  z.a=qnorm(alpha/2,lower.tail = F)
}
z.a
if(Z<0)
{
  valp=2*pnorm(Z)
}else
{
  valp=2*pnorm(Z,lower.tail = F)
}
valp
##comprobando
install.packages("tseries")
library(tseries)
?runs.test
runs.test(rachas)
###:(  asi no corre porque rachas no es factor
runs.test(factor(rachas))

t=factor(rachas)
t
print("Hello, World")
###definiciones de un factor
v=c("a","r","a","g","o","n")
sort(v)
u=factor(v)
u
pi-22/7==0

n<-2^25
K<-0
#X<-c()
#Y<-c()
for(i in 1:n)
{
  V1=runif(1,-1,1)
  V2=runif(1,-1,1)

  if(V1^2+V2^2<=1)
  {
    K=K+1
    #X<-c(X,x)
    #Y<-c(Y,y)
  }
}
Pi=4*K/n
Pi-pi
#plot(X,Y,pch=20,asp=1)










