##Metodo de la transformada inversa
rm(list=ls())
#Ejemplo distr. exponencial
n=10000
U<-runif(n)
lambda<-2
X<--1/lambda*log(1-U)
hist(X,freq=F,col="yellow",breaks="FD")
curve(dexp(x,lambda),lty=25,col="red",add=T)
#?hist
ks.test(X,"pexp",lambda)
library(tseries)
rachas2=ifelse(X<=median(X),-1,1)
runs.test(factor(rachas))
rachas2=ifelse(X<=lambda,-1,1)
runs.test(factor(rachas2))
#Ejemplo distr. Weibull
n=10000
U<-runif(n)
beta<-2
X2<-(-log(1-U))^(1/beta)
hist(X2,freq=F,col="yellow",breaks="FD")
curve(dweibull(x,beta),lty=25,col="red",add=T)
ks.test(X2,"pweibull",beta)
library(tseries)
rachas2=ifelse(X2<=median(X2),-1,1)
runs.test(factor(rachas2))
#Ejemplo distr. Cauchy
n=100000
U<-runif(n)
X3<-tan(pi*(U-1/2))
hist(X3,freq=F,col="yellow",breaks="FD",xlim=c(-3,3))
curve(dcauchy(x),lty=25,col="red",add=T,xlim=c(-3,3))
ks.test(X3,"pcauchy")
library(tseries)
rachas3=ifelse(X3<=median(X3),-1,1)
runs.test(factor(rachas3))
?dcauchy
plot(dcauchy)
#Ejemplo construida
n=100000
df<-function(x,N)((N+1)*x^N)
pf<-function(x,N)(x^(N+1))
N<-2
U<-runif(n)
X4<-U^(1/(N+1))
par(mfrow=c(1,2),pty="s")
hist(X4,freq=F,col="yellow",breaks="FD")
curve(df(x,N),lty=25,col="red",add=T)
curve(pf(x,N),lwd=2,col="blue")
plot(ecdf(X4),add=T,col="pink")
ks.test(X4,"pf",N)
library(tseries)
rachas4=ifelse(X4<=median(X4),-1,1)
runs.test(factor(rachas4))
#Ejemplo triangular
dtrian<-function(x,a,b,C)
{
  2*(x-a)/((b-a)*(C-a))*(x>=a & x<=b) + 2*(C-x)/((C-a)*(C-b))*(x>=b & x<C)
  #if(x>=a & x<=b)
  #{
#2*(x-a)/((b-a)*(C-a))
  #}else if(x>=b & x<C)
 # {
  #  2*(C-x)/((C-a)*(C-b))
  #}
}
ptrian<- function(x,a,b,C)
{
  (x-a)^2/((b-a)*(C-a))*(x>=a & x<=b) + (1-(C-x)^2/((C-b)*(C-a)))*(x>=b & x<C)
}
a=0;b=100;C=200;n=10000
U<-runif(n)
X<-c()
for(i in 1:n)
{
  if(U[i]<(b-a)/(C-a))
  {
    x=a+sqrt((b-a)*(C-a)*U[i])
  }else
  {x=C-sqrt((C-a)*(C-b)*(1-U[i]))}
  X<-c(X,x)
}
hist(X,freq=F,col="yellow",breaks="FD",xlim=c(a,C))
curve(dtrian(x,a,b,C),lty=25,col="red",add=T)
##Ejemplo uniforme a,b

rm(list=ls())
a<-2
b<-5
n<-1000000
U<-runif(n)
X<-a+(b-a)*U
par(mfrow=c(1,2))
hist(X,freq=F,breaks="FD")
abline(h=.4)
plot(ecdf(X),lwd=4)
curve(punif(x,a,b),add=T,col="red")


## aceptacion rechazo dominio finito
rm(list=ls())
n=100000
ALFA<-2
BETA<-3
a=0;b=1
X<-c()
C<-max(dbeta(c(0,1,(ALFA-1)/(ALFA+BETA-2)),ALFA,BETA)) #maximo de los puntos criticos de la beta
for(i in 1:n)
{
Y<-a+(b-a)*runif(1) #se puede poner runif(1,a,b)
U<-runif(1)
while(U>=dbeta(Y,ALFA,BETA)/C)
{
  Y<-a+(b-a)*runif(1) #se puede poner runif(1,a,b)
  U<-runif(1)
}
X<-c(X,Y)
}

hist(X,freq=F,breaks="FD",xlim=c(a,b))
curve(dbeta(x,ALFA,BETA),col="blue",add=T)
ks.test(X,"pbeta",ALFA,BETA)
#aceptacion rechazo dominio infinito
#normal
n=100000
df<-function(x)(2/sqrt(2*pi)*exp(-(x^2)/2))
C<-max(df(1)/dexp(1,1))

X<-c()
for(i in 1:n)
{
Y<--log(1-runif(1))
U<-runif(1)
while(U>=df(1)/(C*dexp(1,1)))
{
  Y<-log(1-runif(1))
  U<-runif(1)
}
X<-c(X,Y)
}
hist(X,freq=F,breaks="FD",xlim=c(0,3))
curve(df(x),col="orange",add=T,xlim=c(0,3))
curve(C*dexp(x),col="green",add=T,xlim=c(0,3))
ks.test(X,"dnorm")
###

# Casos Especiales

# Simulaci??n de una normal

rm(list = ls())
n=50000
U1=runif(n)
U2=runif(n)
Theta=2*pi*U1
R=sqrt(-2*log(1-U2))
X=R*cos(Theta)
Y=R*sin(Theta)
hist(X,freq = F,breaks = "FD", col = "yellow" )
curve(dnorm(x,0,1),add = T,col="red",lwd=2)
## Prueba de bonndad de ajuste 


ks.test(X,"pnorm")


plot(ecdf(X))
curve(pnorm(x,0,1),add = T, col ="red")

## Simulaci??n de una gamma

rm(list = ls())
L=2 ## lambda
K=10 ## #de exponenciales
n=1000000   # #de simulaciones
Y=c()
for (i in 1:n) {
  U=runif(K)
  X=(-1/L)*log(U)
  Y[i]=sum(X)
}
hist(Y, freq = F, breaks = "FD",col="turquoise1")
curve(dgamma(x,K,L),col="purple",add = T,lwd=2)
ks.test(Y,"pgamma",K,L)

###Simulacion de variables aleatorias discretas
##Primer ejemplo
rm(list=ls())
n<-100000
X<-c(1,7,15,20)
p<-c(.2,.3,.1,.4)
f<-cumsum(p)
XS<-c()
for(i in 1:n)
{
U<-runif(1)
if (U<f[1])
{
  XS[i]<-X[1]
}else if (U<f[2])
{
  XS[i]<-X[2]
}else if(U<f[3])
{
  XS[i]<-X[3]
}else
{
  XS[i]<-X[4]
}
}
hist(XS,freq=F,col="turquoise")

##Ejemplo uniforme discreta
N<-100000
n<-5
U<-runif(N)
X<-floor(n*U)+1
hist(X,probability=T,col="seagreen",breaks=c(0,1,2,3,4,5))
##Ejemplo Poisson
lambda<-sample(1:100,1)
n<-100000
X<-c()
for(j in 1:n)
{
p<-exp(-lambda)
suma<-p
U<-runif(1)
i=0
while(U>suma)
{
  p=p*lambda/(i+1)
  suma=suma+p
  i=i+1
}
X[j]<-i
}
hist(X,freq=F,col="yellow",breaks=seq(0,max(X),by=1))

##Aceptacion rechazo variables discretas
rm(list = ls())
p=c(.2,.3,.1,.4)
n=length(p)
q=rep(1/n,n)
C=max(p/q)
N=10000
X=c()
for (i in 1:N) {
  Y=floor(n*runif(1))+1
  U=runif(1)
  while(U>=(p[Y]/(C*q[Y]))){
    Y=floor(n*runif(1))+1
    U=runif(1)
  }
  X[i]=Y  
}
hist(X,freq = F,breaks = seq(0,max(X),1),col = 31)
?rep


