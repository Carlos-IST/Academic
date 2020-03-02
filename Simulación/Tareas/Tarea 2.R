##Ejercicio 1
###Uniforme
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

###Exponencial
n=10000
U<-runif(n)
lambda<-sample(1:1000,1)
X<--1/lambda*log(1-U)
hist(X,freq=F,col="yellow",breaks="FD")
curve(dexp(x,lambda),lty=25,col="red",add=T)
ks.test(X,"pexp",lambda)

###Gamma
curve(dgamma(x,L,K))
curve(dchisq(x,1),add=T)


###Erlang
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

###Beta
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

###Normal
rm(list = ls())
n=50000
U1=runif(n)
U2=runif(n)
Theta=2*pi*U1
R=sqrt(-2*log(1-U2))
X=R*cos(Theta)
Y=R*sin(Theta)
hist(Y,freq = F,breaks = "FD", col = "yellow" )
curve(dnorm(x,0,1),add = T,col="red",lwd=2)
ks.test(X,"pnorm")
plot(ecdf(X))
curve(pnorm(x,0,1),add = T, col ="red")

###Cauchy
n=100000
U<-runif(n)
X3<-tan(pi*(U-1/2))
hist(X3,freq=F,col="yellow",breaks="FD",xlim=c(-3,3))
curve(dcauchy(x),lty=25,col="red",add=T,xlim=c(-3,3))
ks.test(X3,"pcauchy")
library(tseries)
rachas3=ifelse(X3<=median(X3),-1,1)
runs.test(factor(rachas3))

###Chi-cuadrada

###T de student

###Weibull
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
#====================================================================================================================
##Ejecicio 2
rm(list=ls()) 
pcreada<- function(x)((x^2+x)/2)
dcreada<-function(x)(x+1/2)
n=1000000
U<-runif(n)
##Hacer truco de completar cuadrados para despejar X
X<-sqrt(2*U+1/4)-1/2
hist(X,freq=F,breaks="FD",col="yellow")
curve(dcreada(x),col="red",add=T)
ks.test(X,"pcreada")

#=====================================================================================================================
##Ejercicio 3
n=100000
dens<-function(x)
  {(x/2-1)*(x>=2 & x<=3)+(1-x/6)*(x>3 & x<=6)}
distri<-function(x)
{(x/2-1)^2*(x>=2 & x<=3)+(1-(x/(2*sqrt(3))-sqrt(3))^2)*(x>3 & x<=6)}
U<-runif(n)
  X<-c()
  for(i in 1:n)
{if(U[i]>=0 & U[i]<=1/4)
{
  X[i]=2*(sqrt(U[i])+1)
}else if (U[i]>.25 & U[i]<=1)
  {
  X[i]=12-2*sqrt(3)*(sqrt(1-U[i])+sqrt(3))
}}
hist(X,freq=F,breaks="FD",col="seagreen",xlim=c(2,8))  
curve(dens,col="red",add=T)
ks.test(X,distri)

#=================================================================================================================
#Ejercicio 4
rm(list=ls())
n=100000
mulaplace=0
blaplace=2
densidad<-function(x)
{
  (exp(2*x))*(x>-Inf & x<=0)+(exp(-2*x))*(x>0 & x<Inf)
}
curve(densidad(x),xlim=c(-20,20),ylim=c(0,1))
distribucion<-function(x)
{
  (exp(2*x)/2)*(x>-Inf & x<=0)+(exp(-2*x)/2)*(x>0 & x<Inf)
}
curve(distribucion(x),xlim=c(-20,20))
curve(dlaplace(x,0,2),xlim=c(-20,20),add=T)
C<-max(densidad(0)/dlaplace(0,0,2))
X<-c()
for(i in 1:n)
{
  U1<-runif(1)
  if(U1<1/2)
  {
    Y<-blaplace*log(2*U1)+mulaplace
    }else
    {
      Y<-blaplace*log(1/(2*(1-U1)))+mulaplace
    }
  U<-runif(1)
  while(U>=densidad(Y)/(C*dlaplace(Y,0,2)))
  {
    U1<-runif(1)
    if(U1<1/2)
    {
      Y<-blaplace*log(2*U1)+mulaplace
    }else
    {
      Y<-blaplace*log(1/(2*(1-U1)))+mulaplace
    }
    U<-runif(1)
  }
  X<-c(X,Y)
}
hist(X,freq=F,breaks="FD",xlim=c(-5,5))
curve(densidad(x),col="orange",add=T)
curve(C*dlaplace(x,0,2),col="green",add=T)
ks.test(X,"densidad")

#===================================================================================================================
##Ejercicio 5
rm(list=ls())
n=100000
mulaplace=0
blaplace=1
mulogit=0
blogit=1
df<-function(x)(1/(2*blaplace)*exp(-abs(x-mulaplace)/blaplace))
C<-max(df(0)/dlogis(0,mulogit,blogit))
X<-c()
for(i in 1:n)
{

    Y<--blogit*log(1/runif(1)-1)+mulogit
  U<-runif(1)
  while(U>=df(Y)/(C*dlogis(Y,mulogit,blogit)))
  {
      Y<--blogit*log(1/runif(1)-1)+mulogit
    U<-runif(1)
  }
  X<-c(X,Y)
}
hist(X,freq=F,breaks="FD",xlim=c(-5,5))
curve(df(x),col="orange",add=T)
curve(C*dlogis(x,mulogit,blogit),col="green",add=T)
ks.test(X,"plaplace",mulaplace,blaplace)


floor(3)
ceiling(3)

