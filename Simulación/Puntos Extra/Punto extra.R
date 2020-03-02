###Punto exrtra
alpha<--0.001457235
sigma<-0.002060842
beta<-1.2
S0<- 3299.36
TT<-60
n<-1000
N<-50
dt<-TT/n
t<-seq(0,60-dt,dt)
W<-matrix(ncol=n,nrow=N)
for(i in 1:N)
{
W[i,]<-c(0,sqrt(dt)*cumsum(rnorm(n-1,0,1)))
}
STSM<-matrix(ncol=n,nrow=N)
STSM[,1]<-S0
for(j in 1:N)
{
  for(i in 1:(n-1))
{
  STSM[j,i+1]<-STSM[j,i]+alpha*STSM[j,i]*dt+sigma*STSM[j,i]^beta*(W[j,i+1]-W[j,i])+1/2*beta*sigma^2*STSM[j,i]^(2*beta-1)*((W[j,i+1]-W[j,i])^2-dt)
}
  }

media<-c()
for(i in 1:(n))
{
  media[i]<-mean(STSM[,i])
}
matplot(t,t(STSM),lty=3,type="l",lwd=1,ylab="Wt")
lines(t,media,col="red")
abline(h=0,v=0,col="purple",lwd=1)

