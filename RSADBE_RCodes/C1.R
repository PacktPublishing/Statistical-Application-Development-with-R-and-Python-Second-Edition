install.packages("RSADBE")
library(RSADBE)

data(SQ)
class(SQ)
variable.names(SQ)
sapply(SQ,class)

install.packages(c("qcc","ggplot2")) 

M <- 10
mylabels <- 1:M
prob_labels <- rep(1/M,length(mylabels))
dotchart(prob_labels,labels=mylabels,xlim=c(.08,.12),xlab="Probability")
title("A Dot Chart for Probability of Discrete Uniform RV")

n <- 10; p <- 0.5
p_x <- round(dbinom(x=0:10, n, p),4)
plot(x=0:10,p_x,xlab="x", ylab="P(X=x)")

n <- 83; p <- 0.01
dbinom(10,n,p)
dbinom(20,n,p)
dbinom(30,n,p)
sum(dbinom(0:83,n,p))

n <- 83; p <- seq(0.05,0.95,0.05)
x <- seq(0,83,5)
i <- 1
plot(x,pbinom(x,n,p[i]),"l",col=1,xlab="x",ylab=expression(P(X<=x)))
for(i in 2:length(p)) { points(x,pbinom(x,n,p[i]),"l",col=i)}

N = 200; M = 20
n = 10
x = 0:11
round(dhyper(x,M,N,n),3)

1-pnbinom(3,size=12,0.95)
1-(dnbinom(3,size=12,0.95)+dnbinom(2,size=12,0.95)+dnbinom(1,size=12,0.95)+dnbinom(0,size=12,0.95))

dpois(0,lambda=3); dpois(5,lambda=3); dpois(20, lambda=3)

punif(0.58)-punif(0.35)

par(mfrow=c(1,2))
curve(dexp(x,1),0,10,ylab="f(x)",xlab="x",cex.axis=1.25)
curve(dexp(x,0.2),add=TRUE,col=2)
curve(dexp(x,0.5),add=TRUE,col=3)
curve(dexp(x,0.7),add=TRUE,col=4)
curve(dexp(x,0.85),add=TRUE,col=5)
legend(6,1,paste("Rate = ",c(1,0.2,0.5,0.7,0.85)),col=1:5,pch= "___")
curve(dexp(x,50),0,0.5,ylab="f(x)",xlab="x")
curve(dexp(x,10),add=TRUE,col=2)
curve(dexp(x,20),add=TRUE,col=3)
curve(dexp(x,30),add=TRUE,col=4)
curve(dexp(x,40),add=TRUE,col=5)
legend(0.3,50,paste("Rate = ",c(1,0.2,0.5,0.7,0.85)),col=1:5,pch= "___")

par(mfrow=c(3,1))
# Probability Z Greater than 0
curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)")
z=seq(0,4,0.02)
lines(z,dnorm(z),type="h",col="grey")
# 95% Coverage
curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)")
z=seq(-1.96,1.96,0.001)
lines(z,dnorm(z),type="h",col="grey")
# 95% Coverage
curve(dnorm(x,0,1),-4,4,xlab="z",ylab="f(z)")
z=seq(-2.58,2.58,0.001)
lines(z,dnorm(z),type="h",col="grey")


