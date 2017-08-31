install.packages(c("stats4", "PASWR", "PairedData")
library(RSADBE)

# Maximum Likelihood Estimator

# Time for Action: Visualizing the Likelihood Function
par(mfrow=c(1,3))

# # Visualizing the Likelihood Function of a Binomial Distribution. 
n <- 10; x <- 7
p_seq <- seq(0,1,0.05)
plot(p_seq, dbinom(x=7,size=n,prob=p_seq), xlab="p", ylab="Binomial Likelihood Function", "l")

# Visualizing the Likelihood Function of a Poisson Distribution. 
x <- c(1, 2, 2, 1, 0, 2, 3, 1, 2, 4); n <- length(x)
lambda_seq=seq(0,5,0.1)
plot(lambda_seq,dpois(x=sum(x),lambda=n*lambda_seq),xlab=expression(lambda),ylab="Poisson Likelihood Function", "l")

# Visualizing the Likelihood Function of a Normal Distribution. 
set.seed(123)
n <- 25; xn <- rnorm(n,mean=10,sd=2)
mu_seq <- seq(9,11,0.05)
plot(mu_seq,dnorm(x=mean(xn),mean=mu_seq,sd=2),"l",xlab=expression(mu),ylab="Normal Likelihood Function")


# Time for Action: Finding the MLE using “mle” and “fitdistr” Functions
library(stats4)
# Example 5.1.4. (Example 5.1.1. Contd. )
x <- rep(c(0,1),c(3,7)); n <- NROW(x)
binomial_nll <- function(prob)	-sum(stats::dbinom(x,size=1,prob,log=TRUE))
fit_binom <- mle(binomial_nll,start=list(prob=0.5),nobs=n)
summary(fit_binom)

# Example 5.1.5. (Example 5.1.2. Contd. )
x <- c(1, 2, 2, 1, 0, 2, 3, 1, 2, 4); n <- NROW(x)
pois_nll <- function(lambda)-sum(stats::dpois(x,lambda,log=TRUE))
fit_poisson <- mle(pois_nll,start=list(lambda=median(x)),nobs=n, method <- "Brent", lower <- 0, upper <- 10)
summary(fit_poisson)

# Example 5.1.6. (Example 5.1.3. Contd. )
n <- NROW(xn)
normal_nll <- function(mean) -sum(stats::dnorm(xn,mean,sd=2,log=TRUE))
fit_normal <- mle(normal_nll,start=list(mean=8),nobs=n)
summary(fit_normal)

# Using the “fitdistr” Function
# Example 5.1.7 (Example 5.1.2. Contd. )
fitdistr(x,"poisson")

# Example 5.1.8 (Example 5.1.3. Contd. )
fitdistr(xn,"normal")

# TFA: CONFIDENCE INTERVALS
binom_CI <- function(x, n, alpha)	{
	phat <- x/n
	ll <- phat - qnorm(alpha/2,lower.tail=FALSE)*sqrt(phat*(1-phat)/n)
	ul <- phat + qnorm(alpha/2,lower.tail=FALSE)*sqrt(phat*(1-phat)/n)
	return(paste("The ", 100*(1-alpha),"% Confidence Interval for Binomial Proportion is (", round(ll,4),",",round(ul,4),")",sep=''))
					}
binom_CI(x=7,n=10,alpha=0.01)

normal_CI_ksd <- function(x,sigma,alpha)	{
	xbar <- mean(x)
	n <- length(x)
	ll <- xbar-qnorm(alpha/2,lower.tail=FALSE)*sigma/sqrt(n)	
	ul <- xbar+qnorm(alpha/2,lower.tail=FALSE)*sigma/sqrt(n)	
	return(paste("The ", 100*(1-alpha),"% Confidence Interval for the Normal mean is (", round(ll,4),",",round(ul,4),")",sep=''))
					}
normal_CI_ksd(x=xn,sigma=2,alpha=0.05)
normal_CI_ksd(x=xn,sigma=2,alpha=0.01)

normal_CI_uksd <- function(x,alpha)	{
	xbar <- mean(x); s <- sd(x)
	n <- length(x)
	ll <- xbar-qt(alpha/2,n-1,lower.tail=FALSE)*s/sqrt(n)	
	ul <- xbar+qt(alpha/2,n-1,lower.tail=FALSE)*s/sqrt(n)	
	return(paste("The ", 100*(1-alpha),"% Confidence Interval for the Normal mean is (", round(ll,4),",",round(ul,4),")",sep=''))
					}
normal_CI_uksd(x=xn,alpha=0.05)
normal_CI_uksd(x=xn,alpha=0.01)


# Hypotheses Testing

# # Time for Action: Testing Probability of Success
example(binom.test) 
n_lcd <- 893; x_lcd <- 39; p_lcd <- 0.04
binom.test(n=n_lcd,x=x_lcd,p=p_lcd,alternative="greater")
n_doc <- 119; x_doc <- 38; p_doc <- 0.2
binom.test(n=n_doc,x=x_doc,p=p_doc,alternative="two.sided")

# # Time for Action: Testing Proportions
UCBA.Dept <- ftable(UCBAdmissions, row.vars="Dept", col.vars <- c("Gender", "Admit"))
p_female <- UCBA.Dept[,3]/(UCBA.Dept[,3]+UCBA.Dept[,4])
p_female
prop.test(UCBA.Dept[,1:2],p=p_female)
T.Class <- ftable(Titanic, row.vars="Class", col.vars <- c("Sex", "Survived"))
p_female <- T.Class[,3]/(T.Class[,3]+T.Class[,4])
p_female
prop.test(T.Class[,1:2],p=p_female)

# mendel <- c(6022,2001)
# chisq.test(mendel,p=c(0.75,0.25))

chisq.test( UCBAdmissions[,,1])
chisq.test( UCBAdmissions[,,2])
chisq.test( UCBAdmissions[,,3])
chisq.test( UCBAdmissions[,,4])
chisq.test( UCBAdmissions[,,5])
chisq.test( UCBAdmissions[,,6])


# Time for Action: Testing Hypotheses – One Sample
library(PASWR)
pH_Data <- c(8.30, 8.42, 8.44, 8.32, 8.43, 8.41, 8.42, 
8.46, 8.37, 8.42)
pH_sigma <- 0.05
z.test(x=pH_Data,alternative="less",sigma.x=pH_sigma,mu=8.4)
LCD_Data <- c(13.37, 10.96, 12.06, 13.82, 12.96, 10.47, 
10.55, 16.28, 12.94, 11.43, 14.51, 12.63, 13.50, 11.50, 12.87)
LCD_sigma <- 2
z.test(x=LCD_Data,alternative="greater",sigma.x=LCD_sigma,mu=12)
peanuts <- c(8.08, 7.71, 7.89, 7.72, 8.00, 7.90, 7.77, 
7.81, 8.33, 7.67, 7.79, 7.79, 7.94, 7.84, 8.17, 7.87)
peanuts_sigma <- 0.03
z.test(x=peanuts,sigma.x=peanuts_sigma,mu=8.0)
# t-test
t.test(x=pH_Data,alternative="less",mu=8.4)
t.test(x=LCD_Data,alternative="greater",mu=12)
t.test(x=peanuts,alternative="two.sided",mu=8.0)
# Testing for the variance
library(PairedDate)
var.test(x=pH_Data,alternative="greater",ratio=7)
var.test(x=peanuts,alternative="two.sided",ratio=0.03)


# Time for Action: Testing Hypotheses – Two Sample
pH_Data <- c(8.30, 8.42, 8.44, 8.32, 8.43, 8.41, 8.42, 
8.46, 8.37, 8.42)
pH_New <- c(8.78, 8.85, 8.74, 8.83, 8.82, 8.79, 8.82, 
8.74, 8.84, 8.78, 8.75, 8.81)
z.test(x=pH_Data,y=pH_New,sigma.x=sigma.y=0.05,alternative="less")

length_M1 <- c(122.4, 123.12, 122.51, 123.12, 122.55, 
121.76, 122.31, 123.2, 122.48, 121.96)
length_M2 <- c(122.36, 121.88, 122.2, 122.88, 123.43, 
122.4, 122.12, 121.78, 122.85, 123.04)
z.test(x=length_M1,y=length_M2,sigma.x=0.5,sigma.y=0.5)

t.test(x=pH_Data,y=pH_New,alternative="less")
t.test(x=length_M1,y=length_M2)

machine_new <- c(8.06, 8.64, 7.97, 7.81, 7.93, 8.57, 8.39, 8.46, 8.28, 8.02, 8.39)
machine_old <- c(7.99, 8.12, 8.34, 8.17, 8.11, 8.03, 8.14, 8.14, 7.87)
t.test(machine_new,machine_old, alternative="greater")




