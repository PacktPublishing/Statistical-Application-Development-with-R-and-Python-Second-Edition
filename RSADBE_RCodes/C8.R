install.packages(c("ridge", "DAAG"))
library(RSADBE)

# Time for action – understanding overfitting
data(OF)
using plot(OF$X, OF$Y,"b",col="red",xlab="X", ylab="Y")
lines(OF$X,lm(Y~poly(X,1,raw=TRUE),data=OF)$fitted.values,"b",col="green")
lines(OF$X,lm(Y~poly(X,2,raw=TRUE),data=OF)$fitted.values,"b",col="wheat")
lines(OF$X,lm(Y~poly(X,3,raw=TRUE),data=OF)$fitted.values,"b",col="yellow")
lines(OF$X,lm(Y~poly(X,6,raw=TRUE),data=OF)$fitted.values,"b",col="orange")
lines(OF$X,lm(Y~poly(X,9,raw=TRUE),data=OF)$fitted.values,"b",col="black")
legend(6,50,c("Poly 1","Poly 2","Poly 3","Poly 6","Poly 9"),
col=c("green","wheat","yellow","orange","black"),pch=1,ncol=3)
R2 <- NULL; AdjR2 <- NULL; FStat <- NULL
Mvar <- NULL; PolyOrder<-1:9
for(i in 1:9)	{
	temp <- summary(lm(Y~poly(X,i,raw=T),data=OF))
	R2[i] <- temp$r.squared
	AdjR2[i] <- temp$adj.r.squared
	FStat[i] <- as.numeric(temp$fstatistic[1])
	Mvar[i] <- temp$sigma
		}
cbind(PolyOrder,R2,AdjR2,FStat,Mvar)
as.numeric(lm(Y~poly(X,1,raw=T),data=OF)$coefficients)
as.numeric(lm(Y~poly(X,2,raw=T),data=OF)$coefficients)
as.numeric(lm(Y~poly(X,3,raw=T),data=OF)$coefficients)
as.numeric(lm(Y~poly(X,4,raw=T),data=OF)$coefficients)
as.numeric(lm(Y~poly(X,5,raw=T),data=OF)$coefficients)
as.numeric(lm(Y~poly(X,6,raw=T),data=OF)$coefficients)
as.numeric(lm(Y~poly(X,7,raw=T),data=OF)$coefficients)
as.numeric(lm(Y~poly(X,8,raw=T),data=OF)$coefficients)

# Time for action – fitting piecewise linear regression models
data(PW_Illus)
attach(PW_Illus)
break1 <- X[which(X>=12 & X<=18)] 
break2 <- X[which(X>=27 & X<=33)]
n1 <- length(break1) 
n2 <- length(break2)
MSE_MAT <- matrix(nrow=(n1*n2), ncol=3) 
colnames(MSE_MAT) <- c("Break_1","Break_2","MSE") 
curriter=0 
for(i in 1:n1)	{ 	
	for(j in 1:n2)	{
 		curriter=curriter+1 	
		MSE_MAT[curriter,1]<-break1[i]
		MSE_MAT[curriter,2]<-break2[j] 	
		piecewise1 <- lm(Y ~ X*(X<break1[i])+X*(X>=break1[i] & X<break2[j])+X*(X>=break2[j])) 	
		MSE_MAT[curriter,3] <- as.numeric(summary(piecewise1)[6]) 			       
			}
                }
MSE_MAT[which(MSE_MAT[,3]==min(MSE_MAT[,3])),]
plot(PW_Illus)
pw_final <- lm(Y ~ X*(X<14)+X*(X>=14 & X<30)+X*(X>=30))
points(PW_Illus$X,pw_final$fitted.values,col ="red")

# Time for action – fitting the spline regression models
data(VD)
par(mfrow=c(1,2))
plot(VD)
title(main="Scatter Plot for the Voltage Drop")
VD_PRS <- lm(Voltage_Drop~Time+I(Time^2)+I(Time^3)+I(((Time-6.5)^3)*(sign(Time-6.5)==1))+I(((Time-13)^3)*(sign(Time-13)==1)),data=VD)
plot(VD) 
points(VD$Time,fitted(VD_PRS),col="red","l") 
title("Piecewise Cubic Polynomial Regression Model")
summary(VD_PRS)
VD_NCS <- lm(Voltage_Drop~ns(Time,knots=c(6.5,13),intercept= TRUE, degree=3), data=VD)
par(mfrow=c(1,2))
plot(VD) 
points(VD$Time,fitted(VD_NCS),col="green","l")
title("Natural Cubic Regression Model")
summary( VD_NCS)
VD_BS <- lm(Voltage_Drop~bs(Time,knots=c(6.5,13),intercept=TRUE, degree=3), data=VD)
plot(VD)
points(VD$Time,fitted(VD_BS),col="brown","l") 
title("B-Spline Regression Model")
summary(VD_BS)

# Time for action – ridge regression for the linear regression model
data(OF)
library(MASS); library(ridge)
LR <- linearRidge(Y~poly(X,3),data=as.data.frame(OF),lambda=c(0, 0.5,1,1.5,2,5,10,30))
LR
LR_Coef <- LR$coef
colSums(LR_Coef^2)
linearRidge(Y~poly(X,3),data=as.data.frame(OF),lambda="automatic")
summary(linearRidge(Y~poly(X,3),data=as.data.frame(OF),lambda="automatic"))
LM <-lm.ridge(Y~poly(X,3),data = as.data.frame(OF),lambda=c(0,0.5,1,1.5,2,5,10,30))
LM
LM_Coef <- LM$coef
colSums(LM_Coef^2)
data(Gasoline)
gasoline_lm <- lm(y~., data=Gasoline)
gasoline_rlm <- linearRidge(y~., data=Gasoline,lambda= "automatic")
sum(coef(gasoline_lm)[-1]^2)-sum(coef(gasoline_rlm)[-1]^2)
summary(gasoline_rlm)

# Time for action – ridge regression for the logistic regression model
data(German)
GC_RLR<-logisticRidge(as.numeric(good_bad)-1~.,data= as.data.frame(GC), lambda = "automatic")
summary(GC_LR) 
summary(GC_RLR)

# Time for action – selecting   iteratively and other topics
data(Gasoline)
Gasoline <- Gasoline[,-12]
set.seed(1234567)
data_part_label <- c("Train","Validate","Test") indv_label=sample(data_part_label,size=nrow(Gasoline),replace=TRUE ,prob=c(0.6,0.2,0.2))
G_Train <- Gasoline[indv_label=="Train",]
G_Validate <- Gasoline[indv_label=="Validate",]
G_Test <- Gasoline[indv_label=="Test",]
lambda <- seq(0,10,0.2)
Train_Errors <- vector("numeric",length=length(lambda))
Val_Errors <- vector("numeric",length=length(lambda))
plot(lambda,Val_Errors,"l",col="red",xlab=expression(lambda),ylab="Training and Validation Errors",ylim=c(0,600))
points(lambda,Train_Errors,"l",col="green")
legend(6,500,c("Training Errors","Validation Errors"),col=c( "green","red"),pch="-")
library(DAAG) 
data(VD) 
CVlm(df=VD,form.lm=formula(Voltage_Drop~Time+I(Time^2)+I(Time^3)+I(((Time-6.5)^3)*(sign(Time-6.5)==1))+I(((Time-13)^3)*(sign(Time-13)==1))),m=10,plotit="Observed")
LM_OF <- lm.ridge(Y~poly(X,3),data=as.data.frame(OF),lambda=c(0,0.5,1,1.5,2,5,10,30))
LM_OF$GCV
LM_GT <- lm.ridge(y~.,data=G_Train,lambda=seq(0,10,0.2))
LM_GT$GCV



