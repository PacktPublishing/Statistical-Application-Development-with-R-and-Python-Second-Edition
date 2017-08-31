install.packages(c("pscl","ROCR"))
library(RSADBE)
# Time for Action: Limitation of Linear Regression Model 
data(sat)
plot(sat$Sat,sat$Pass,xlab="SAT Score", ylab = "Final Result")
passlm <- lm(Pass~Sat,data=sat)
summary(passlm)
abline(passlm)
predict(passlm,newdata=list(Sat=400))
predict(passlm,newdata=list(Sat=700),interval="prediction")

# Time for Action: Probit Regression Model
pass_probit <- glm(Pass~Sat,data=sat,binomial(probit))
summary(pass_probit)
library(pscl)
pR2(pass_probit)
predict(pass_probit,newdata=list(Sat=400),type = "response")
predict(pass_probit,newdata=list(Sat=700),type = "response")

# Time for Action: Fitting the Logistic Regression Model
pass_logistic <- glm(Pass~Sat,data=sat,family = 'binomial')
summary.glm(pass_logistic)
pR2(pass_logistic)
with(pass_logistic, pchisq(null.deviance - deviance, df.null 
- df.residual, lower.tail = FALSE))
confint(pass_logistic)
predict.glm(pass_logistic,newdata=list(Sat=400),type = "response")
predict.glm(pass_logistic,newdata=list(Sat=700),type = "response")
sat_x <- seq(400,700, 10)
pred_l <- predict(pass_logistic,newdata=list(Sat=sat_x),type="response")
pred_p <- predict(pass_probit,newdata=list(Sat=sat_x),type="response")
plot(sat_x,pred_l,type="l",ylab="Probability",xlab="Sat_M")
lines(sat_x,pred_p,lty=2)

# Time for Action: Hosmer-Lemeshow Goodness-of-Fit Statistic
# Source: http://sas-and-r.blogspot.in/2010/09/example-87-hosmer-and-lemeshow-goodness.html
pass_hat <- fitted(pass_logistic)
hosmerlem <- function(y, yhat, g=10) 	{
  cutyhat <- cut(yhat,breaks = quantile(yhat, probs=seq(0,1, 1/g)), include.lowest=TRUE)
     obs = xtabs(cbind(1 - y, y) ~ cutyhat)
     expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
     chisq = sum((obs - expect)^2/expect)
     P = 1 - pchisq(chisq, g - 2)
  return(list(chisq=chisq,p.value=P))
					}
hosmerlem(pass_logistic$y, pass_hat)


# Time for Action: Residual Plots for Logistic Regression Model 
par(mfrow=c(1,3), oma=c(0,0,3,0))
plot(fitted(pass_logistic), residuals(pass_logistic,"response"), col="red", xlab="Fitted Values", ylab="Response Residuals")
points(fitted(pass_probit), residuals(pass_probit,"response"), col="green")
abline(h=0)
plot(fitted(pass_logistic), residuals(pass_logistic,"deviance"), col="red", xlab="Fitted Values", ylab="Deviance Residuals")
points(fitted(pass_probit), residuals(pass_probit,"deviance"), col="green")
abline(h=0)
plot(fitted(pass_logistic), residuals(pass_logistic,"pearson"), col="red", xlab="Fitted Values", ylab="Pearson Residuals")
points(fitted(pass_probit), residuals(pass_probit,"pearson"), col="green")
abline(h=0)
title(main="Response, Deviance, and Pearson Residuals Comparison for the Logistic and Probit Models",outer=TRUE)

# Time for Action: Diagnostics for the Logistic Regression
hatvalues(pass_logistic)
cooks.distance(pass_logistic)
dfbetas(pass_logistic)
dffits(pass_logistic)
cbind(hatvalues(pass_logistic),cooks.distance(pass_logistic),
dfbetas(pass_logistic),dffits(pass_logistic))
hatvalues(pass_logistic)>2*(length(pass_logistic$coefficients)-1)
/length(pass_logistic$y)
cooks.distance(pass_logistic)>qf(0.1,length(pass_logistic$coefficients),
length(pass_logistic$y)-length(pass_logistic$coefficients))
cooks.distance(pass_logistic)>qf(0.5,length(pass_logistic$coefficients),
length(pass_logistic$y)-length(pass_logistic$coefficients))
par(mfrow=c(1,3))
plot(dfbetas(pass_logistic)[,1],ylab="DFBETAS - INTERCEPT")
plot(dfbetas(pass_logistic)[,2],ylab="DFBETAS - SAT")
plot(dffits(pass_logistic),ylab="DFFITS")


# Time for Action: ROC Construction
pred_prob <- c(0.32, 0.62, 0.19, 0.75, 0.18, 0.18, 0.95, 0.79, 0.24, 0.59)
(pred_prob <- sort(pred_prob,decreasing=T))
pred_prob <- (pred_prob-min(pred_prob))/(max(pred_prob)-min(pred_prob))
pred_prob


data(simpledata)
threshold <- seq(1,0,-0.01)
P <- sum(simpledata$Label==1)
N <- sum(simpledata$Label==0)
tpr=fpr=threshold*0

for(i in 1:length(threshold))	{
      FP=TP=0
      for(j in 1:nrow(simpledata))	{
	    if(simpledata$Predictions[j]>=threshold[i])		{
	    if(simpledata$Label[j]==1) TP=TP+1 else FP=FP+1
								 }
					 }
      tpr[i] <- TP/P
      fpr[i] <- FP/N
				 }
plot(fpr,tpr,"l",xlab="False Positive Rate", ylab="True Positive Rate",col="red", xlim=c(0,1),ylim=c(0,1))
abline(a=0,b=1)


# Time for Action: Logistic Regression for the German Credit Dataset
library(ROCR)
data(GC)
GC_LR <- glm(good_bad~.,data=GC,family=binomial())
summary(GC_LR)
LR_Pred <- predict(GC_LR,type='response')
GC_pred <- prediction(LR_Pred,GC$good_bad)
GC_perf <- performance(GC_pred,"tpr","fpr")
plot(GC_perf)


