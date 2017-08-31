install.packages("faraway")
library(RSADBE)

# Essential Summary Statistics
setwd("C:/Users/prabhanjan_tattar/Documents/Prabhanjan/Packt/RLAB")

# IO_Time = read.csv("IO_Time.csv",header=T) # Executed already in C4_Exploratory_Analysis.R
# attach(IO_Time) # Executed already in C4_Exploratory_Analysis.R

# Time for Action: Arbitrary Choice of Parameters
data(IO_Time)
par(mfrow=c(1,3))
plot(IO_Time$No_of_IO,IO_Time$CPU_Time,xlab="Number of Processes",ylab="CPU Time",ylim=c(0,0.6),xlim=c(0,11))
abline(a=0.05,b=0.05,col="blue")
myline1 = function(x) 0.05*x+0.05
for(i in 1:length(IO_Time$No_of_IO)) 	{
		lines(c(IO_Time$No_of_IO[i],IO_Time$No_of_IO[i]),c(IO_Time$CPU_Time[i],myline1(IO_Time$No_of_IO[i])),col="blue",pch=10)
				}
title("Residuals for the First Guess")

plot(IO_Time$No_of_IO,IO_Time$CPU_Time,xlab="Number of Processes",ylab="CPU Time",ylim=c(0,0.6),xlim=c(0,11))
abline(a=0.1,b=0.04,col="green")
myline2 = function(x) 0.04*x+0.1
for(i in 1:length(IO_Time$No_of_IO)) 	{
		lines(c(IO_Time$No_of_IO[i],IO_Time$No_of_IO[i]),c(IO_Time$CPU_Time[i],myline2(IO_Time$No_of_IO[i])),col="green",pch=10)
				}
title("Residuals for the Second Guess")

plot(IO_Time$No_of_IO,IO_Time$CPU_Time,xlab="Number of Processes",ylab="CPU Time",ylim=c(0,0.6),xlim=c(0,11))
abline(a=0.15,b=0.03,col="yellow")
myline3 = function(x) 0.03*x+0.15
for(i in 1:length(IO_Time$No_of_IO)) 	{
		lines(c(IO_Time$No_of_IO[i],IO_Time$No_of_IO[i]),c(IO_Time$CPU_Time[i],myline3(IO_Time$No_of_IO[i])),col="yellow",pch=10)
				}
title("Residuals for the Third Guess")

# Time for Action: Building a Simple Linear Regression Model
IO_lm = lm(CPU_Time ~ No_of_IO,data=IO_Time)
class(IO_lm)
summary(IO_lm)

# Time for Action: ANOVA and Confidence Intervals
IO_anova = anova(IO_lm)
IO_anova
confint(IO_lm)


# Time for Action: Residual Plots for Model Validation
IO_lm_resid=resid(IO_lm) 
par(mfrow=c(3,2))
plot(No_of_IO, IO_lm_resid,main="Plot of Residuals Vs
Predictor Variable",ylab="Residuals",xlab="Predictor Variable")
plot(No_of_IO, abs(IO_lm_resid), main="Plot of
Absolute Residual Values Vs Predictor Variable",
ylab="Absolute Residuals", xlab="Predictor Variable")
# Equivalently
plot(No_of_IO, IO_lm_resid^2,main="Plot of
Squared Residual Values Vs Predictor Variable",
ylab="Squared Residuals", xlab="Predictor Variable")
plot(IO_lm$fitted.values,IO_lm_resid, main=
"Plot of Residuals Vs Fitted Values",
ylab="Residuals", xlab="Fitted Values")
plot.ts(IO_lm_resid, main="Sequence Plot of the Residuals")
boxplot(IO_lm_resid,main="Box Plot of the Residuals")

rpanova = anova(IO_lm)
IO_lm_resid_rank=rank(IO_lm_resid)
tc_mse=rpanova$Mean[2]
IO_lm_resid_expected=sqrt(tc_mse)*qnorm((IO_lm_resid_rank-0.375)
/(length(CPU_Time)+0.25))
plot(IO_lm_resid,IO_lm_resid_expected,xlab="Expected",ylab=
"Residuals",main="The Normal Probability Plot")
abline(0,1)


# Multiple Linear Regression Model

# Time for Action: Averaging k Simple Linear Regression
y = c(1,5,3,8,5,3,10,7)
x1 = c(2,4,5,6,8,10,11,13)
x2 = c(1,2,2,4,4,4,6,6)
par(mfrow=c(1,3)) # Graphical output suppressed
plot(x1,y)
plot(x2,y)
plot(x1,x2)
summary(lm(y~x1))
summary(lm(y~x2))
summary(lm(y~x1+x2)) # Our first multiple regression model

# Time for Action: Building a Multiple Linear Regression Model
# Gasoline = read.csv("Gasoline_Mileage.csv",header=TRUE, row.names=1)
data(Gasoline)
gasoline_lm = lm(y~., data=Gasoline)
summary(gasoline_lm)

# Time for Action: ANOVA and Confidence Intervals for the Multiple Linear Regression Model
gasoline_anova=anova(gasoline_lm)
gasoline_anova
confint(gasoline_lm)
gasoline_fitted = gasoline_lm$fitted.values

# Time for Action: Residual Plots for the Multiple Linear Regression Model
gasoline_lm_mse=gasoline_anova$Mean[length(gasoline_anova$Mean)]
stan_resid_gasoline=resid(gasoline_lm)/sqrt(gasoline_lm_mse)
#Standardizing the residuals
studentized_resid_gasoline=resid(gasoline_lm)/(sqrt(gasoline_lm_mse*(1-hatvalues(gasoline_lm)))) #Studentizing the residuals
pred_resid_gasoline=rstandard(gasoline_lm)
pred_student_resid_gasoline=rstudent(gasoline_lm)
# returns the R-Student Prediction Residuals
par(mfrow=c(2,2))
plot(gasoline_fitted,stan_resid_gasoline,xlab="Fitted",ylab="Residuals")
title("Standardized Residual Plot")
plot(gasoline_fitted,studentized_resid_gasoline,xlab="Fitted",ylab="Residuals")
title("Studentized Residual Plot")
plot(gasoline_fitted,pred_resid_gasoline,xlab="Fitted",ylab="Residuals")
title("PRESS Plot")
plot(gasoline_fitted,pred_student_resid_gasoline,xlab="Fitted",ylab="Residuals")
title("R-Student Residual Plot")
R-Student Residuals

# Leverage Points
hatvalues(gasoline_lm)
which(hatvalues(gasoline_lm) > length(gasoline_lm$coefficients)/nrow(Gasoline))

# Influential Points
cooks.distance(gasoline_lm)


# Multicollinearity Problem
round(cor(Gasoline[,-c(1,12)],use="comp"),2)
library(faraway)
vif(Gasoline[,-c(1,12)])
vif(Gasoline[,-c(1,4,12)])
vif(Gasoline[,-c(1,4,11,12)])
vif(Gasoline[,-c(1,2,4,11,12)])
vif(Gasoline[,-c(1,2,3,4,11,12)])

summary(lm(y~x4+x5+x6+x7+x8+x9,data=Gasoline))


# Model Selection
# The Backward Selection Methodology
pvalueslm=function(lm) {summary(lm)$coefficients[-1,4]}
backwardlm=function(lm,criticalalpha) 	{
lm2=lm
while(max(pvalueslm(lm2))>criticalalpha) 	{
lm2=update(lm2,paste(".~.-",attr(lm2$terms,
	"term.labels")[(which(pvalueslm(lm2)==max(pvalueslm(lm2))))],sep=""))
						}
return(lm2)
					}
gasoline_lm_backward = backwardlm(gasoline_lm,criticalalpha=0.20)
summary(gasoline_lm_backward)

# The Forward Selection Methodology
forwardlm=function(y,x,criticalalpha) 	{
yx = data.frame(y=Gasoline$y,Gasoline[,-1])
mylm = lm(y~-.,data=yx)
avail_cov =  attr(mylm$terms,"dataClasses")[-1]
minpvalues=0
while(minpvalues<criticalalpha)	{
pvalues_curr = NULL
for(i in 1:length(avail_cov))	{
	templm = update(mylm,paste(".~.+",names(avail_cov[i])))
	mypvalues = summary(templm)$coefficients[,4]
	pvalues_curr = c(pvalues_curr,mypvalues[length(mypvalues)])
				}
minpvalues = min(pvalues_curr)
if(minpvalues<criticalalpha)	{
include_me_in = min(which(pvalues_curr<criticalalpha))
mylm = update(mylm,paste(".~.+",names(avail_cov[include_me_in])))
avail_cov = avail_cov[-include_me_in]
				}
				}
return(mylm)
					}
gasoline_lm_forward = forwardlm(Gasoline$y,Gasoline[,-1],criticalalpha=0.2)
summary(gasoline_lm_forward)

step(gasoline_lm)

