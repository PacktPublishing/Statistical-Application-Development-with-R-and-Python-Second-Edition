install.packages(c("rpart","rattle"))
library(RSADBE)

# Recursive Partitions
# Time for Action: Partitioning the Display Plot
data(CART_Dummy)
CART_Dummy$Y <- as.factor(CART_Dummy$Y)
attach(CART_Dummy)
par(mfrow=c(1,2))
plot(c(0,12),c(0,10),type="n",xlab="X1",ylab="X2")
points(X1[Y==0],X2[Y==0],pch=15,col="red")
points(X1[Y==1],X2[Y==1],pch=19,col="green")
title(main="A Difficult Classification Problem")
plot(c(0,12),c(0,10),type="n",xlab="X1",ylab="X2")
points(X1[Y==0],X2[Y==0],pch=15,col="red")
points(X1[Y==1],X2[Y==1],pch=19,col="green")
segments(x0=c(0,0,6,6),y0=c(3.75,6.25,2.25,5),x1=c(6,6,12,12),y1=c(3.75,6.25,2.25,5),lwd=2)
abline(v=6,lwd=2)
title(main="Looks a Solvable Problem Under Partitions")

# Time for Action: Building Our First Tree
library(rpart)
CART_Dummy_rpart <- rpart(Y~X1+X2,data=CART_Dummy)
plot(CART_Dummy_rpart); text(CART_Dummy_rpart)
summary(CART_Dummy_rpart)
library(rattle)
asRules(CART_Dummy_rpart)

# Repeated for regeneration
plot(c(0,12),c(0,10),type="n",xlab="X1",ylab="X2")
points(X1[Y==0],X2[Y==0],pch=15,col="red")
points(X1[Y==1],X2[Y==1],pch=19,col="green")
# Repetition ends here
abline(h=4.875,lwd=2)
segments(x0=4.5,y0=4.875,x1=4.5,y1=10,lwd=2)
abline(h=1.75,lwd=2)
segments(x0=3.5,y0=1.75,x1=3.5,y1=4.875,lwd=2)
title(main="Classification Tree on the Data Display")


# Time for Action: The Construction of a Regression Tree
library(MASS)
cpus.ltrpart <- rpart(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
cpus.ltrpart
summary(cpus.ltrpart)
plot(cpus.ltrpart);  text(cpus.ltrpart)

getNode <- function(x,y)	{
		xu <- sort(unique(x),decreasing=TRUE)
		ss <- numeric(length(xu)-1)
		for(i in 1:length(ss))	{
			partR <- y[x>xu[i]]
			partL <- y[x<=xu[i]]
			partRSS <- sum((partR-mean(partR))^2)
			partLSS <- sum((partL-mean(partL))^2)
			ss[i]=partRSS + partLSS
					}
		return(list(xnode=xu[which(ss==min(ss,na.rm=T))], minss <- min(ss,na.rm=T),ss,xu))
			}

getNode(cpus$syct,log10(cpus$perf))[[2]]
getNode(cpus$mmin,log10(cpus$perf))[[2]]
getNode(cpus$mmax,log10(cpus$perf))[[2]]
getNode(cpus$cach,log10(cpus$perf))[[2]]
getNode(cpus$chmin,log10(cpus$perf))[[2]]
getNode(cpus$chmax,log10(cpus$perf))[[2]]
getNode(cpus$cach,log10(cpus$perf))[[1]]
sort(getNode(cpus$cach,log10(cpus$perf))[[4]],
decreasing=FALSE)

cpus_FS_R <- cpus[cpus$cach>=27,]
cpus_FS_L <- cpus[cpus$cach<27,]

getNode(cpus_FS_R$syct,log10(cpus_FS_R$perf))[[2]]
getNode(cpus_FS_R$mmin,log10(cpus_FS_R$perf))[[2]]
getNode(cpus_FS_R$mmax,log10(cpus_FS_R$perf))[[2]]
getNode(cpus_FS_R$cach,log10(cpus_FS_R$perf))[[2]]
getNode(cpus_FS_R$chmin,log10(cpus_FS_R$perf))[[2]]
getNode(cpus_FS_R$chmax,log10(cpus_FS_R$perf))[[2]]
getNode(cpus_FS_R$mmax,log10(cpus_FS_R$perf))[[1]]
sort(getNode(cpus_FS_R$mmax,log10(cpus_FS_R$perf))[[4]],
decreasing=FALSE)
getNode(cpus_FS_L$syct,log10(cpus_FS_L$perf))[[2]]
getNode(cpus_FS_L$mmin,log10(cpus_FS_L$perf))[[2]]
getNode(cpus_FS_L$mmax,log10(cpus_FS_L$perf))[[2]]
getNode(cpus_FS_L$cach,log10(cpus_FS_L$perf))[[2]]
getNode(cpus_FS_L$chmin,log10(cpus_FS_L$perf))[[2]]
getNode(cpus_FS_L$chmax,log10(cpus_FS_L$perf))[[2]]
getNode(cpus_FS_L$mmax,log10(cpus_FS_L$perf))[[1]]
sort(getNode(cpus_FS_L$mmax,log10(cpus_FS_L$perf))[[4]],
decreasing=FALSE)

cpus_FS_R_SS_R <- cpus_FS_R[cpus_FS_R$mmax>=28000,]
cpus_FS_R_SS_L <- cpus_FS_R[cpus_FS_R$mmax<28000,]
getNode(cpus_FS_R_SS_R$syct,log10(cpus_FS_R_SS_R$perf))[[2]]
getNode(cpus_FS_R_SS_R$mmin,log10(cpus_FS_R_SS_R$perf))[[2]]
getNode(cpus_FS_R_SS_R$mmax,log10(cpus_FS_R_SS_R$perf))[[2]]
getNode(cpus_FS_R_SS_R$cach,log10(cpus_FS_R_SS_R$perf))[[2]]
getNode(cpus_FS_R_SS_R$chmin,log10(cpus_FS_R_SS_R$perf))[[2]]
getNode(cpus_FS_R_SS_R$chmax,log10(cpus_FS_R_SS_R$perf))[[2]]
getNode(cpus_FS_R_SS_R$cach,log10(cpus_FS_R_SS_R$perf))[[1]]
sort(getNode(cpus_FS_R_SS_R$cach,log10(cpus_FS_R_SS_R$perf))[[4]],
decreasing=FALSE)

getNode(cpus_FS_R_SS_L$syct,log10(cpus_FS_R_SS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L$mmin,log10(cpus_FS_R_SS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L$mmax,log10(cpus_FS_R_SS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L$cach,log10(cpus_FS_R_SS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L$chmin,log10(cpus_FS_R_SS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L$chmax,log10(cpus_FS_R_SS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L$cach,log10(cpus_FS_R_SS_L$perf))[[1]]
sort(getNode(cpus_FS_R_SS_L$cach,log10(cpus_FS_R_SS_L$perf))[[4]],
decreasing=FALSE)
# Working with first splits right sides left split
cpus_FS_R_SS_L_TS_L <- cpus_FS_R_SS_L[cpus_FS_R_SS_L$cach<96.5,]
getNode(cpus_FS_R_SS_L_TS_L$syct,log10(cpus_FS_R_SS_L_TS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L_TS_L$mmin,log10(cpus_FS_R_SS_L_TS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L_TS_L$mmax,log10(cpus_FS_R_SS_L_TS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L_TS_L$cach,log10(cpus_FS_R_SS_L_TS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L_TS_L$chmin,log10(cpus_FS_R_SS_L_TS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L_TS_L$chmax,log10(cpus_FS_R_SS_L_TS_L$perf))[[2]]
getNode(cpus_FS_R_SS_L_TS_L$mmax,log10(cpus_FS_R_SS_L_TS_L$perf))[[1]]
sort(getNode(cpus_FS_R_SS_L_TS_L$mmax,log10(cpus_FS_R_SS_L_TS_L$perf))[[4]],
decreasing=FALSE)


# Working the first split left side
cpus_FS_L_SS_R <- cpus_FS_L[cpus_FS_L$mmax>=6100,]
cpus_FS_L_SS_L <- cpus_FS_L[cpus_FS_L$mmax<6100,]
getNode(cpus_FS_L_SS_R$syct,log10(cpus_FS_L_SS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R$mmin,log10(cpus_FS_L_SS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R$mmax,log10(cpus_FS_L_SS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R$cach,log10(cpus_FS_L_SS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R$chmin,log10(cpus_FS_L_SS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R$chmax,log10(cpus_FS_L_SS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R$syct,log10(cpus_FS_L_SS_R$perf))[[1]]
sort(getNode(cpus_FS_L_SS_R$syct,log10(cpus_FS_L_SS_R$perf))[[4]],
decreasing=FALSE)
getNode(cpus_FS_L_SS_L$syct,log10(cpus_FS_L_SS_L$perf))[[2]]
getNode(cpus_FS_L_SS_L$mmin,log10(cpus_FS_L_SS_L$perf))[[2]]
getNode(cpus_FS_L_SS_L$mmax,log10(cpus_FS_L_SS_L$perf))[[2]]
getNode(cpus_FS_L_SS_L$cach,log10(cpus_FS_L_SS_L$perf))[[2]]
getNode(cpus_FS_L_SS_L$chmin,log10(cpus_FS_L_SS_L$perf))[[2]]
getNode(cpus_FS_L_SS_L$chmax,log10(cpus_FS_L_SS_L$perf))[[2]]
getNode(cpus_FS_L_SS_L$mmax,log10(cpus_FS_L_SS_L$perf))[[1]]
sort(getNode(cpus_FS_L_SS_L$mmax,log10(cpus_FS_L_SS_L$perf))[[4]],
decreasing=FALSE)
cpus_FS_L_SS_R_TS_R <- cpus_FS_L_SS_R[cpus_FS_L_SS_R$syct<360,]
getNode(cpus_FS_L_SS_R_TS_R$syct,log10(cpus_FS_L_SS_R_TS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R_TS_R$mmin,log10(cpus_FS_L_SS_R_TS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R_TS_R$mmax,log10(cpus_FS_L_SS_R_TS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R_TS_R$cach,log10(cpus_FS_L_SS_R_TS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R_TS_R$chmin,log10(cpus_FS_L_SS_R_TS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R_TS_R$chmax,log10(cpus_FS_L_SS_R_TS_R$perf))[[2]]
getNode(cpus_FS_L_SS_R_TS_R$chmin,log10(cpus_FS_L_SS_R_TS_R$perf))[[1]]
sort(getNode(cpus_FS_L_SS_R_TS_R$chmin,log10(cpus_FS_L_SS_R_TS_R$perf))[[4]],
decreasing=FALSE)



# The Construction of a Classification Tree
p <- seq(0.01,0.99,0.01)
plot(p,pmin(p,1-p),"l",col="red",xlab="p",xlim=c(0,1),ylim=c(0,1), ylab="Impurity Measures")
points(p,-p*log(p)-(1-p)*log(1-p),"l",col="green")
points(p,p*(1-p),"l",col="blue")
title(main="Impurity Measures")
legend(0.6,1,c("Bayes Error","Cross-Entropy","Gini Index"),col=c("red","green","blue"),pch="-")

# Time for Action: The Construction of a Classification Tree
ky_rpart <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,parms=list(split="information"))
plot(ky_rpart); text(ky_rpart)
asRules(ky_rpart)

getNode <- function(x,y)	{
	xu <- sort(unique(x),decreasing=FALSE)
	delta_ISA <- numeric(length(xu)-1)
	for(i in 1:(length(xu)-1))	{
		partR <- y[x>xu[i]]
		partL <- y[x<=xu[i]]
		if((length(partR)>0) & (length(partL)>0))	{
		tt <-  table(x>xu[i],y)
		IA_L <- -(tt[1,1]/(tt[1,1]+tt[1,2]))*log(tt[1,1]/(tt[1,1]+tt[1,2]))
			-(tt[1,2]/(tt[1,1]+tt[1,2]))*log(tt[1,2]/(tt[1,1]+tt[1,2]))
		IA_R <- -(tt[2,1]/(tt[2,1]+tt[2,2]))*log(tt[2,1]/(tt[2,1]+tt[2,2]))
			-(tt[2,2]/(tt[2,1]+tt[2,2]))*log(tt[2,2]/(tt[2,1]+tt[2,2]))
		pA_L <- (tt[1,1]+tt[1,2])/sum(tt)
		pA_R <- (tt[2,1]+tt[2,2])/sum(tt)
		pA <- mean(y)
		IA <- -pA*log(pA)-(1-pA)*log(1-pA)
		delta_ISA[i] <- IA -pA_L*IA_L - pA_R*IA_R
								}
		else delta_ISA[i]=0
					}
		return(list(xnode=xu[which(delta_ISA==max(delta_ISA,na.rm=T))], 
		max_delta_ISA <- max(delta_ISA,na.rm=T), delta_ISA,xu))
			}

KYPHOSIS <- kyphosis
KYPHOSIS$Kyphosis_y <- (kyphosis$Kyphosis=="absent")*1
getNode(KYPHOSIS$Age,KYPHOSIS$Kyphosis_y)[[2]]
getNode(KYPHOSIS$Number,KYPHOSIS$Kyphosis_y)[[2]]
getNode(KYPHOSIS$Start,KYPHOSIS$Kyphosis_y)[[2]]
getNode(KYPHOSIS$Start,KYPHOSIS$Kyphosis_y)[[1]]
sort(getNode(KYPHOSIS$Start,KYPHOSIS$Kyphosis_y)[[4]],
decreasing=FALSE)
KYPHOSIS_FS_R <- KYPHOSIS[KYPHOSIS$Start<12.5,]
KYPHOSIS_FS_L <- KYPHOSIS[KYPHOSIS$Start>=12.5,]
getNode(KYPHOSIS_FS_R$Age,KYPHOSIS_FS_R$Kyphosis_y)[[2]]
getNode(KYPHOSIS_FS_R$Number,KYPHOSIS_FS_R$Kyphosis_y)[[2]]
getNode(KYPHOSIS_FS_R$Start,KYPHOSIS_FS_R$Kyphosis_y)[[2]]
getNode(KYPHOSIS_FS_R$Age,KYPHOSIS_FS_R$Kyphosis_y)[[1]]
sort(getNode(KYPHOSIS_FS_R$Age,KYPHOSIS_FS_R$Kyphosis_y)[[4]],
decreasing=FALSE)
KYPHOSIS_FS_R_SS_R <- KYPHOSIS_FS_R[KYPHOSIS_FS_R$Age>=34.5,]
KYPHOSIS_FS_R_SS_L <- KYPHOSIS_FS_R[KYPHOSIS_FS_R$Age<34.5,]
getNode(KYPHOSIS_FS_R_SS_R$Age,KYPHOSIS_FS_R_SS_R$Kyphosis_y)[[2]]
getNode(KYPHOSIS_FS_R_SS_R$Number,KYPHOSIS_FS_R_SS_R$Kyphosis_y)[[2]]
getNode(KYPHOSIS_FS_R_SS_R$Start,KYPHOSIS_FS_R_SS_R$Kyphosis_y)[[2]]
getNode(KYPHOSIS_FS_R_SS_R$Number,KYPHOSIS_FS_R_SS_R$Kyphosis_y)[[1]]
sort(getNode(KYPHOSIS_FS_R_SS_R$Number,KYPHOSIS_FS_R_SS_R$Kyphosis_y)[[4]],
decreasing=FALSE)



# Classification Tree for the German Credit Data
# Time for Action: The Construction of a Classification Tree
set.seed(1234567)
data_part_label <- c("Train","Validate","Test")
indv_label <- sample(data_part_label,size=1000,replace=TRUE,prob=c(0.6,0.2,0.2))
library(ROCR)
data(GC)
GC_Train <- GC[indv_label=="Train",]
GC_Validate <- GC[indv_label=="Validate",]
GC_Test <- GC[indv_label=="Test",]

GC_rpart <- rpart(good_bad~.,data=GC_Train)
plot(GC_rpart); text(GC_rpart)
asRules(GC_rpart)

Pred_Train_Class <- predict(GC_rpart,type='class')
Pred_Train_Prob <-predict(GC_rpart,type='prob')
Train_Pred <- prediction(Pred_Train_Prob[,2],GC_Train$good_bad)
Perf_Train <- performance(Train_Pred,"tpr","fpr")
plot(Perf_Train,col="green",lty=2)
Pred_Validate_Class <- predict(GC_rpart,newdata <- GC_Validate[,-21],type='class')
Pred_Validate_Prob <-predict(GC_rpart,newdata <- GC_Validate[,-21],type='prob')
Validate_Pred <- prediction(Pred_Validate_Prob[,2],GC_Validate$good_bad)
Perf_Validate <- performance(Validate_Pred,"tpr","fpr")
plot(Perf_Validate,col="yellow",lty=2,add=TRUE)
Pred_Test_Class <- predict(GC_rpart,newdata <- GC_Test[,-21],type='class')
Pred_Test_Prob <-predict(GC_rpart,newdata <- GC_Test[,-21],type='prob')
Test_Pred <- prediction(Pred_Test_Prob[,2],GC_Test$good_bad)
Perf_Test <- performance(Test_Pred,"tpr","fpr")
plot(Perf_Test,col="red",lty=2,add=TRUE)
legend(0.6,0.5,c("Train Curve","Validate Curve","Test Curve"),col=c("green","yellow","red"),pch="-")

# Prune Tree
# Time for Action: Pruning a Classification Tree
par(mfrow=c(1,2))
GC_rpart_minsplit <- rpart(good_bad~.,data=GC_Train, minsplit=30)
GC_rpart_minsplit <- prune(GC_rpart,cp=0.05)
Pred_Train_Class <- predict(GC_rpart_minsplit,type='class')
Pred_Train_Prob <-predict(GC_rpart_minsplit,type='prob')
Train_Pred <- prediction(Pred_Train_Prob[,2],GC_Train$good_bad)
Perf_Train <- performance(Train_Pred,"tpr","fpr")
plot(Perf_Train,col="green",lty=2)
Pred_Validate_Class <- predict(GC_rpart_minsplit,newdata <- GC_Validate[,-21],type='class')
Pred_Validate_Prob <-predict(GC_rpart_minsplit,newdata <- GC_Validate[,-21],type='prob')
Validate_Pred <- prediction(Pred_Validate_Prob[,2],GC_Validate$good_bad)
Perf_Validate <- performance(Validate_Pred,"tpr","fpr")
plot(Perf_Validate,col="yellow",lty=2,add=TRUE)
Pred_Test_Class <- predict(GC_rpart_minsplit,newdata <- GC_Test[,-21],type='class')
Pred_Test_Prob <-predict(GC_rpart_minsplit,newdata <- GC_Test[,-21],type='prob')
Test_Pred <- prediction(Pred_Test_Prob[,2],GC_Test$good_bad)
Perf_Test <- performance(Test_Pred,"tpr","fpr")
plot(Perf_Test,col="red",lty=2,add=TRUE)
legend(0.6,0.5,c("Train Curve","Validate Curve","Test Curve"),col=c("green","yellow","red"),pch="-")
title(main="Improving a Classification Tree with minsplit")

GC_rpart_prune <- prune(GC_rpart,cp=0.02)
Pred_Train_Class <- predict(GC_rpart_prune,type='class')
Pred_Train_Prob <-predict(GC_rpart_prune,type='prob')
Train_Pred <- prediction(Pred_Train_Prob[,2],GC_Train$good_bad)
Perf_Train <- performance(Train_Pred,"tpr","fpr")
plot(Perf_Train,col="green",lty=2)
Pred_Validate_Class <- predict(GC_rpart_prune,newdata <- GC_Validate[,-21],type='class')
Pred_Validate_Prob <-predict(GC_rpart_prune,newdata <- GC_Validate[,-21],type='prob')
Validate_Pred <- prediction(Pred_Validate_Prob[,2],GC_Validate$good_bad)
Perf_Validate <- performance(Validate_Pred,"tpr","fpr")
plot(Perf_Validate,col="yellow",lty=2,add=TRUE)
Pred_Test_Class <- predict(GC_rpart_prune,newdata <- GC_Test[,-21],type='class')
Pred_Test_Prob <-predict(GC_rpart_prune,newdata <- GC_Test[,-21],type='prob')
Test_Pred <- prediction(Pred_Test_Prob[,2],GC_Test$good_bad)
Perf_Test <- performance(Test_Pred,"tpr","fpr")
plot(Perf_Test,col="red",lty=2,add=TRUE)
legend(0.6,0.5,c("Train Curve","Validate Curve","Test Curve"),col=c("green","yellow","red"),pch="-")
title(main="Improving a Classification Tree with Pruning")


