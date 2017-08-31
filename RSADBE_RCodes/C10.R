setwd("C:/Users/prabhanjan_tattar/Documents/Prabhanjan/Packt/RLAB")

# Time for Action: Cross-Validation Predictions
GC_Complete <- rpart(good_bad~.,data=GC)
GC_Complete$cptable
GC_CV_Pred = xpred.rpart(GC_Complete)
sum(diag(table(GC_CV_Pred[,2],GC$good_bad)))/1000
sum(diag(table(GC_CV_Pred[,3],GC$good_bad)))/1000
sum(diag(table(GC_CV_Pred[,4],GC$good_bad)))/1000
sum(diag(table(GC_CV_Pred[,5],GC$good_bad)))/1000
sum(diag(table(GC_CV_Pred[,6],GC$good_bad)))/1000

# BAGGING 

# Time for Action: Understanding the Bootstrap Technique 
library(boot)
OR <- function(data,i)	{
	x <- data[,1]; y <- data[,2]
	odds.ratio <- (sum(x[i]==1,na.rm=T)/length(na.omit(x[i])))/(sum(y[i]==1,na.rm=T)/length(na.omit(y[i])))
	return(odds.ratio)
			}
aspirin_hattack <- c(rep(1,104),rep(0,11037-104))
placebo_hattack <- c(rep(1,189),rep(0,11034-189))
aspirin_strokes <- c(rep(1,119),rep(0,11037-119))
placebo_strokes <- c(rep(1,98),rep(0,11034-98))
hattack <- cbind(aspirin_hattack,c(placebo_hattack,NA,NA,NA))
hattack_boot <- boot(data=hattack,statistic=OR,R=1000)
strokes <- cbind(aspirin_strokes,c(placebo_strokes,NA,NA,NA))
strokes_boot <- boot(data=strokes,statistic=OR,R=1000)
quantile(hattack_boot$t,c(0.025,0.975))
quantile(strokes_boot$t,c(0.025,0.975))


# Time for Action: The Bagging Algorithm 
data(GC)
library(ipred)
GC_bagging <- bagging(good_bad~.,data=GC,coob=FALSE,nbagg=200,keepX=T)
for(i in 1:200) {
	plot(GC_bagging$mtrees[[i]]$btree); 
	text(GC_bagging$mtrees[[i]]$btree,pretty=1,use.n=T)
		}
GCB_Margin <- round(predict(GC_bagging,type="prob")*200,0)
head(GCB_Margin)
mean(pmax(GCB_Margin[,1],GCB_Margin[,2])-pmin(GCB_Margin[,1],GCB_Margin[,2]))/200
GC_bagging_oob <- bagging(good_bad~.,data=GC,coob=TRUE,nbagg=200,keepX=T)


# Random Forests
library(randomForest)
GC_RF <- randomForest(good_bad~.,data=GC,keep.forest=TRUE,ntree=500)

to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1)	{
	if(dfrep[rownum,'status'] == -1)	{
		rval <- list()
		attr(rval,"members") <- 1
		attr(rval,"height") <- 0.0
		attr(rval,"label") <- dfrep[rownum,'prediction']
		attr(rval,"leaf") <- TRUE
						}
	else	{
		left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)		
		right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
		rval <- list(left,right)
		attr(rval,"members") <- attr(left,"members") + attr(right,"members")
		attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
		attr(rval,"leaf") <- FALSE
		attr(rval,"edgetext") <- dfrep[rownum,'split var']
		}
  class(rval) <- "dendrogram"
  return(rval)
								}

for(i in 1:20)	{
	tree <- getTree(GC_RF,i,labelVar=T)
	d <- to.dendrogram(tree)	
	plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=1,p.col=NA,p.lty=0))
		}

plot(1:500,GC_RF$err.rate[,1],"l",xlab="No.of.Trees",ylab="OOB Error Rate")
varImpPlot(GC_RF)


# THE CONSOLIDATION
lowbwt = read.csv("lowbwt.csv",header=TRUE, row.names=1)
pairs(lowbwt,diag.panel=panel.hist,lower.panel=panel.smooth,upper.panel=panel.cor)
LOW <- lowbwt[,-10]
BWT <- lowbwt[,-1]
BWT_lm <- lm(BWT~., data=BWT)
summary(BWT_lm)
LOW_glm <- glm(LOW~.,data=LOW)
summary(LOW_glm)
hosmerlem(LOW_glm$y,fitted(LOW_glm))
LOW_rpart<- rpart(LOW~.,data=LOW)
plot(LOW_rpart)
text(LOW_rpart,pretty=1
asRules(LOW_rpart)
LOW_bagging<- bagging(LOW~., data=LOW,coob=TRUE,nbagg=50,keepX=T)
LOW_bagging$err
LOW_RF <- randomForest(LOW~.,data=LOW,keep.forest=TRUE,ntree=50)
LOW_RF$err.rate
LOW_RF <- randomForest(LOW~.,data=LOW,keep.forest=TRUE,ntree=150)
plot(1:150,LOW_RF$err.rate[,1],"l",xlab="No.of.Trees",ylab="OOB Error Rate")







