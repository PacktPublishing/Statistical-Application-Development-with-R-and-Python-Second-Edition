install.packages(c("qcc","ggplot2"))
library(RSADBE)

# Time for action – bar charts in R
example(barplot)
library(lattice)
example(barchart) 
data(Severity_Counts)
barchart(Severity_Counts,xlab="Bug Count",xlim=c(0,12000), col=rep(c(2,3),5))
barplot(Severity_Counts,xlab="Bug Count",xlim=c(0,12000), horiz =TRUE,col=rep(c(2,3),5))
data(Bug_Metrics_Software)
par(mfrow=c(1,2))
barplot(Bug_Metrics_Software[,,1],beside=TRUE,col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"),legend = c("JDT","PDE","Equinox","Lucene", "Mylyn")) title(main = "Before Release Bug Frequency", font.main = 4)
barplot(Bug_Metrics_Software[,,2],beside=TRUE,col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"),legend = c("JDT","PDE","Equinox","Lucene", "Mylyn")) title(main = "After Release Bug Frequency",font.main = 4)
barchart(Software~Freq|Bugs,groups=BA_Ind, data= as.data.frame(Bug_Metrics_Software),col=c(2,3))

# Time for action – dot charts in R
example(dotplot); example(dotchart);
dotchart(Severity_Counts,col=15:16,lcolor="black",pch=2:3,labels=names(Severity_Counts),main="Dot Plot for the Before and After Release Bug Frequency",cex=1.5)
par(mfrow=c(1,2))
dotchart(Bug_Metrics_Software[,,1],gcolor=1:5,col=6:10,lcolor = "black",pch=15:19,labels=names(Bug_Metrics_Software[,,1]), main="Before Release Bug Frequency",xlab="Frequency Count")
dotchart(Bug_Metrics_Software[,,2],gcolor=1:5,col=6:10,lcolor = "black",pch=15:19,labels=names(Bug_Metrics_Software[,,2]), main="After Release Bug Frequency",xlab="Frequency Count")
 
# Time for action – spine plot for the shift and operator data
example(spineplot)
ShiftOperator <- matrix(c(40, 35, 28, 26, 40, 22, 52, 46, 49),nrow=3,dimnames=list(c("Shift 1", "Shift 2", "Shift 3"), c("Operator 1", "Opereator 2", "Operator 3")),byrow=TRUE)
colSums(ShiftOperator)
rowSums(ShiftOperator)
spineplot(ShiftOperator)
abline(h=0.33,lwd=3,col="red")
abline(h=0.67,lwd=3,col="red")
abline(v=0.33,lwd=3,col="green")
abline(v=0.67,lwd=3,col="green")

# Time for action – mosaic plot for the Titanic dataset
xtabs(Freq~Class,data=Titanic)
prop.table( xtabs(Freq~Class+Survived,data=Titanic),margin=1)
xtabs(Freq~Sex,data=Titanic)
prop.table(xtabs(Freq~Sex+Survived,data=Titanic),margin=1)
xtabs(Freq~Age,data=Titanic) 
prop.table(xtabs(Freq~Age+Survived,data=Titanic), margin=1)
mosaicplot(Titanic,col=c("red","green"))

# Pie chart and the fourfold plot
pie(Severity_Counts[1:5])
title("Severity Counts Post-Release of JDT Software")
pie(Severity_Counts[6:10])
title("Severity Counts Pre-Release of JDT Software")
fourfoldplot(UCBAdmissions,mfrow=c(2,3),space=0.4)

# Time for action – using the boxplot
example(boxplot)
example(bwplot) 
data(resistivity)
summary(resistivity)
boxplot(resistivity, range=0)
resistivity2 <- data.frame(rep(names( resistivity),each=8),c(resistivity[,1],resistivity[,2]))
names(resistivity2)<- c("Process","Resistivity")
bwplot(Resistivity~Process, data=resistivity2,notch=TRUE)
boxplot(Speed~Expt,data=morley,main = "Whiskers at Lower- and Upper- Confidence Limits")
abline(h=792.458,lty=3)

# Time for action – understanding the effectiveness of histogram
example(hist) 
example(histogram) 
data(galton)
par(mfrow=c(2,2)) 
hist(galton$parent,breaks="FD",xlab="Height of Parent", main="Histogram for Parent Height with Freedman-Diaconis Breaks",xlim=c(60,75))
hist(galton$parent,xlab="Height of Parent",main= "Histogram for Parent Height with Sturges Breaks",xlim=c(60,75))
hist(galton$child,breaks="FD",xlab="Height of Child", main="Histogram for Child Height with Freedman-Diaconis Breaks",xlim=c(60,75))
hist(galton$child,xlab="Height of Child",main="Histogram for Child Height with Sturges Breaks",xlim=c(60,75))
data(octane)
par(mfrow=c(2,2))
hist(octane$Method_1,xlab="Ratings Under Method I",main="Histogram of Octane Ratings for Method I",col="mistyrose") 
hist(octane$Method_2,xlab="Ratings Under Method II",main="Histogram of Octane Ratings for Method II",col=" cornsilk")
data(Samplez)
hist(Samplez$Sample_1,xlab="Sample 1",main="Histogram: Sample 1",col="magenta")
hist(Samplez$Sample_2,xlab="Sample 2",main="Histogram: Sample 2",col="snow")

# Time for action – plot and pairs R functions
data(DCD)
plot(DCD$Drain_Current, DCD$GTS_Voltage,type="b",xlim=c(1,2.2),ylim=c(0.6,2.4),xlab="Current Drain", ylab="Voltage")
points(DCD$Drain_Current,DCD$GTS_Voltage/1.15,type="b",col="green")
panel.hist <- function(x, ...)	{ 
	usr<- par("usr"); on.exit(par(usr)) 
	par(usr = c(usr[1:2], 0, 1.5) ) 
	h <- hist(x, plot = FALSE) 
	breaks<- h$breaks; nB<- length(breaks) 
	y <- h$counts; 
	y <- y/max(y) 
	rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
				} 
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)	{ 
	usr<- par("usr"); on.exit(par(usr)) 
	par(usr = c(0, 1, 0, 1)) 
	r <- abs(cor(x,y,use="complete.obs")) 
	txt<- format(c(r, 0.123456789), digits=digits)[1] 
	txt<- paste(prefix, txt, sep="") 
	if(missing(cex.cor)) cex.cor<- 0.8/strwidth(txt) 
	text(0.5, 0.5, txt, cex = cex.cor * r) 
								}
data(Gasoline)
pairs(Gasoline,diag.panel=panel.hist,lower.panel=panel.smooth,upper.panel=panel.cor)
example(plot)
example(pairs) 
example(xyplot)

# Pareto Chart
Cause_Freq <- c(5, 23, 7, 41, 19, 4, 3, 4, 2, 1)
names(Cause_Freq) <- paste("C",1:10,sep="")
Cause_Freq_Dec <- sort(Cause_Freq,dec=TRUE)
Cause_Freq_Cumsum <- cumsum(Cause_Freq_Dec)
Cause_Freq_Cumsum_Perc <- Cause_Freq_Cumsum/sum(Cause_Freq)
cbind(Cause_Freq_Dec,Cause_Freq_Cumsum,Cause_Freq_Cumsum_Perc)
library(qcc)
Reject_Freq = c(9,22,15,40,8)
names(Reject_Freq) = c("No Addr.", "Illegible", "Curr. Customer", "No Sign.", "Other")
Reject_Freq
options(digits=2)
pareto.chart(Reject_Freq)

# Time for action – qplot
library(ggplot2)
test <- data.frame(rep(c("R1","R2"),each=8),c(resistivity[,1], resistivity[,2]))
names(test) <- c("RES","VALUE")
qplot(factor(RES),VALUE,data=test,geom="boxplot")
qplot(factor(x11),y,data=Gasoline, geom= "boxplot")
qplot(child,data=galton,geom="histogram", binwidth = 2,xlim=c(60,75),xlab="Height of Child", ylab="Frequency")
qplot(parent,child,data=galton,xlab="Height of Parent", ylab="Height of Child", main="Height of Parent Vs Child")

# Time for action – ggplot
library(ggplot2)
galton_gg <- ggplot(galton,aes(child,parent)) 
galton_gg <- galton_gg + layer(geom="point")
galton_gg <- galton_gg + xlim(60,75)
galton_gg
galton_gg <- galton_gg + ylim(60,75)
galton_gg
galton_gg<-galton_gg+ylab("Height of Parent")+xlab("Height ofChild")
galton_gg
galton_gg <- galton_gg + ggtitle("Height of Parent Vs Child")
galton_gg

