install.packages("aplpack")
library(RSADBE)
install.packages("tcltk")
library(aplpack)
install.packages("vcd")
library(vcd)

# Time for action – the essential summary statistics for "The Wall" dataset
library(LearnEDA)
data(TheWALL)
quantile(TheWALL$Score)
diff(quantile(TheWALL$Score))
quantile(TheWALL$Score,seq(0,1,.1))
diff(quantile(TheWALL$Score,seq(0,1,.1)))
table(HA_Ind)
boxplot(Score~HA_Ind,data=TheWALL)
boxplot(Score~HA_Ind,subset=(Score<200),data=TheWALL)
fivenum(TheWALL$Score)
range(TheWALL$Score)
diff(range(TheWALL$Score))
IQR(TheWALL$Score)
IQR(TheWALL$Score[TheWALL$HA_Ind=="Away"]) 
IQR(TheWALL$Score[TheWALL$HA_Ind=="Home"])

# Time for action – the stem function in play
x <- c(12,22,42, 13,27,46,25,52)
stem(x)
data(octane)
stem(octane$Method_1, scale=2)
stem(octane$Method_2, scale=2)
library(aplpack)
stem.leaf.backback(octane$Method_1, octane$Method_2,back.to.back=FALSE, m=5)

# Letter Values
lval(octane$Method_1)
lval(octane$Method_2)

# Data re-expression
hydroelectric <- c(14,18,28,26,36,30,30,34,30,43,45,54,52,60,68, + 68,61,75,76,70,76,86,90,96,100,100,100,100,100,100,110,112,118,110,124,130,135,135,130,175,165,140,250,280,204,200,270,
40,320,330,468,400,518,540,595,600,810,810,1728,1400,1743,2700)
stem.leaf(hydroelectric,unit=1)
max(hydroelectric)/min(hydroelectric)
stem.leaf(round(log(hydroelectric,10),2),unit=0.01)

# Time for action – the bagplot display for multivariate dataset
example(bagplot)
panel.bagplot <- function(x,y)	{ 
	require(aplpack) 
	bagplot(x,y,verbose=FALSE,create.plot = TRUE,add=TRUE) 
				}
pairs(Gasoline[-19,-c(1,4,5,13)],upper.panel=panel.bagplot)

# Time for action – resistant line as a first regression model
example(rline)
data(IO_Time)
IO_rline <- rline(IO_Time$No_of_IO, IO_Time$CPU_Time,iter=10) 
IO_rline$a 
IO_rline$b
plot(IO_Time$No_of_IO, IO_Time$CPU_Time)
abline(a= IO_rline$a,b=IO_rline$b)
title("Resistant Line for the IO-CPU Time")

# Time for action – smoothening the cow temperature data
data(CT)
plot.ts(CT$Temperature,col="red",pch=1)
CT_3RSS <- smooth(CT$Temperature,kind="3RSS")
CT_3RSSH <- han(smooth(CT$Temperature,kind="3RSS"))
lines.ts(CT_3RSS,col="blue",pch=2)
lines.ts(CT_3RSSH,col="green",pch=3)
legend(20,90,c("Original","3RSS","3RSSH"),col=c("red","blue","green"),pch="___")

# Time for action – the median polish algorithm
data(MDR)
MDR2 <- as.matrix(MDR[,2:5])
rownames(MDR2) <- c("Lung", "UR","Sto","CaR","Prost","Other_Lung", "Pul_TB","CB","RD_Other", "CT","Other_Cardio","CH","PU","Viol", "Other_Dis")
MDR_medpol <- medpolish(MDR2)
MDR_medpol$row
MDR_medpol$col
MDR_medpol$overall
MDR_medpol$residuals



