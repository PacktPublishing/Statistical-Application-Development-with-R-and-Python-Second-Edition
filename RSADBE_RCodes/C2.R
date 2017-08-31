install.packages(c("foreign","RMySQL"))
library(RSADBE)

# Time for Action: Understanding the Constants, Vectors, and Basic Arithmetic
LETTERS
letters 
LETTERS[c(1:5,22:26)]
letters[c(1:5,22:26)]
?Constants
month.abb[c(1:3,8:10)]
month.name[c(1:3,8:10)]
pi 

vector(length=4) 
vector(mode="numeric",length=4) 
vector(mode="logical",length=4)  
vector(mode="complex",length=4) 
vector(mode="character",length=4) 
vector(mode="integer",length=4) 

x <- 1:10
names(x) 
names(x) <- letters[1:10]
names(x)
x
x <- 1:10
y <- 11:20
a <- 10
b <- -4
x + y
a*x + b*y
sum((a+b)*x == a*x + b*x)*x*y
round(x/y,4)
x^2 

x <- 1:10
x+c(1:12)
length(x+c(1:12))
c(1:3)^c(1,2)
(9:11)-c(4,6) -3:3 %% 2
-3:3 %% 3
-3:3 %% c(2,3) -3:3 %/% 2
-3:3 %/% 3
-3:3 %/% c(2,3)


# Time for Action: Matrix Computations 
matrix(nrow=2,ncol=2) 
matrix(1:4,nrow=2,ncol=2, byrow="TRUE") 
A <- matrix(data=1:4, nrow=2, ncol=2, byrow=TRUE, dimnames =  list(c("R_1", "R_2"),c("C_1", "C_2"))) 
dim(A); nrow(A); ncol(A); dimnames(A)
X <- matrix(c(1:12),nrow=3,ncol=4)
Y <- matrix(13:24, nrow=4) X %*% Y t(Y) 
A <- matrix(data=c(13,24,34,23,67,32,45,23,11),nrow=3) 
det(A) 
solve(A)

# Time for Action: Creating a List Object
A <- LETTERS[1:6]; B <- 1:10; C <- matrix(1:6,nrow=2)
Z <- list(A = A, B = B, C = C)
Z
class(Z); class(Z$A); class(Z$B); class(Z$C)

# Time for Action: Creating a “data.frame” Object
x <-c(2,3,4); y <- LETTERS[1:3] 
df1 <- data.frame(x,y) 
variable.names(df1) 
sapply(df1,class) 
df1$x 
df1$y 
df1$z <- c(pi,sqrt(2), 2.71828) 
df1 
df1$x <- NULL 
df1 
df1$x <- x 
df1[4,] <- list(y=LETTERS[2],z=3,x=5) 
df1 <- df1[-2,] 
df1 
row.names(df1) 
dim(df1) 
colnames(df1) 
row.names(df1) <- 1:3 
colnames(df1) <- LETTERS[1:3] 
df1

data(iris)
head(iris,10)
str(iris)
iris$Species
iris[1:10,c("Sepal.Length","Petal.Length")]

 
# The Table Object
Class.Level <- c("1st","2nd","3rd", "Crew")
Sex.Level <- c("Male", "Female")
Age.Level <- c("Child", "Adult")
Survived.Level <- c("No", "Yes")
Data.Level <- list(Class = Class.Level, Sex = Sex.Level, 
Age = Age.Level, Survived = Survived.Level)
T.Table = expand.grid(Class = Class.Level, Sex = 
Sex.Level, Age = Age.Level, Survived = Survived.Level)
T.freq <- c(0,0,35,0,0,0,17,0,118,154,387,670,4,13,89,3, 
5,11, 13,0,1,13,14,0,57,14,75,192,140,80, 76,20)
T.Table <- cbind(T.Table, count=T.freq)
xtabs(count~ Class + Sex + Age + Survived , data = 
T.Table)

# Time for Action: Importing Data from External Files 
employ <- read.table("Employ.dat",header=TRUE)
View(employ)
class(employ)
sapply(employ,class)

SCV <- read.csv("SCV.csv",header=TRUE)

SCV

SCV_Usual <- read.csv("SCV_Usual.csv",header=TRUE,sep=",")
SCV_Modified <- read.csv("SCV_Modified.csv",header=TRUE)
SCV_Combined <- merge(SCV_Usual,SCV_Modified,by.y=c("Response", 
"A","B","C","D","E"),all.x=TRUE,all.y=TRUE)
SCV_Combined

library(foreign)
rootstock.url <- "http://www.stata-press.com/data/r10/rootstock.dta"
rootstock <- read.dta(rootstock.url)
rootstock

# Importing Data from MySQL
install.packages(“RMySQL”) 
# Run the next line at gnome-terminal and not in R session
sudo apt-get install libmysqlclient-dev
library(RMySQL)
d <- dbDriver("MySQL")
con <- dbConnect(d,dbname='test') 
io_data<- dbGetQuerry(con,'select * from IO_Time')

# Exporting Data/Graphs
write.csv(Titanic,"Titanic.csv",row.names=FALSE)
