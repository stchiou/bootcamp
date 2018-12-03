#########################################
# Statistical Quality Control
# III. Modeling Process Quality
# Start: 10/23/2018
# by Sean Chiou
########################################
# Stem-and-leaf Plot
########################################
days <- c(48,41,35,36,37,26,36,46,35,47,35,34,36,42,43,36,56,32,46,30,37,43,17,26,28,27,45,33,22,27,16,22,33,30,24,23,22,30,31,17)
tempplot1 <- capture.output(stem(days))
plot.new()
png(filename="stem.png")
text(0,1,paste(tempplot1,collapse='\n'),adj=c(0,1),family='mono')
device.off()
########################################
# 2. Time Series Marginal Plot
########################################
times <- seq(1:40)
df <- data.frame(times,days)
new_layout <- layout(matrix(c(1,2),nrow=1,ncol=2),widths=c(5,1),heights=c(5,5),TRUE)
par(mar=c(5,4,2,0))
png(filename="timeplot.png")
plot(df,xlab="Times",ylab="Days",main="Marginal Plot")
par(mar=c(5,0,2,1))
png(filename="strip.png")
stripchart(days,method="stack",offset=0.5,vertical=TRUE,axes=FALSE)
dev.off()
#########################################
# 3. Histograms
#########################################
thick <- c(438,413,444,468,445,472,474,454,455,449,450,450,450,459,466,470,457,441,450,445,487,430,446,450,456,433,455,459,423,455,451,437,444,453,434,454,448,435,432,441,452,465,466,473,471,464,478,446,459,464,441,444,458,454,437,443,465,435,444,457,444,471,471,458,459,449,462,460,445,437,461,453,452,438,445,435,454,428,454,434,432,431,455,447,454,435,425,449,449,452,471,458,445,463,423,451,440,442,441,439)
hist(thick)

defect <- c(6,1,5,7,8,6,0,2,4,2,5,2,4,4,1,4,1,7,2,3,4,3,3,3,6,3,2,3,4,5,5,2,3,4,4,4,2,3,5,7,5,4,5,5,4,5,3,3,3,12)
hist(defect,12)
########################################
# 4. Mean Calculation
########################################
mean(thick)
#######################################
# 5. Variance Calculation
#######################################
var(thick)
#######################################
# 6. Standard Deviation Calculation
#######################################
sd(thick)
#######################################
# 7. Box Plot
#######################################
diameter <- c(120.5,120.9,120.3,121.3,120.4,120.2,120.1,120.5,120.7,121.1,120.9,120.8)
bp <- boxplot(diameter, horizontal=TRUE, xlab="Hole Diameters (mm)", border=TRUE, frame=FALSE,axes=FALSE)
leg <- as.character(bp$stats)
text(x=bp$stats,y=0.7,labels=leg)
###########################################
# 8. Cumulative Distribution, Hypergeometric
###########################################
# Displaying Cumulative Distribution Function of Hypergeometric Distribution
# x=0,1,2,3,4,5,6,7,8,9,10
# D=5; nonconforming
# lot size, N =5+95=100, conforming=100-5=95
# n=10; sampling size
# p(X $\le$ x)
library(gridExtra)
library(grid)
drawn <- c(0:10)
cumulative.probability <- phyper(drawn,5,95,10)
# probability of 0, 1, 2, 3,...,10 nonconformative items found in the sample.
hyper <- cbind(drawn,cumulative.probability)
grid.newpage()
g <- grid.table(hyper)
#########################################
# 9. Binomial Distribution
########################################
library(grid)
library(gridExtra)
drawn=c(0:10)
pd <- dbinom(drawn,15,0.1)
bd <- cbind(drawn,pd)
grid.newpage()
grid.table(bd)
###########################################
# 10. Binomial Distribution of Different p
###########################################
n=c(0:15)
bd0.1 <- dbinom(n,15,0.1)
bd0.5 <- dbinom(n,15,0.5)
bd0.9 <- dbinom(n,15,0.9)
barplot(bd0.1,n,col="blue",xlab="x",ylab="f(x)",beside=TRUE,axes=TRUE,width=15,space=1,ylim=c(0,0.4))
barplot(bd0.5,n,col="red",xlab="x",ylab="f(x)",beside=TRUE,axes=TRUE,width=15,space=1,add=TRUE)
barplot(bd0.9,n,col="yellow",xlab="x",ylab="f(x)",beside=TRUE,axes=TRUE,width=15,space=1,add=TRUE)
###################################################
# 11. Binomial Distribution of Different n, fixed p
###################################################
n <- c(0:30)
bd1 <- dbinom(n,10,0.25)
bd2 <- dbinom(n,20,0.25)
bd3 <- dbinom(n,40,0.25)
barplot(bd1,n,col="blue",xlab="x",ylab="f(x)",beside=TRUE,axes=TRUE,width=15,space=1,ylim=c(0,0.3))
barplot(bd2,n,col="red",xlab="x",ylab="f(x)",beside=TRUE,axes=TRUE,width=15,space=1,add=TRUE)
barplot(bd3,n,col="yellow",xlab="x",ylab="f(x)",beside=TRUE,axes=TRUE,width=15,space=1,add=TRUE)
#####################################################
# 12. Poisson Distribution--Density Function
#####################################################
library(grid)
library(gridExtra)
x <- c(0,1,2)
P <- dpois(q,4)
pd <-cbind(x,P)
grid.newpage()
grid.table(pd)
#######################################################
# 13. Poison Probability Distribution, Different Lambda
#######################################################
x <- c(0:40)
pd1 <- dpois(x,4)
pd2 <- dpois(x,8)
pd3 <- dpois(x,12)
pd4 <- dpois(x,16)
barplot(pd1,x,col="blue",xlab="x",ylab="p(x)",beside=TRUE,axes=FALSE,width=15,space=1,ylim=c(0,0.2))
barplot(pd2,x,col="red",xlab="x",ylab="p(x)",beside=TRUE,axes=TRUE,width=15,space=1,add=TRUE)
barplot(pd3,x,col="yellow",xlab="x",ylab="p(x)",beside=TRUE,axes=FALSE,width=15,space=1,add=TRUE)
barplot(pd4,x,col="green",xlab="x",ylab="p(x)",beside=TRUE,axes=FALSE,width=15,space=1,add=TRUE)
#########################################################
