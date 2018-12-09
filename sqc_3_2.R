

#########################################
# Statistical Quality Control
# III. Modeling Process Quality
# Start: 10/23/2018
# by Sean Chiou
########################################
 Time Series Marginal Plot
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
#########################################################
