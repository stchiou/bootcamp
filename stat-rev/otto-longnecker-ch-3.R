#########################################################
# Describing Data on a Single Variable: Graphical Methods
#
# Start 2/13/2019
#########################################################
#3.1
#a
program <- c("National Defense","Social Security","Medicare & Medicaid","National Debt Interest","Major Social-Aid Programs","Others")
expenditure <- c(525,500,500,300,200,475)
pie(expenditure,labels=program,main="Federal Government Programs")
#b
barplot(expenditure,names.arg=program,ylab="2006 Expenditures (Billions of Dollars)",main="Federal Government Programs",las=2)
#c
total <- sum(expenditure)
percent <- expenditure/total*100
pie(percent,labels=program, main="Federal Government Programs")
barplot(percent,names.arg=program,ylab="2006 Expenditures (%)",main="Federal Government Programs",las=2)
#3.2
year<- c("1990","1995","1997","1998","1999","2000","2001","2002")
passenger<- c(9436,8687,8273,8142,8697,8852,8422,8082)
suv_truck<-c(4733,6517,7226,7821,8717,8965,9050,9036)
vehicle <- rbind(passenger,suv_truck)
barplot(vehicle,main="Vehicles Sold",names.arg=year,xlab="Year",ylab="Thousands of Units",col=c("darkblue","red"),las=2, beside=TRUE)