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