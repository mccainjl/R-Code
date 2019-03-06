#Code for a class project for an Item Response Theory analysis of a scale called the Communal Narcissism Inventory.

#Important! Set the directory in the line below to the folder you're working from, i.e., where you downloaded the repo to.
setwd("C:/Users/wmcla/desktop/repos/R-Code")
getwd()

#Read in the data:

IRTdata<-read.csv(file="IRTdata.csv")


#Obtain and load the necessary libraries

install.packages("CTT")
install.packages("mirt")
library(CTT)
library(mirt)

#Clean up the data, including replacing NA's

summary(IRTdata)
IRTdata[IRTdata==999]<-NA

#Reliability according to classical test theory, aka Cronbach's alpha

reliability(IRTdata)


#Calculating a total score and creating a separate dataset that has the total score included as a column. 
#this scale, luckily, contains no reverse scored items to worry about

CNItotal<-rowSums(IRTdata,na.rm=T)
IRTscored<-data.frame(IRTdata,CNItotal)

#It can also help to look at what the alpha would be if each item were deleted.  Ideally, we want to remove items
#if the alpha would be higher without them.

CNIrel<-reliability(IRTdata)
CNIrel$bis
CNIrel$alphaIfDeleted
CNIrel$itemMean

#We also want to view item characteristic curves, or ICC.  These look at how well each item identifies people at
#different levels of the overall score (in this case, at different levels of communal narcissism)


cttICC(IRTscored$CNItotal,IRTscored$CNI1,plotTitle="ICC for Item 1",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI2,plotTitle="ICC for Item 2",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI3,plotTitle="ICC for Item 3",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI4,plotTitle="ICC for Item 4",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI5,plotTitle="ICC for Item 5",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI6,plotTitle="ICC for Item 6",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI7,plotTitle="ICC for Item 7",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI8,plotTitle="ICC for Item 8",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI9,plotTitle="ICC for Item 9",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI10,plotTitle="ICC for Item 10",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI11,plotTitle="ICC for Item 11",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI12,plotTitle="ICC for Item 12",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI13,plotTitle="ICC for Item 13",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI14,plotTitle="ICC for Item 14",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI15,plotTitle="ICC for Item 15",xlab="CNI Total Score",ylab="Proportion Correct (p)")
cttICC(IRTscored$CNItotal,IRTscored$CNI16,plotTitle="ICC for Item 16",xlab="CNI Total Score",ylab="Proportion Correct (p)")



#Now we will test the first model, the graded response model, or GRM.  Because our items have ordered categorical scores 
#(i.e., they were responded to on a seven point likert-type scale), the simpler models, 1PL and 2PL, don't apply here.
#This model looks at both difficulty of the item and discrimination

cnigrm<-mirt(IRTdata,1,"graded",technical=list(NCYCLES=50000))
scores<-fscores(cnigrm,method='EAP',full.scores=TRUE,scores.only=TRUE)
fullIRTdata<-imputeMissing(cnigrm,scores)
cnigrm<-mirt(fullIRTdata,1,"graded",technical=list(NCYCLES=50000))
cnigrm
str(cnigrm)
is.na(cnigrm)
show(cnigrm)

#Next, we will test the generalized partial credit model, or GPCM

cnigpcm<-mirt(IRTdata,1,"gpcm",technical=list(NCYCLES=50000))
scores<-fscores(cnigpcm,method='EAP',full.scores=TRUE,scores.only=TRUE)
fullIRTdatagpcm<-imputeMissing(cnigpcm,scores)
cnigpcm<-mirt(fullIRTdatagpcm,1,"gpcm",technical=list(NCYCLES=50000))

#Finally, we will test the Nominal model.  

cninom<-mirt(IRTdata,1,"nominal",technical=list(NCYCLES=50000))
scores<-fscores(cninom,method='EAP',full.scores=TRUE,scores.only=TRUE)
fullIRTdatanom<-imputeMissing(cninom,scores)
cninom<-mirt(fullIRTdatanom,1,"nominal",technical=list(NCYCLES=50000))

#We can also pull the parameters for each question to find out for which groups of people they are the most discriminating.

coef(cnigrm,IRTpars=TRUE,simplify=TRUE)
coef(cnigpcm,IRTpars=TRUE,simplify=TRUE)
coef(cninom,IRTpars=TRUE,simplify=TRUE)

M2(cnigrm)
M2(cnigpcm)

#Plots include theta limits and information curves.

plot(cnigrm,theta_lim=c(-3,3))
plot(cnigpcm,theta_lim=c(-3,3))
plot(cninom,theta_lim=c(-3,3))
plot(cnigrm,type='info',theta_lim=c(-3,3))
plot(cnigpcm,type='info',theta_lim=c(-3,3))
plot(cninom,type='info',theta_lim=c(-3,3))
?plot()
itemplot(cnigrm,1,theta_lim=c(-3,3))
itemplot(cnigrm,2,theta_lim=c(-3,3))
itemplot(cnigrm,3,theta_lim=c(-3,3))
itemplot(cnigrm,4,theta_lim=c(-3,3))
itemplot(cnigrm,5,theta_lim=c(-3,3))
itemplot(cnigrm,6,theta_lim=c(-3,3))
itemplot(cnigrm,7,theta_lim=c(-3,3))
itemplot(cnigrm,8,theta_lim=c(-3,3))
itemplot(cnigrm,9,theta_lim=c(-3,3))
itemplot(cnigrm,10,theta_lim=c(-3,3))
itemplot(cnigrm,11,theta_lim=c(-3,3))
itemplot(cnigrm,12,theta_lim=c(-3,3))
itemplot(cnigrm,13,theta_lim=c(-3,3))
itemplot(cnigrm,14,theta_lim=c(-3,3))
itemplot(cnigrm,15,theta_lim=c(-3,3))
itemplot(cnigrm,16,theta_lim=c(-3,3))

theta<-seq(-4,4,by=.1)
?seq()

theta

testinfo(cnigrm,matrix(theta))

itemplot(cnigrm,1,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,2,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,3,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,4,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,5,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,6,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,7,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,8,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,9,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,10,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,11,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,12,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,13,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,14,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,15,type='info',theta_lim=c(-3,3))
itemplot(cnigrm,16,type='info',theta_lim=c(-3,3))

itemplot(cnigpcm,1,theta_lim=c(-3,3))
itemplot(cnigpcm,2,theta_lim=c(-3,3))
itemplot(cnigpcm,3,theta_lim=c(-3,3))
itemplot(cnigpcm,4,theta_lim=c(-3,3))
itemplot(cnigpcm,5,theta_lim=c(-3,3))
itemplot(cnigpcm,6,theta_lim=c(-3,3))
itemplot(cnigpcm,7,theta_lim=c(-3,3))
itemplot(cnigpcm,8,theta_lim=c(-3,3))
itemplot(cnigpcm,9,theta_lim=c(-3,3))
itemplot(cnigpcm,10,theta_lim=c(-3,3))
itemplot(cnigpcm,11,theta_lim=c(-3,3))
itemplot(cnigpcm,12,theta_lim=c(-3,3))
itemplot(cnigpcm,13,theta_lim=c(-3,3))
itemplot(cnigpcm,14,theta_lim=c(-3,3))
itemplot(cnigpcm,15,theta_lim=c(-3,3))
itemplot(cnigpcm,16,theta_lim=c(-3,3))

itemplot(cnigpcm,1,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,2,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,3,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,4,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,5,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,6,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,7,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,8,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,9,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,10,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,11,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,12,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,13,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,14,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,15,type='info',theta_lim=c(-3,3))
itemplot(cnigpcm,16,type='info',theta_lim=c(-3,3))

itemplot(cninom,1,theta_lim=c(-3,3))
itemplot(cninom,2,theta_lim=c(-3,3))
itemplot(cninom,3,theta_lim=c(-3,3))
itemplot(cninom,4,theta_lim=c(-3,3))
itemplot(cninom,5,theta_lim=c(-3,3))
itemplot(cninom,6,theta_lim=c(-3,3))
itemplot(cninom,7,theta_lim=c(-3,3))
itemplot(cninom,8,theta_lim=c(-3,3))
itemplot(cninom,9,theta_lim=c(-3,3))
itemplot(cninom,10,theta_lim=c(-3,3))
itemplot(cninom,11,theta_lim=c(-3,3))
itemplot(cninom,12,theta_lim=c(-3,3))
itemplot(cninom,13,theta_lim=c(-3,3))
itemplot(cninom,14,theta_lim=c(-3,3))
itemplot(cninom,15,theta_lim=c(-3,3))
itemplot(cninom,16,theta_lim=c(-3,3))

itemplot(cninom,1,type='info',theta_lim=c(-3,3))
itemplot(cninom,2,type='info',theta_lim=c(-3,3))
itemplot(cninom,3,type='info',theta_lim=c(-3,3))
itemplot(cninom,4,type='info',theta_lim=c(-3,3))
itemplot(cninom,5,type='info',theta_lim=c(-3,3))
itemplot(cninom,6,type='info',theta_lim=c(-3,3))
itemplot(cninom,7,type='info',theta_lim=c(-3,3))
itemplot(cninom,8,type='info',theta_lim=c(-3,3))
itemplot(cninom,9,type='info',theta_lim=c(-3,3))
itemplot(cninom,10,type='info',theta_lim=c(-3,3))
itemplot(cninom,11,type='info',theta_lim=c(-3,3))
itemplot(cninom,12,type='info',theta_lim=c(-3,3))
itemplot(cninom,13,type='info',theta_lim=c(-3,3))
itemplot(cninom,14,type='info',theta_lim=c(-3,3))
itemplot(cninom,15,type='info',theta_lim=c(-3,3))
itemplot(cninom,16,type='info',theta_lim=c(-3,3))


#Nested testing is used to determine which of the three models provide the best model fit.
#

anova(cnigrm,cnigpcm)
anova(cnigpcm,cninom)
anova(cnigrm,cninom)

#We can also test the fit of the model to each item in the CNI data.

itemfit(cnigrm)
itemfit(cnigpcm)
itemfit(cninom)

dput(head(IRTdata))


citation(package="CTT")
citation(package="mirt")
