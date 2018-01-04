#Code for a project for an Item Response Theory analysis of a scale called the Communal Narcissism Inventory.

IRTdatashort<-read.csv(file="C:/Users/Jessica/OneDrive/CommunalNarcissism/IRTdatashort.csv")
IRTdata<-read.csv(file="C:/Users/Jessica/OneDrive/CommunalNarcissism/IRTdata.csv")
diIRT<-read.csv(file="C:/Users/Jessica/OneDrive/CommunalNarcissism/dichotomousirtdata2.csv")
library(CTT)
summary(IRTdata)
summary(IRTdatashort)
IRTdatashort[IRTdatashort==999]<-NA
IRTdata[IRTdata==999]<-NA

#Reliability
diirtjust<-diIRT[1:16]

str(reliability(diirtjust),vector.len=1000)
reliability(IRTdata,output.scored=TRUE)
CNItotal<-rowSums(IRTdata,na.rm=T)
IRTscored<-data.frame(IRTdata,CNItotal)
?reliability()
CNIrel<-reliability(diirtjust)
CNIrel$bis
CNIrel$alphaIfDeleted
CNIrel$itemMean

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

frame<cbind(testscores$score,testscores$scored)

lapply(frame,cttICC)

?cttICC()

library(mirt)

#GRM

cnigrm<-mirt(IRTdata,1,"graded",technical=list(NCYCLES=50000))
scores<-fscores(cnigrm,method='EAP',full.scores=TRUE,scores.only=TRUE)
fullIRTdata<-imputeMissing(cnigrm,scores)
cnigrm<-mirt(fullIRTdata,1,"graded",technical=list(NCYCLES=50000))
cnigrm
str(cnigrm)
is.na(cnigrmshort)
show(cnigrmshort)


#GPCM

cnigpcm<-mirt(IRTdata,1,"gpcm",technical=list(NCYCLES=50000))
scores<-fscores(cnigpcm,method='EAP',full.scores=TRUE,scores.only=TRUE)
fullIRTdatagpcm<-imputeMissing(cnigpcm,scores)
cnigpcm<-mirt(fullIRTdatagpcm,1,"gpcm",technical=list(NCYCLES=50000))

#Nominal

cninom<-mirt(IRTdata,1,"nominal",technical=list(NCYCLES=50000))
scores<-fscores(cninom,method='EAP',full.scores=TRUE,scores.only=TRUE)
fullIRTdatanom<-imputeMissing(cninom,scores)
cninom<-mirt(fullIRTdatanom,1,"nominal",technical=list(NCYCLES=50000))

library(mirt)


coef(cnigrm,IRTpars=TRUE,simplify=TRUE)
coef(cnigpcm,IRTpars=TRUE,simplify=TRUE)
coef(cninom,IRTpars=TRUE,simplify=TRUE)

M2(cnigrm)
M2(cnigpcm)

#Plots

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


#Nested testing

anova(cnigrm,cnigpcm)
anova(cnigpcm,cninom)
anova(cnigrm,cninom)

cnigrm

?itemfit()
itemfit(cnigrm)
itemfit(cnigpcm)
itemfit(cninom)

dput(head(IRTdata))
?dput()

citation(package="CTT")
citation(package="mirt")
