#Sample of R Code from my comps conducting a meta-analysis of research on narcissism and social media use.  This means we looked at all the the published research on the topic and measured the average size of the relationship between narcissism and social media usage across all studies.

#Use the setwd code below to set to whatever directory (file) you downloaded the git repo into.  This will allow you to open the data from the repo.
setwd("c:/Users/wmcla/desktop/repos/R-Code")
getwd() #Just to confirm you've set the right directory

metadata<-read.csv(file="MetaanalysisNarcandSMforrreduced.csv")
install.packages("metafor")
library(metafor)
attach(metadata)

##The following code tests correlations between narcissism and self-reported time spent on social media across the effect sizes of each of the studies.

dattime<-escalc(measure="COR",ri=Time.Spent,ni=N)
modeltime<-rma(yi,vi,data=dattime)
modeltime

#And here we look at potetial moderators of that relationship, such as age, gender, etc.

modeltime<-rma(yi,vi,data=dattime,mods=~metadata$Mean.Age)
modeltime

modeltime<-rma(yi,vi,data=dattime,mods=~metadata$X.Male)
modeltime

modeltime<-rma(yi,vi,data=dattime,mods=~metadata$Year)
modeltime


modeltime<-rma(yi,vi,data=dattime,mods=~metadata$Platform)
modeltime

modeltime<-rma(yi,vi,data=dattime,mods=~metadata$Platform)
modeltime

modeltime<-rma(yi,vi,data=dattime,mods=~metadata$Measure.of.Narcissism)
modeltime

modeltime<-rma(yi,vi,data=dattime,mods=~metadata$Sample.Type)
modeltime

modeltime<-rma(yi,vi,data=dattime,mods=~metadata$Country)
modeltime

modeltime<-rma(yi,vi,data=dattime,mods=~metadata$Type)
modeltime

modeltime<-rma(yi,vi,data=dattime,mods=~metadata$Type.of.Narcissism)
modeltime

#Here, we break down the same test for specific subtypes of narcissism, such as grandiose or vulnerable.

#TimespentGrandiose

dattimegrandiose<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modeltimegrandiose<-res<-rma(yi,vi,data=dattimegrandiose,slab=paste(Authors, Year, sep=", "))
modeltimegrandiose

dattimegrandiose<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modeltimegrandiose<-res<-rma(yi,vi,data=dattimegrandiose)
modeltimegrandiose

######Plots###################################################################################################################

#Here, we create a funnel plot for these tests

funnel(modeltimegrandiose, main="Standard Error")

#Pulling in a file that is a subset of the data in order to do a forest plot

timespent<-read.csv(file="MetaanalysisNarcandSMforrtimespent.csv")
attach(timespent)
detach(metadata)

#Forest Plot

#windows(width=40,height=40) #optional code to do plot in a separate window.

par(mar=c(4,4,1,2))
par(font=1)
forest(modeltimegrandiose, cex=.75,ylim=c(-1, 21),xlab="Time Spent", mlab="RE Model for All Studies")
par(font=2)

text(-1.4,                19.5, "Author(s) and Year",     pos=4)
text(1.5,21,"Estimate and 95% CI",pos=1)

op <- par(cex=.75, font=4)
par(op)



dev.off() #stop working on plot.  This gets rid of the plot, so you want to save it before running this.

#Then I repeated the above analyses on the vulnerable narcissism data to see if they are different. After this, I detached my main dataset 

detach(metadata)

###Subgroups##########################################################################################################################################

#After doing the moderation tests above and finding which were significant, I broke the dataset into subsets below to find out how the relationships differed between groups.
#I.E., basically post hoc tests for a meta-analysis.

#For example, this code separates out published versus unpublished data in case you want to do analyses just on published datasets.
#You would just run this code, and then go back to the top to run the tests again to see how the relatinship is different in the subgroups.

metadataPublished<-metadata[Published=="Yes",]
attach(metadataPublished)
detach(metadataPublished)

#TimeSpentUndergraduate

metadatastudent<-metadata[metadata$Sample.Type=="Undergraduate",]
attach(metadatastudent)
detach(metadatastudent)

#TimeSpentMturk

metadatamturk<-metadata[metadata$Sample.Type=="Mturk",]
attach(metadatamturk)
detach(metadatamturk)

#TimeSpentUS

metadataUS<-metadata[metadata$Country=="US/Canada",]
detach(metadata)
attach(metadataUS)
detach(metadataUS)
attach(metadata)

#All of this would then be repeated for the other measures of social media use such as selfies posted, status updates, etc. 

