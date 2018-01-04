#R Code from my comps conducting a meta-analysis of research on narcissism and social media use.

metadata<-read.csv(file="C:/Users/Jessica/OneDrive/Comps Summer Reading List/MetaanalysisNarcandSMforrreduced.csv")
library(metafor)

#Timespent######################################################################################

dattime<-escalc(measure="COR",ri=Time.Spent,ni=N)
modeltime<-rma(yi,vi,data=dattime)
modeltime

#Timespentmoderators###########################################################################

attach(metadata)

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

attach(metadata)

#TimespentGrandiose

?res

dattimegrandiose<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modeltimegrandiose<-res<-rma(yi,vi,data=dattimegrandiose,slab=paste(Authors, Year, sep=", "))
modeltimegrandiose

dattimegrandiose<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modeltimegrandiose<-res<-rma(yi,vi,data=dattimegrandiose)
modeltimegrandiose

#data(dat.bcg)
#dat.bcg

funnel(modeltimegrandiose, main="Standard Error")

?forest

timespent<-read.csv(file="C:/Users/Jessica/OneDrive/Comps Summer Reading List/MetaanalysisNarcandSMforrtimespent.csv")
attach(timespent)
detach(metadata)

windows(width=40,height=40)

par(mar=c(4,4,1,2))
par(font=1)
forest(modeltimegrandiose, cex=.75,ylim=c(-1, 21),xlab="Time Spent", mlab="RE Model for All Studies")
par(font=2)

text(-1.4,                19.5, "Author(s) and Year",     pos=4)
text(1.50,20,"Estimate and 95% CI",pos=1)

op <- par(cex=.75, font=4)
par(op)



dev.off()

#MetadataPublished

metadataPublished<-metadata[Published=="Yes",]
attach(metadataPublished)
detach(metadataPublished)


#TimespentVulnerable

dattimevulnerable<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Vulnerable"],ni=N[Type.of.Narcissism=="Vulnerable"])
modeltimevulnerable<-res<-rma(yi,vi,data=dattimevulnerable)
modeltimevulnerable

#TimespentDark

dattimeDark<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Dark Triad"],ni=N[Type.of.Narcissism=="Dark Triad"])
modeltimeDark<-res<-rma(yi,vi,data=dattimeDark)
modeltimeDark

#TimeSpentFacebook

metadataFB<-metadata[Platform=="Facebook",]
attach(metadataFB)
detach(metadataFB)

#TimeSpentInstagram

attach(metadata)

metadatainstagram<-metadata[Platform=="Instagram",]
attach(metadatainstagram)
detach(metadatainstagram)

#TimeSpentAll

metadataall<-metadata[Platform=="All",]
attach(metadataall)
detach(metadataall)

#TimespentGrandioseFB

attach(metadataFB)
dattimegrandiose<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modeltimegrandiose<-res<-rma(yi,vi,data=dattimegrandiose)
modeltimegrandiose


#TimespentVulnerable

dattimevulnerable<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Vulnerable"],ni=N[Type.of.Narcissism=="Vulnerable"])
modeltimevulnerable<-res<-rma(yi,vi,data=dattimevulnerable)
modeltimevulnerable

#TimespentDark

dattimeDark<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Dark Triad"],ni=N[Type.of.Narcissism=="Dark Triad"])
modeltimeDark<-res<-rma(yi,vi,data=dattimeDark)
modeltimeDark

detach(metadataFB)

#TimeSpentObjective

detach(metadata)
metadataob<-metadata[metadata$Type=="Objective",]
attach(metadataob)
detach(metadataob)

#TimeSpentSubjective

metadatasub<-metadata[metadata$Type=="Self-Report",]
attach(metadatasub)
detach(metadatasub)

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

#TimeSpent Asian 

metadataAsian<-metadata[metadata$Country=="Asian",]
detach(metadata)
attach(metadataAsian)
detach(metadataAsian)

#TimeSpent Russian 

metadataRussian<-metadata[metadata$Country=="Russian",]
detach(metadata)
attach(metadataRussian)
detach(metadataRussian)

#TimeSpent European

metadataEuropean<-metadata[metadata$Country=="European",]
detach(metadata)
attach(metadataEuropean)
detach(metadataEuropean)

#TimeSpent Adolescent

metadataadolescent<-metadata[metadata$Sample.Type=="Adolescent",]
attach(metadataadolescent)
detach(metadataadolescent)

#Timespent Internet

metadatainternet<-metadata[metadata$Sample.Type=="Internet",]
attach(metadatainternet)
detach(metadatainternet)

#TimeSpent NPI-40

metadataNPI40<-metadata[metadata$Measure.of.Narcissism=="NPI40",]
attach(metadataNPI40)
detach(metadataNPI40)

#TimeSpent NPI-16

metadataNPI16<-metadata[metadata$Measure.of.Narcissism=="NPI16",]
attach(metadataNPI16)
detach(metadataNPI16)

#TimeSpent NIR

metadataNIR<-metadata[metadata$Measure.of.Narcissism=="NIR",]
attach(metadataNIR)
detach(metadataNIR)



datselfiesgrandiose<-escalc(measure="COR",ri=Selfies[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modelselfiesgrandiose<-res<-rma(yi,vi,data=datselfiesgrandiose)
modelselfiesgrandiose



#FreqStatusUpdatesall#######################################################################

attach(metadata)

datstatus<-escalc(measure="COR",ri=Freq.Status.Updates.Tweets,ni=N)
modelstatus<-res<-rma(yi,vi,data=datstatus)
modelstatus

modelstatus<-rma(yi,vi,data=datstatus,mods=~metadata$Type.of.Narcissism)
modelstatus

modelstatus<-rma(yi,vi,data=datstatus,mods=~metadata$Platform)
modelstatus

modelstatus<-rma(yi,vi,data=datstatus,mods=~metadata$Measure.of.Narcissism)
modelstatus

modelstatus<-rma(yi,vi,data=datstatus,mods=~metadata$Sample.Type)
modelstatus

modelstatus<-rma(yi,vi,data=datstatus,mods=~metadata$Country)
modelstatus

modelstatus<-rma(yi,vi,data=datstatus,mods=~metadata$Type)
modelstatus

modelstatus<-rma(yi,vi,data=datstatus,mods=~metadata$Type.of.Narcissism)
modelstatus














detach(metadata)

#StatusUpdatesGrandiose

status<-read.csv(file="C:/Users/Jessica/OneDrive/Comps Summer Reading List/MetaanalysisNarcandSMforrstatus.csv")
attach(status)
detach(timespent)
detach(status)
detach(friends)

datstatusgrandiose<-escalc(measure="COR",ri=Freq.Status.Updates.Tweets[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modelstatusgrandiose<-res<-rma(yi,vi,data=datstatusgrandiose)
modelstatusgrandiose

datstatus<-escalc(measure="COR",ri=Freq.Status.Updates.Tweets,ni=N)
modelstatus<-res<-rma(yi,vi,data=datstatus,slab=paste(Authors, Year, sep=","))
modelstatus

funnel(modelstatusgrandiose, main="Standard Error")

windows(width=40,height=40)

par(mar=c(4,4,1,2))
par(font=1)
forest(modelstatus, at=log(c(.05, .25, 1, 4)),cex=.75,ylim=c(-1, 36),rows=c(4:24,30:32),order=order(Type.of.Narcissism),xlab="Status Updates", mlab="RE Model for All Studies")

par(font=2)
text(-2.40,                36, "Author(s) and Year",     pos=1)
text(1.40,36,"Estimate and 95% CI",pos=1)
par(font=1)
par(font=3)
text(-2.5,34,"Vulnerable",pos=1)
text(-2.5,26.5,"Grandiose",pos=1)

addpoly(modelstatusvulnerable, row=28.5, cex=.75, mlab="RE Model for Subgroup")
addpoly(modelstatusgrandiose, row=2.5, cex=.75, mlab="RE Model for Subgroup")

#?text()

par(op)
op <- par(cex=.75, font=4)

#TimespentVulnerable

datstatusvulnerable<-escalc(measure="COR",ri=Freq.Status.Updates.Tweets[Type.of.Narcissism=="Vulnerable"],ni=N[Type.of.Narcissism=="Vulnerable"])
modelstatusvulnerable<-res<-rma(yi,vi,data=datstatusvulnerable)
modelstatusvulnerable

#TimespentDark

datstatusDark<-escalc(measure="COR",ri=Freq.Status.Updates.Tweets[Type.of.Narcissism=="Dark Triad"],ni=N[Type.of.Narcissism=="Dark Triad"])
modelstatusDark<-res<-rma(yi,vi,data=datstatusDark)
modelstatusDark

#TimeSpentFacebook

metadataFB<-metadata[Platform=="Facebook",]
detach(metadata)

#TimespentGrandioseFB

attach(metadataFB)
dattimegrandiose<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modeltimegrandiose<-res<-rma(yi,vi,data=dattimegrandiose)
modeltimegrandiose

#TimespentVulnerable

dattimevulnerable<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Vulnerable"],ni=N[Type.of.Narcissism=="Vulnerable"])
modeltimevulnerable<-res<-rma(yi,vi,data=dattimevulnerable)
modeltimevulnerable

#TimespentDark

dattimeDark<-escalc(measure="COR",ri=Time.Spent[Type.of.Narcissism=="Dark Triad"],ni=N[Type.of.Narcissism=="Dark Triad"])
modeltimeDark<-res<-rma(yi,vi,data=dattimeDark)
modeltimeDark

detach(metadataFB)

#TimeSpentObjective

detach(metadata)
metadataob<-metadata[metadata$Type=="Objective",]
attach(metadataob)
detach(metadataob)

#TimeSpentObjective

metadatasub<-metadata[metadata$Type=="Self-Report",]
attach(metadatasub)
detach(metadatasub)

#TimeSpentUndergraduate

metadatastudent<-metadata[metadata$Sample.Type=="Undergraduate",]
attach(metadatastudent)
detach(metadatastudent)

#TimeSpentUndergraduate

metadatamturk<-metadata[metadata$Sample.Type=="Mturk",]
attach(metadatamturk)
detach(metadatamturk)

#TimeSpentUS

metadataUS<-metadata[metadata$Country=="US/Canada",]
detach(metadata)
attach(metadataUS)

##Friends#####################################################################################

attach(metadata)

detach(timespent)
detach(status)
friends<-read.csv(file="C:/Users/Jessica/OneDrive/Comps Summer Reading List/MetaanalysisNarcandSMforrfriends.csv")
attach(friends)
detach(friends)

datfriends<-escalc(measure="COR",ri=Friends.Followers,ni=N)
modelfriends<-res<-rma(yi,vi,data=datfriends,slab=paste(Authors, Year, sep=", "))
modelfriends

datfriends<-escalc(measure="COR",ri=Friends.Followers,ni=N)
modelfriends<-res<-rma(yi,vi,data=datfriends)
modelfriends


windows(width=40,height=40)

par(mar=c(4,4,1,2))
par(font=1)
forest(modelfriends, at=log(c(.05, .25, 1, 4)),cex=.75,ylim=c(-1, 41),order=order(Type.of.Narcissism),rows=c(4:27,33:36),xlab="Friends and Followers", mlab="RE Model for All Studies")

par(font=2)
text(-2.40,                41, "Author(s) and Year",     pos=1)
text(1.40,41,"Estimate and 95% CI",pos=1)
par(font=1)
par(font=3)
text(-2.5,39,"Vulnerable",pos=1)
text(-2.5,30,"Grandiose",pos=1)

addpoly(modelfriendsvulnerable, row=31.5, cex=.75, mlab="RE Model for Subgroup")
addpoly(modelfriendsgrandiose, row=2.5, cex=.75, mlab="RE Model for Subgroup")

#FriendsGrandiose

datfriendsgrandiose<-escalc(measure="COR",ri=Friends.Followers[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modelfriendsgrandiose<-res<-rma(yi,vi,data=datfriendsgrandiose)
modelfriendsgrandiose

funnel(modelfriendsgrandiose, main="Standard Error")

#FriendsVulnerable

datfriendsvulnerable<-escalc(measure="COR",ri=Friends.Followers[Type.of.Narcissism=="Vulnerable"],ni=N[Type.of.Narcissism=="Vulnerable"])
modelfriendsvulnerable<-res<-rma(yi,vi,data=datfriendsvulnerable)
modelfriendsvulnerable

modelfriends<-rma(yi,vi,data=datfriends,mods=~metadata$Type.of.Narcissism)
modelfriends

modelfriends<-rma(yi,vi,data=datfriends,mods=~metadata$Platform)
modelfriends

modelfriends<-rma(yi,vi,data=datfriends,mods=~metadata$Measure.of.Narcissism)
modelfriends

modelfriends<-rma(yi,vi,data=datfriends,mods=~metadata$Sample.Type)
modelfriends

modelfriends<-rma(yi,vi,data=datfriends,mods=~metadata$Country)
modelfriends

modelfriends<-rma(yi,vi,data=datfriends,mods=~metadata$Type)
modelfriends

#TimeSpentUS

metadataUS<-metadata[metadata$Country=="US/Canada",]
detach(metadata)
attach(metadataUS)

metadatanonUS<-metadata[metadata$Country!="US/Canada",]
detach(metadata)
attach(metadatanonUS)
detach(metadatanonUS)

##Pictures of self/selfies##################################################################
detach(metadatainternet)
attach(metadata)
detach(friends)
selfies<-read.csv(file="C:/Users/Jessica/OneDrive/Comps Summer Reading List/MetaanalysisNarcandSMforrselfies.csv")
attach(selfies)
detach(selfies)

datselfies<-escalc(measure="COR",ri=Selfies,ni=N)
modelselfies<-res<-rma(yi,vi,data=datselfies,slab=paste(Authors, Year, sep=", "))
modelselfies

windows(width=40,height=40)

par(mar=c(4,4,1,2))
par(font=1)
forest(modelselfies, at=log(c(.05, .25, 1, 4)),cex=.75,ylim=c(-1, 19),order=order(Type.of.Narcissism),rows=c(2:9,13:15),xlab="Selfies Posted", mlab="RE Model for All Studies")

par(font=2)
text(-2.40,                18, "Author(s) and Year",     pos=1)
text(.80,18,"Estimate and 95% CI",pos=1)
par(font=1)
par(font=3)
text(-2.5,17,"Vulnerable",pos=1)
text(-2.5,11,"Grandiose",pos=1)

addpoly(modelselfiesvulnerable, row=12, cex=.75, mlab="RE Model for Subgroup")
addpoly(modelselfiesgrandiose, row=.75, cex=.75, mlab="RE Model for Subgroup")

modelselfies<-rma(yi,vi,data=datselfies,mods=~metadata$Type.of.Narcissism)
modelselfies

modelselfies<-rma(yi,vi,data=datselfies,mods=~metadata$Platform)
modelselfies

modelselfies<-rma(yi,vi,data=datselfies,mods=~metadata$Measure.of.Narcissism)
modelselfies

modelselfies<-rma(yi,vi,data=datselfies,mods=~metadata$Sample.Type)
modelselfies

modelselfies<-rma(yi,vi,data=datselfies,mods=~metadata$Country)
modelselfies

modelselfies<-rma(yi,vi,data=datselfies,mods=~metadata$Type)
modelselfies

#TimespentGrandiose

datselfiesgrandiose<-escalc(measure="COR",ri=Selfies[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modelselfiesgrandiose<-res<-rma(yi,vi,data=datselfiesgrandiose)
modelselfiesgrandiose

funnel(modelselfiesgrandiose, main="Standard Error")

#TimespentVulnerable

datselfiesvulnerable<-escalc(measure="COR",ri=Selfies[Type.of.Narcissism=="Vulnerable"],ni=N[Type.of.Narcissism=="Vulnerable"])
modelselfiesvulnerable<-res<-rma(yi,vi,data=datselfiesvulnerable)
modelselfiesvulnerable

#TimespentDark

datstatusDark<-escalc(measure="COR",ri=Pictures.of.Self.Selfies[Type.of.Narcissism=="Dark Triad"],ni=N[Type.of.Narcissism=="Dark Triad"])
modelstatusDark<-res<-rma(yi,vi,data=datstatusDark)
modelstatusDark

metadataNPI13<-metadata[metadata$Measure.of.Narcissism=="NPI-13",]
detach(metadata)
attach(metadataNPI13)
detach(metadataNPI13)

metadataNPI40<-metadata[metadata$Measure.of.Narcissism=="NPI-40",]
attach(metadataNPI40)
detach(metadataNPI40)

metadataNPQC<-metadata[metadata$Measure.of.Narcissism=="NPQC-R",]
attach(metadataNPQC)
detach(metadataNPQC)

metadataHSNS<-metadata[metadata$Measure.of.Narcissism=="HSNS",]
attach(metadataHSNS)
detach(metadataHSNS)

metadataDD<-metadata[metadata$Measure.of.Narcissism=="Dirty Dozen",]
attach(metadataDD)
detach(metadataDD)

####Other focused (communal) behavior#######################################################

attach(metadata)

datother<-escalc(measure="COR",ri=Other.Focused.Activities,ni=N)
modelother<-res<-rma(yi,vi,data=datother)
modelother

modelother<-rma(yi,vi,data=datother,mods=~metadata$Type.of.Narcissism)
modelother

modelother<-rma(yi,vi,data=datother,mods=~metadata$Platform)
modelother

modelother<-rma(yi,vi,data=datother,mods=~metadata$Measure.of.Narcissism)
modelother

modelother<-rma(yi,vi,data=datother,mods=~metadata$Sample.Type)
modelother

modelother<-rma(yi,vi,data=datother,mods=~metadata$Country)
modelother

modelother<-rma(yi,vi,data=datother,mods=~metadata$Type)
modelother

#TimespentGrandiose

datothergrandiose<-escalc(measure="COR",ri=Other.Focused.Activities[Type.of.Narcissism=="Grandiose"],ni=N[Type.of.Narcissism=="Grandiose"])
modelothergrandiose<-res<-rma(yi,vi,data=datothergrandiose)
modelothergrandiose

#TimespentVulnerable

datstatusvulnerable<-escalc(measure="COR",ri=Other.Focused.Activities[Type.of.Narcissism=="Vulnerable"],ni=N[Type.of.Narcissism=="Vulnerable"])
modelstatusvulnerable<-res<-rma(yi,vi,data=datstatusvulnerable)
modelstatusvulnerable

#TimespentDark

datstatusDark<-escalc(measure="COR",ri=Other.Focused.Activities[Type.of.Narcissism=="Dark Triad"],ni=N[Type.of.Narcissism=="Dark Triad"])
modelstatusDark<-res<-rma(yi,vi,data=datstatusDark)
modelstatusDark

detach(metadata)

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

sink("moderators.txt")
sink()
