#Instagram Selfie Ratings

setwd("C:/Users/wmcla/desktop/repos/R-Code")
getwd()

#Pulling in Zach's ratings for 1

ratings1Zach<-read.csv(file="Ratings1Zach.csv",
                       header=FALSE, # header=TRUE is the default in read.csv so this is unnecessary
                       col.names=c("Picture","Question","Questiontext","ZachRating","ZachComments1"))



attach(ratings1Zach)


#These ratings were in long format (each row is a rating), but we need to reshape them to wide format, so that each row is a selfie and each column is a rating made of that selfie.

ratings1Zachwide<-reshape(ratings1Zach,direction="wide",timevar="Question",idvar="Picture")

head(ratings1Zachwide)

#The datasets we pulled from qualtrics do not have participant IDs as the selfies, but rather a set of urls referring to the pictures on qualtrics.  
#In order to match up selfies with who they belong to, I used a separate .csv with the URLS and the participant IDs matched up.

selfieurls<-read.csv(file="selfieurls.csv",header=FALSE,col.names=c("Url","address","Subject","Pictureletter"))

#I then took a slice from the URLs file to create an ID variable for the ratings file.

ratings1Zachwide$Subject<-selfieurls$Subject[1:188]
ratings1Zachwide$Pictureletter<-selfieurls$Pictureletter[1:188]

detach(ratings1Zach)



#We repeat the process using Nevill's ratings for 1

ratings1Nevill<-read.csv(file="Ratings1Nevill.csv",
                       header=FALSE, # header=TRUE is the default in read.csv so this is unnecessary
                       col.names=c("Picture","Question","Questiontext","NevillRating","NevillComments1"))



attach(ratings1Nevill)

ratings1Nevillwide<-reshape(ratings1Nevill,direction="wide",timevar="Question",idvar="Picture")

head(ratings1Nevillwide)

ratings1Nevillwide$Subject<-selfieurls$Subject[1:188]
ratings1Nevillwide$Pictureletter<-selfieurls$Pictureletter[1:188]

#We then can repeat the above for Zach and Nevill's ratings for group 2.

#Zachs ratings for 2


ratings2Zach<-read.csv(file="Ratings2Zach.csv",
                       header=FALSE, # header=TRUE is the default in read.csv so this is unnecessary
                       col.names=c("Picture","Question","Questiontext","ZachRating","ZachComments1"))



attach(ratings2Zach)

#?reshape()
ratings2Zachwide<-reshape(ratings2Zach,direction="wide",timevar="Question",idvar="Picture")

head(ratings2Zachwide)

ratings2Zachwide$Subject<-selfieurls$Subject[189:376]
ratings2Zachwide$Pictureletter<-selfieurls$Pictureletter[189:376]
detach(ratings2Zach)

#Nevill's ratings for 2

ratings2Nevill<-read.csv(file="Ratings2Nevill.csv",
                         header=FALSE, # header=TRUE is the default in read.csv so this is unnecessary
                         col.names=c("Picture","Question","Questiontext","NevillRating","NevillComments1"))



attach(ratings2Nevill)

#?reshape()
ratings2Nevillwide<-reshape(ratings2Nevill,direction="wide",timevar="Question",idvar="Picture")

head(ratings2Nevillwide)

ratings2Nevillwide$Subject<-selfieurls$Subject[189:376]
ratings2Nevillwide$Pictureletter<-selfieurls$Pictureletter[189:376]


detach(ratings2Nevill)



#Consolidating

df1<-data.frame(ratings1Zachwide,ratings1Nevillwide)
df2<-data.frame(ratings2Zachwide,ratings2Nevillwide)

ratings<-rbind(df1,df2)


#colnames(ratings)                

#data reduction

ratingsreduced <- ratings[c(1,21,24,27,30,33,36,39,42,45,48,51,75,78,81,84,87,90,93,96,99,102,105,107:108)]
ratingsreduced[c(2:23),]<-as.numeric(sapply(ratingsreduced[c(2:23),],as.character))

detach(ratings)
attach(ratingsreduced)
#View(ratings)

narcissistic<-(ZachRating.Q2_1+NevillRating.Q2_1)/2
selfesteem<-(ZachRating.Q2_2+NevillRating.Q2_2)/2
likeable<-(ZachRating.Q2_3+NevillRating.Q2_3)/2
attractive<-(ZachRating.Q2_4+NevillRating.Q2_4)/2
status<-(ZachRating.Q2_5+NevillRating.Q2_5)/2
intelligent<-(ZachRating.Q2_6+NevillRating.Q2_6)/2
kind<-(ZachRating.Q2_7+NevillRating.Q2_7)/2
caring<-(ZachRating.Q2_8+NevillRating.Q2_8)/2
warm<-(ZachRating.Q2_9+NevillRating.Q2_9)/2
friendly<-(ZachRating.Q2_10+NevillRating.Q2_10)/2
driven<-(ZachRating.Q2_11+NevillRating.Q2_11)/2


#ICC's
library(irr)
?icc()
colnames(ratings)
#ratingscomplete<-ratings[]
icc(t(ratings7[complete.cases(ratings7[,c(8,53)]),c(8,53)]))
icc(t(ratings5[complete.cases(ratings5[,c(9,54)]),c(9,54)]))
icc(t(ratings[complete.cases(ratings[,c(10,55)]),c(10,55)]))
icc(t(ratings[complete.cases(ratings[,c(11,56)]),c(11,56)]))
icc(t(ratings7[complete.cases(ratings7[,c(12,57)]),c(12,57)]))
icc(t(ratings[complete.cases(ratings[,c(13,58)]),c(13,58)]))
icc(t(ratings[complete.cases(ratings[,c(14,59)]),c(14,59)]))
icc(t(ratings[complete.cases(ratings[,c(15,60)]),c(15,60)]))
icc(t(ratings[complete.cases(ratings[,c(16,61)]),c(16,61)]))
icc(t(ratings[complete.cases(ratings[,c(17,62)]),c(17,62)]))
icc(t(ratings[complete.cases(ratings[,c(18,63)]),c(18,63)]))
icc(t(ratings[complete.cases(ratings[,c(19,64)]),c(19,64)]))
icc(t(ratings[complete.cases(ratings[,c(20,65)]),c(20,65)]))
icc(t(ratings[complete.cases(ratings[,c(21,66)]),c(21,66)]))
icc(t(ratings[complete.cases(ratings[,c(22,67)]),c(22,67)]))
icc(t(ratings[complete.cases(ratings[,c(23,68)]),c(23,68)]))

