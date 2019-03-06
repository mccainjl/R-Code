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

selfieurls<-read.csv(file="selfieurls.csv",header=FALSE,col.names=c("Url","Subject","Pictureletter"))

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

detach(ratings1Nevill)
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



#Consolidating into one dataframe

df1<-data.frame(ratings1Zachwide,ratings1Nevillwide)
df2<-data.frame(ratings2Zachwide,ratings2Nevillwide)

ratings<-rbind(df1,df2)

#data reduction down to just the ratings

ratingsreduced <- ratings[c(1,21,24,27,30,33,36,39,42,45,48,51,75,78,81,84,87,90,93,96,99,102,105,107:108)]

detach(ratings)
attach(ratingsreduced)
#View(ratings)

#Calculating mean scores of each rating by averaging the ratings from both judges

ratingsreduced$narcissistic<-rowMeans(sapply(ratingsreduced[c(2,13),],as.numeric),na.rm = TRUE)
ratingsreduced$selfesteem<-rowMeans(sapply(ratingsreduced[c(3,14),],as.numeric),na.rm = TRUE)
ratingsreduced$likeable<-rowMeans(sapply(ratingsreduced[c(4,15),],as.numeric),na.rm = TRUE)
ratingsreduced$attractive<-rowMeans(sapply(ratingsreduced[c(5,16),],as.numeric),na.rm = TRUE)
ratingsreduced$status<-rowMeans(sapply(ratingsreduced[c(6,17),],as.numeric),na.rm = TRUE)
ratingsreduced$intelligent<-rowMeans(sapply(ratingsreduced[c(7,18),],as.numeric),na.rm = TRUE)
ratingsreduced$kind<-rowMeans(sapply(ratingsreduced[c(8,19),],as.numeric),na.rm = TRUE)
ratingsreduced$caring<-rowMeans(sapply(ratingsreduced[c(9,20),],as.numeric),na.rm = TRUE)
ratingsreduced$warm<-rowMeans(sapply(ratingsreduced[c(10,21),],as.numeric),na.rm = TRUE)
ratingsreduced$friendly<-rowMeans(sapply(ratingsreduced[c(11,22),],as.numeric),na.rm = TRUE)
ratingsreduced$driven<-rowMeans(sapply(ratingsreduced[c(12,23),],as.numeric),na.rm = TRUE)

#Now our dataset of ratings is ready to be merged with and correlated to the self-reported personality data in the other dataset (not included)
