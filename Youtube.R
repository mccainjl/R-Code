#Youtube Ratings - shows data management combining researcher ratings of Youtube videos with participant-reported ratings of the same videos. 

ratings1Kristina<-read.csv(file="C:/Users/Jessica/OneDrive/Youtube/KristinaRatings.csv",
                       header=TRUE) # header=TRUE is the default in read.csv so this is unnecessary
                       
ratings1Connor<-read.csv(file="c:/Users/Jessica/OneDrive/Youtube/ConnorRatings.csv",header=TRUE)

ratings2Alex<-read.csv(file="c:/Users/Jessica/OneDrive/Youtube/AlexRatings.csv",header=TRUE)

ratings2Joseph<-read.csv(file="c:/Users/Jessica/OneDrive/Youtube/JosephRatings.csv",header=TRUE)

#Combine like

ratings1<-cbind(ratings1Kristina,ratings1Connor)
head(ratings1)
tail(ratings1)
?cbind()
ratings2<-cbind(ratings2Alex,ratings2Joseph)
head(ratings2)
ratings<-rbind(ratings1,ratings2)
head(ratings)
tail(ratings)

write.table(ratings,file="C:/Users/Jessica/OneDrive/Youtube/Ratings.csv",sep=",")

ratings<-read.csv(file="C:/Users/Jessica/OneDrive/Youtube/Ratings.csv")

#fulldataset construction

surveyresults<-read.csv(file="c:/Users/Jessica/OneDrive/Youtube/YoutubeStudyMaster.csv",header=TRUE)

?merge()
YoutubeMaster<-merge(surveyresults,ratings,by.x="FaveofList",by.y="Rank",all=TRUE)
head(YoutubeMaster)
write.table(YoutubeMaster,file="C:/Users/Jessica/OneDrive/Youtube/YoutubeMaster.csv",sep=",")
