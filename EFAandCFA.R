#Exploratory and confirmatory factor analysis of the Communal Naricissism Inventory
#This is to demonstrate exploratory and confirmatory factor analysis in R

#Important: put the address of the directory you're working in in the parenthesis in the next line
setwd("C:/Users/wmcla/desktop/repos/R-Code") #This is to access the directory you put the repo in
getwd()

#Obtain necessary packages
install.packages("lavaan")
install.packages("psych")
library(lavaan)
library(psych)

#Reading in the data

CNIdata <- read.csv(file="IRTdata.csv",header=TRUE)

#The CNI is a survey that measures communal narcissism, a personality trait.  It consists of 16 statements
#that the respondent rates their agreement to on a scale from 1 (Strongly Disagree) to 7 (Strongly Agree)
#No items are reversed scored.  Higher scores indicate higher levels of communal narcissism.
#The file below contains the responses of 619 respondents of a larger study to the CNI.  Each column in the dataset
#represents an item, and each row is one respondent.

#In this dataset, missing values are coded as 999, which means we must replace then with NA values to work with it in R

CNIdata[CNIdata==999]<-NA

#I know different people have different approaches, but depending on whether or not I have theoretical basis, I will run
#a few different EFAs to try to see which has the best fit indices.  Of course, parsimony, theory, and interpretability
#and utility should all be balanced when choosing and interpreting a factor structure.  

#I use maximum likelihood and a promax (non-orthaganal) rotation to ensure a more natural fit to the data.  
?fa()
EFA1 <- fa(na.omit(CNIdata),nfactors = 1,fm = "ml",rotate = "Promax")
EFA2 <- fa(na.omit(CNIdata),nfactors = 2,fm = "ml",rotate = "Promax")
EFA3 <- fa(na.omit(CNIdata),nfactors = 3,fm = "ml",rotate = "Promax")
EFA4 <- fa(na.omit(CNIdata),nfactors = 4,fm = "ml",rotate = "Promax")
EFA5 <- fa(na.omit(CNIdata),nfactors = 5,fm = "ml",rotate = "Promax")
EFA6 <- fa(na.omit(CNIdata),nfactors = 6,fm = "ml",rotate = "Promax")

#I like to compare fit indices.  My favorites to look at are TLI and RMSEA.

show(EFA1)
show(EFA2)
show(EFA3)
show(EFA4)
show(EFA5)
show(EFA6)

#Just based on fit indices, it looks like a six factor solution add little over a five factor solution.
#A five factor solution gives the best fit, but ideally I would examine the factors produced to see which has
#the strongest factor structure and no junk factors, etc., and examine which works best with known theory.

#The condundrum is that communal narcissism is supposed to be a unitary construct, and thus the one factor model should produce 
#good fit.  It fit ok, not great, with the exploratory factor analysis. Let's try a confirmatory factor analysis
#to see if a one factor model fits well enough.

attach(CNIdata)
mdl<- 'cnar =~ CNI1 + CNI2 + CNI3 + CNI4 + CNI5 + CNI6 + CNI7 + CNI8 + CNI9 + CNI10 + CNI11 + CNI12 + CNI13 + CNI14 + CNI15 + CNI16'
fit <- cfa(mdl,CNIdata)
summary(fit, fit.measures = TRUE)

#Eh, again, it leaves something to be desired.  I wouldn't consider it the best fit for this dataset. So we could say
#we failed to replicate the original research suggesting that this scale had one underlying factor, which potentially means 
#that communal narcissism is not a unitary construct (or that the CNI is just not a pure measure)