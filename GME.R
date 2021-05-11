#Using ML to draw regression model of how WallStreetBets drove price of GME

# Setup

setwd("C:/Users/aarth/Downloads/R/GME_Analysis")

library(dplyr)
library(tidyverse)
library(readr)



dataset1 <- read_csv("reddit_wsb.csv",col_types = cols(.default = "c"))
dataset1$title = gsub(pattern = "\\,",".",dataset1$title)
dataset1$body  = gsub(pattern = "\\,",".",dataset1$body)
dataset1$title = gsub(pattern = "\\|",".",dataset1$title)
dataset1$body  = gsub(pattern = "\\|",".",dataset1$body)


GME<-read.table(unz("archive.zip","GME.csv"), header=T,quote="\"",sep=",")


wsb<-dataset1

#1. Regression Fit against Subreddit Daily Thread Volume

GME$Date<-as.Date(GME$Date,"%Y-%m-%d")  
wsb$X<-as.Date(wsb$timestamp,"%m/%d/%Y")

#Histogram
hist(wsb$X,100)
freq<-data.frame(table(wsb$X))
freq$Var1<-as.Date(freq$Var1,"%Y-%m-%d")
text<-wsb[,1]
colnames(freq)<-c("Date","Freq")
GME<-dplyr::left_join(GME,freq,by="Date")
GME$Freq[is.na(GME$Freq)] <- 0

summary(lm(GME$High~GME$Freq,data=GME))



#2. Regression Fit against DeepFuckingValue's Posts



GME<-read.table(unz("archive.zip","GME.csv"), header=T,quote="\"",sep=",")

GME$Date<-as.Date(GME$Date,"%Y-%m-%d")  
m<-str_which(wsb$title,"GME YOLO update")
m<-append(m,str_which(wsb$title,"GME YOLO month-end update"))
wsb$score<-as.numeric(wsb$score)

dfv<-filter(wsb[c(m),],score>10000)
dfv$X<-as.Date(dfv$timestamp,"%m/%d/%Y")-1
freq<-data.frame(table(dfv$X))
freq$Var1<-as.Date(freq$Var1,"%Y-%m-%d")
colnames(freq)<-c("Date","Freq")
n<-data.frame(c("2021-02-01","2021-02-02","2021-02-03","2021-01-19","2021-01-14","2021-01-11","2021-01-05","2021-01-22","2021-01-25","2021-01-26","2021-01-27"),c("1","1","1","1","1","1","1","1","1","1","1"))
colnames(n)<-c("Date","Freq")
freq<-rbind(freq,n)
GME<-dplyr::left_join(GME,freq,by="Date")

GME$Freq[is.na(GME$Freq)] <- 0

summary(lm(GME$High~GME$Freq,data=GME))




#3. Regression Fit against GME posts



GME<-read.table(unz("archive.zip","GME.csv"), header=T,quote="\"",sep=",")

GME$Date<-as.Date(GME$Date,"%Y-%m-%d")  


m<-str_which(wsb$title,"GME")
m<-append(m,str_which(wsb$title,"GameStop"))
wsb_gamestop<-wsb[c(m),]
freq<-data.frame(table(wsb_gamestop$X))
freq$Var1<-as.Date(freq$Var1,"%Y-%m-%d")
colnames(freq)<-c("Date","Freq")
GME<-dplyr::left_join(GME,freq,by="Date")

GME$Freq[is.na(GME$Freq)] <- 0

summary(lm(High~Freq,data=GME))

plot(GME$Freq, GME$High)
abline(lm(High~Freq,data=GME))

#4. Regression Fit against Musk's Tweet
 
GME$Musk<-0
GME$Musk[GME$Date=="2021-01-26"]<-1

summary(lm(High~Freq+Musk,data=GME))
GME$Change<-((GME$Close-GME$Open)/GME$Open)*100
summary(lm(Change~Freq+Musk,data=GME))

plot(GME$Musk,GME$Change)
abline(lm(Change~Musk,data=GME))
