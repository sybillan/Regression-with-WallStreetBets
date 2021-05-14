<<<<<<< HEAD
---
title: "GME Regression Model"
output: html_document
---


Per MLane@Kaggle, Wall street bets data for the column "title", has rare error that it contains punctuation ",", ";", ".", "/", "\", and worst "|". The "|" is the US convention for sharing deliminated files with known punctuation errors like this one. So we have to pick the one "deliminator that we want to use and then eliminate the deliminator form every column regardless of if we think it should be there. In R the function to do this is "gsub".

```{r setup, include=TRUE, message=FALSE,warning=FALSE}
#Using ML to draw regression model of how the users of subreddit WallStreetBets drove price of GME in 2021
#Inital Setup



library(dplyr)
library(tidyverse)
library(readr)

dataset1 <- read_csv("reddit_wsb.csv",col_types = cols(.default = "c"))
dataset1$title = gsub(pattern = "\\,",".",dataset1$title)
dataset1$body  = gsub(pattern = "\\,",".",dataset1$body)

GME<-read.table(unz("archive.zip","GME.csv"), header=T,quote="\"",sep=",")


wsb<-dataset1

```

Regression Fit Against daily frequency posts that mention GME/GameStop
```{r}


GME<-read.table(unz("archive.zip","GME.csv"), header=T,quote="\"",sep=",")

GME$Date<-as.Date(GME$Date,"%Y-%m-%d")  

wsb$X<-as.Date(wsb$timestamp,"%m/%d/%Y")

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
```

Regression Fit Against Musk's tweet that mention GME/GameStop

```{r}

GME$Musk<-0
GME$Musk[GME$Date=="2021-01-26"]<-1

summary(lm(High~Freq+Musk,data=GME))
GME$Change<-((GME$Close-GME$Open)/GME$Open)*100
summary(lm(Change~Freq+Musk,data=GME))

plot(GME$Musk,GME$Change)
abline(lm(Change~Musk,data=GME))

```

=======
---
title: "GME Regression Model"
output: html_document
---


Per MLane@Kaggle, Wall street bets data for the column "title", has rare error that it contains punctuation ",", ";", ".", "/", "\", and worst "|". The "|" is the US convention for sharing deliminated files with known punctuation errors like this one. So we have to pick the one "deliminator that we want to use and then eliminate the deliminator form every column regardless of if we think it should be there. In R the function to do this is "gsub".

```{r setup, include=TRUE, message=FALSE,warning=FALSE}
#Using ML to draw regression model of how the users of subreddit WallStreetBets drove price of GME in 2021
#Inital Setup



library(dplyr)
library(tidyverse)
library(readr)

dataset1 <- read_csv("reddit_wsb.csv",col_types = cols(.default = "c"))
dataset1$title = gsub(pattern = "\\,",".",dataset1$title)
dataset1$body  = gsub(pattern = "\\,",".",dataset1$body)

GME<-read.table(unz("archive.zip","GME.csv"), header=T,quote="\"",sep=",")


wsb<-dataset1

```

Regression Fit Against daily frequency posts that mention GME/GameStop
```{r}


GME<-read.table(unz("archive.zip","GME.csv"), header=T,quote="\"",sep=",")

GME$Date<-as.Date(GME$Date,"%Y-%m-%d")  

wsb$X<-as.Date(wsb$timestamp,"%m/%d/%Y")

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
```

Regression Fit Against Musk's tweet that mention GME/GameStop

```{r}

GME$Musk<-0
GME$Musk[GME$Date=="2021-01-26"]<-1

summary(lm(High~Freq+Musk,data=GME))
GME$Change<-((GME$Close-GME$Open)/GME$Open)*100
summary(lm(Change~Freq+Musk,data=GME))

plot(GME$Musk,GME$Change)
abline(lm(Change~Musk,data=GME))

```

>>>>>>> 2d9a20a2d07661037f99573d783bfe941998c216
