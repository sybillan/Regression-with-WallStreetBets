Regression with WSB
================

``` r
#Using ML to draw regression model of how WallStreetBets drove price of GME

# Setup

setwd("C:/Users/aarth/Downloads/R/GME_Analysis")

library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.5

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.1     v stringr 1.4.0
    ## v tidyr   1.1.3     v forcats 0.5.1
    ## v readr   1.4.0

    ## Warning: package 'ggplot2' was built under R version 4.0.5

    ## Warning: package 'tibble' was built under R version 4.0.5

    ## Warning: package 'tidyr' was built under R version 4.0.5

    ## Warning: package 'readr' was built under R version 4.0.5

    ## Warning: package 'forcats' was built under R version 4.0.5

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
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
```

![](REGRESSION-WITH-WSB_files/figure-gfm/1-1.png)<!-- -->

``` r
freq<-data.frame(table(wsb$X))
freq$Var1<-as.Date(freq$Var1,"%Y-%m-%d")
text<-wsb[,1]
colnames(freq)<-c("Date","Freq")
GME<-dplyr::left_join(GME,freq,by="Date")
GME$Freq[is.na(GME$Freq)] <- 0

summary(lm(GME$High~GME$Freq,data=GME))
```

    ## 
    ## Call:
    ## lm(formula = GME$High ~ GME$Freq, data = GME)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -125.37  -91.13   15.73   47.17  318.26 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 1.434e+02  9.990e+00  14.359  < 2e-16 ***
    ## GME$Freq    1.779e-02  5.597e-03   3.179  0.00208 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 89.28 on 83 degrees of freedom
    ## Multiple R-squared:  0.1085, Adjusted R-squared:  0.09778 
    ## F-statistic:  10.1 on 1 and 83 DF,  p-value: 0.00208
