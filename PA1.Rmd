---
title: "Untitled"
author: "zbijoux"
date: "October 15, 2015"
output: html_document
---
# Reproducible Research Peer Assessment 1

## Introduction
The purpose of this assignment is to write a single R markdown document that can be processed by knitr and transformed into an HTML file. This R markdown document contain results from Peer Assessment 1 in the Rerpoducible Rsearch course in Cousera.

## Data
The dataset consists of 17,568 observations and 3 variables that is steps,date and interval. The data was collected over two months period from October to November in the year of 2012. The data was measured using a personal activity monitoring device measuring the steps of individual after every 5 minutes each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data][1] [52k]

[1]: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip "Activity monitoring data"

## loading Packages

```{r echo=TRUE}

library(dplyr)
library(lubridate)
library(ggplot2)

```


## Loading and preprocessing the data

**1. Loading and Reading data**

```{r echo=TRUE}
setwd("~/COURSERA/RR/data")
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}

activity<-read.csv("activity.csv")

act<- tbl_df(activity)

```

**2. Process and transforming the data**

```{r echo=TRUE}
act<-act%>%
  mutate(date1=ymd(date))%>%
  print()

str(act)
dim(act)

```

## What is the mean total number of steps taken per day?

**1. Calculating the total number of steps taken per day**

```{r echo=TRUE}
act%>%
  group_by(date)%>%
  summarise(sum(steps))%>%
  print()

```

**2. Plotting a histogram of the total number of steps taken each day**
```{r echo=TRUE}
actstep<-act%>%
  filter(!is.na(steps))%>%
  group_by(date)%>%
  summarise(steps=sum(steps))%>%
  print()

hist(actstep$steps, 
     breaks=seq(from=0, to=25000, by=2500),
     col="green", 
     xlab="Total number of steps", 
     ylim=c(0, 18), 
     main="Histogram of total number of steps per day")

```

**3. Calculating the mean and median total number of steps taken per day**

```{r echo=TRUE}

summarise(actstep,meanstep=mean(steps),medianstep=median(steps))

```

 * The mean is 10766.19 and the median is 10765
 
## What is the average daily activity pattern?

**1. Time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

    a) Data Manipulation
```{r echo=TRUE}

actint <- act %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))%>%
  print()
  
  dim(actint)
  str(actint)

```

  b) Plotting the time series plot
```{r echo=TRUE}  
  ggplot(actint, aes(x=interval, y=steps)) +
  geom_line(color = "blue")+
  xlab("interval in 5 minutes") + 
  ylab("avarage number of steps") +
  ggtitle("Time-series of average number of steps per intervals")
  
```


**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```{r}
  
    actint[which.max(actint$steps),]
  
```

## Imputing missing values

**1. Calculate and report the total number of missing values in the dataset**

```{r echo=TRUE}
  
   sum(is.na(act))
  colSums(is.na(act))

```
 
 * The number of missing value is 2304
 
**2. Devise a strategy for filling in all of the missing values in the dataset**

The strategy use for filling in the missing values or replacing the missing values in the dataset will be to calculate the means at each 5 minutes interval.

**3. Creating a new dataset equal to the original dataset but with the missing data filled in.**

```{r echo=TRUE}
  
  impdat <- act
  misdat <- is.na(impdat$steps)
  avgbyinter <- tapply(impdat$steps,impdat$interval, mean, na.rm=TRUE, simplify = TRUE)
  impdat$steps[misdat] <- avgbyinter[as.character(impdat$interval[misdat])]
   
  head(impdat)
  
```  

Checking for missing values

```{r echo=TRUE}
    
  sum(is.na(impdat$steps))
  colSums(is.na(impdat))

```

* No missing values

**4. Make a histogram of the total number of steps taken each day**

```{r echo=TRUE}
  datstep <- impdat %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print()
  
  ggplot(datstep, aes(x = steps)) +
  geom_histogram(fill = "green", binwidth = 600) +
  labs(title = "Histogram of Steps per day, with missing values", x = "Total Steps per day (Imputed)", y = "Frequency binwidth = 600")

```


 **Calculate and report the mean and median total number of steps taken per day.**
 
```{r echo=TRUE}
   meanstep<-mean(datstep$steps)
   medianstep<-median(datstep$steps)

```

* Mean: 10766
* Median: 10766

The values do not differ from the estimates from the first part of the assignment. 

The impact of imputing missing is that both the mean and the median are equal. But there seems to be no much impact.

## Are there differences in activity patterns between weekdays and weekends?

**1. Creating a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**

```{r echo=TRUE}
   
   impdat$weektype <- ifelse(wday(impdat$date1) %in% c(0,6), "weekend", "weekday")

```

**2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken**

```{r echo=TRUE}
   avgimpdat<- aggregate(steps ~ interval + weektype, data=impdat, mean)

   ggplot(avgimpdat, aes(interval, steps, color = weektype)) + 
    geom_line() + 
    facet_grid(weektype ~ .) +
    xlab("5-minute interval") + 
    ylab("Avarage number of steps")
   
```



 
