---
title: "Activity Monitoring"
author: "Sukalpo Mitra"
date: "Saturday, April 18, 2015"
output: html_document
---

# Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

# Data

The data for the analysis was downloaded from [here](http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) at 12:13 pm on 18th April, 2015.

Before proceeding with any R coding we set the working directory.

```{r}
setwd("D:/RWD")
```

The following piece of R code will download the data from the web link to your working directory. The link has the data in a zip format so we need to unzip the contents too. Once unzipped read the csv file into a data frame.

```{r}
## get source file from provided URL and read into variable "data"
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
url_file <- "activity_monitoring"
download.file(url, dest=url_file, mode="wb")
workdir <- getwd()
unzip (url_file, exdir = workdir)
data <- read.csv("activity.csv")
```

Here is how the structure of the data frame looks like

```{r}
str(data)
```

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset has a total of 17,568 observations in this dataset.

# What is mean total number of steps taken per day?

To answer the above question we need to do the following preprocessing of the data

```{r}
library(reshape2)
meltedData <- melt(data,id=c("date"), measure.vars=c("steps"))
castedData <- dcast(meltedData, date~variable,fun.aggregate=sum)
```

The following code now plots a histogram of the total number of steps taken each day

```{r}
hist(castedData$steps, main="Histogram of the total number of steps taken each day" , xlab="Total no of steps taken per day")
```

Mean of the total number of steps taken per day is as follows

```{r}
mean(castedData[,2],na.rm=T)
```

And median of the total number of steps taken per day is as follows

```{r}
median(castedData[,2],na.rm=T)
```

# What is the average daily activity pattern?

To answer the above question we need to do the following preprocessing of the data

```{r}
library(reshape2)
meltedData <- melt(data,id=c("date","interval"), measure.vars=c("steps"),na.rm = T)
castedData <- dcast(meltedData, interval~variable,fun.aggregate=mean)
```

The following code now plots a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
plot(castedData$interval,castedData$steps,type="n",xlab="5 minute interval",ylab="Average number of steps taken, averaged across all days")
lines(castedData$interval,castedData$steps,type="l")
```

The 5-minute interval, that on average across all the days in the dataset, contains the maximum number of steps is

```{r}
castedData[which(castedData$steps == max(castedData[,2])),1]
```

# Inputing missing values

Total number of missing values in the dataset (i.e. the total number of rows with NAs) is

```{r}
sum(is.na(data[,1]))
```

We had used mean of the number of steps for the 5 minute intervals to fill in missing data in the dataset. We first create temp data frame from data. Put all the NA values in the temp data frame as -1 and then plug in the means in those -1 rows having same 5 minute intervals as castedData that we calculated earlier and create a new data frame. We then remove the temporary one.


```{r}
library(DBI)
library(RSQLite)
library(proto)
library(gsubfn)
library(sqldf)
library(tcltk)
tempData <- data
tempData[which(is.na(tempData$steps)),1] = -1
tempData <- sqldf("select tempData.date,tempData.interval,castedData.steps from tempData left outer join castedData on  (tempData.interval = castedData.interval and tempData.steps = '-1')")
dataWithNoMissingValues <- data
dataWithNoMissingValues[which(is.na(dataWithNoMissingValues[,1])),1] <- tempData[which(!is.na(tempData[,3])),3]
rm(tempData)
```

Now to calculate the total no of steps taken each day on this ne dataset the following preprocessing needs to be done.

```{r}
library(reshape2)
meltedData <- melt(dataWithNoMissingValues,id=c("date"), measure.vars=c("steps"))
castedData <- dcast(meltedData, date~variable,fun.aggregate=sum)
```

The following code now plots a histogram of the total number of steps taken each day

```{r}
hist(castedData$steps, main="Histogram of the total number of steps taken each day" , xlab="Total no of steps taken per day")
```

Mean of the total number of steps taken per day is as follows

```{r}
mean(castedData[,2],na.rm=T)
```

And median of the total number of steps taken per day is as follows

```{r}
median(castedData[,2],na.rm=T)
```

As noticed the mean and median became same after inputting the missing values

# Are there differences in activity patterns between weekdays and weekends?

Firstly we need to convert the date column into a date class

```{r}
dataWithNoMissingValues[,2] <- as.Date(dataWithNoMissingValues[,2],"%Y-%m-%d")
```

We now add a new column that will hold a factor variable viz. Weekday or Weekend

```{r}
library(dplyr)
dataWithNoMissingValues <- mutate(dataWithNoMissingValues,dayOfWeek = weekdays(date))
dataWithNoMissingValues[which(dataWithNoMissingValues$dayOfWeek == "Sunday"),4] <- 1
dataWithNoMissingValues[which(dataWithNoMissingValues$dayOfWeek == "Saturday"),4] <- 1
dataWithNoMissingValues[which(dataWithNoMissingValues$dayOfWeek != "1"),4] <- 0
dataWithNoMissingValues[,4] <- as.numeric(dataWithNoMissingValues[,4])
dataWithNoMissingValues[,4] <- factor(dataWithNoMissingValues[,4],levels=c(0,1), labels=(c("weekday","weekend")))
```

Now we do some more processing of the data 

```{r}
meltedData <- melt(dataWithNoMissingValues,id=c("dayOfWeek","interval"), measure.vars=c("steps"),na.rm = T)
castedData <- dcast(meltedData, dayOfWeek + interval~variable,fun.aggregate=mean)
```
Now to plot the data
```{r}
library(lattice)
xyplot(steps~interval|dayOfWeek,data=castedData, layout=c(1,2), type="l",xlab="Interval",ylab="Number of steps")
```


