---
title: "Reproducible Research: Peer Assessment 1"
author: "Samuel Tandoh"
date: "2/8/2021"
output: 
  html_document:
    keep_md: true
---

---

## Loading and preprocessing the data
###  Show any code that is needed to
1. _Load the data (i.e. read.csv())_
2. _Process/transform the data (if necessary) into a format suitable for your analysis_


```r
unzip(zipfile="activity.zip")
activity <- read.csv("activity.csv")
```

* Loading libaries

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

* Information about data variables

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
From the output, there are 17568 observations with 3 variables included in this dataset are:

 1. **_steps_** : Number of steps taking in a 5-minute interval (missing values are coded as NA)
 2. **_date_**  : The date on which the measurement was taken in YYYY-MM-DD format
 3. **_interval_**: Identifier for the 5-minute interval in which measurement was taken



## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. _Calculate the total number of steps taken per day_
2. _Make a histogram of the total number of steps taken each day_
3. _Calculate and report the mean and median total number of steps taken per day_

**1. Number of steps per day**


```r
tot.steps_perday <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(tot.steps_perday) <- c("Date", "Steps")
tot.steps_perday
```

```
##          Date Steps
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01    NA
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04    NA
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14    NA
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30    NA
```

**2. Histogram of the total number of steps taken each day**


```r
g <- ggplot(tot.steps_perday, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="black", fill="salmon")+
    ggtitle("Histogram of Steps per Day")+
    xlab("Steps")+
    ylab("Frequency")+
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=12))+
    scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,20,2))
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

**3. Mean and median of total number of steps taken per day**

```r
round(mean(tot.steps_perday$Steps, na.rm = TRUE))
```

```
## [1] 10766
```


```r
median(tot.steps_perday$Steps, na.rm = TRUE)
```

```
## [1] 10765
```


The mean and median of the total steps taken per day are **_10766_** and **_10765_** respectively


## **What is the average daily activity pattern?** 

1.  _Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number      of steps taken, averaged across all days (y-axis)_
2.  _Which 5-minute interval, on average across all the days in the dataset, contains the maximum number      of steps?_

**1. Time series plot of the 5 minute interval (x) and averaged number of steps taken averaged across all      days (y)**

```r
steps_per.time <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)

steps_per.time$time <- steps_per.time$interval/100

h <- ggplot(steps_per.time, aes(time, steps))
h+geom_line(col="blue")+
    ggtitle("Average steps per time interval")+
    xlab("Time")+
    ylab("Steps")+theme(plot.title = element_text(hjust = 0.5, face="bold", size=12))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->




## **Imputing missing values**

**1. calculating the total number of missing values in the dataset**


```r
activity2 <- tibble::as_tibble(activity)

activity2 %>% 
  filter(is.na(steps)) %>% 
  summarize(missing_values = n())
```

```
## # A tibble: 1 x 1
##   missing_values
##            <int>
## 1           2304
```
As we can see,  the original data set has 2304 rows with missing data.


**2. Replace missing values**

We use the mean for that 5-minute interval to replace all missing values.We create a new column       without missing values called _full_steps_

```r
activity$full_steps <- ifelse(is.na(activity$steps), round(steps_per.time$steps[match(activity$interval, steps_per.time$interval)],0), activity$steps)
```


**3. Creating new dataset that is equal to the original dataset but with the missing data filled in**


```r
activity_comp <- data.frame(steps=activity$full_steps, interval=activity$interval, date=activity$date)
# see first 10 values of the new dataset
head(activity_comp, n=10)
```

```
##    steps interval       date
## 1      2        0 2012-10-01
## 2      0        5 2012-10-01
## 3      0       10 2012-10-01
## 4      0       15 2012-10-01
## 5      0       20 2012-10-01
## 6      2       25 2012-10-01
## 7      1       30 2012-10-01
## 8      1       35 2012-10-01
## 9      0       40 2012-10-01
## 10     1       45 2012-10-01
```

**4a. Histogram of the total number of steps taken each day with missing data filled in**

```r
steps_perday_comp <- aggregate(activity_comp$steps, list(activity_comp$date), FUN=sum)
colnames(steps_perday_comp) <- c("Date", "Steps")

# draw the histogram
g <- ggplot(steps_perday_comp, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="black", fill="lightgreen")+
  ggtitle("Histogram of steps per day")+
  xlab("Steps")+ylab("Frequency")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=12))+
  scale_x_continuous(breaks=seq(0,25000,2500))+
  scale_y_continuous(breaks=seq(0,26,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

**4b. Calculate and report the mean and median total number of steps taken per day.** 
    **Do these values differ from the estimates from the first part of the assignment?**
    **What is the impact of imputing missing data on the estimates of the total daily number of steps?**

```r
round(mean(steps_perday_comp$Steps))
```

```
## [1] 10766
```



```r
median(steps_perday_comp$Steps)
```

```
## [1] 10762
```
Based on the imputed data set, the new mean is **_10766_** and the new median is **_10762_** .
Comparing this with the original mean 10766 and median 10765 , the mean doesn't change, and the median has a small change.This could be attributed to the filling of the missing data using means for intervals, so we have more data close or identical to the means, and median is shifted and becomes identical to the mean.
The impact of imputing missing data on the estimates of the total daily number of steps is also clear: now we have higher frquency counts in the histogram at the center region (close to the mean).


## **Are there differences in activity patterns between weekdays and weekends?**

**1. Creating a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.**


```r
activity_comp$RealDate <- as.Date(activity_comp$date, format = "%Y-%m-%d")
# create a variable with weekdays name
activity_comp$weekday <- weekdays(activity_comp$RealDate)
# create a new variable indicating weekday or weekend
activity_comp$day_type <- ifelse(activity_comp$weekday=='Saturday' | activity_comp$weekday=='Sunday', 'weekend','weekday')
# see first 10 values
head(activity_comp, n=10)
```

```
##    steps interval       date   RealDate weekday day_type
## 1      2        0 2012-10-01 2012-10-01  Monday  weekday
## 2      0        5 2012-10-01 2012-10-01  Monday  weekday
## 3      0       10 2012-10-01 2012-10-01  Monday  weekday
## 4      0       15 2012-10-01 2012-10-01  Monday  weekday
## 5      0       20 2012-10-01 2012-10-01  Monday  weekday
## 6      2       25 2012-10-01 2012-10-01  Monday  weekday
## 7      1       30 2012-10-01 2012-10-01  Monday  weekday
## 8      1       35 2012-10-01 2012-10-01  Monday  weekday
## 9      0       40 2012-10-01 2012-10-01  Monday  weekday
## 10     1       45 2012-10-01 2012-10-01  Monday  weekday
```

```r
str(activity_comp)
```

```
## 'data.frame':	17568 obs. of  6 variables:
##  $ steps   : num  2 0 0 0 0 2 1 1 0 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ RealDate: Date, format: "2012-10-01" "2012-10-01" ...
##  $ weekday : chr  "Monday" "Monday" "Monday" "Monday" ...
##  $ day_type: chr  "weekday" "weekday" "weekday" "weekday" ...
```


**2. A panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**


```r
steps_per_timedf <- aggregate(steps~interval+day_type,data=activity_comp,FUN=mean,na.action=na.omit)

steps_per_timedf$time <- steps_per.time$interval/100
# draw the line plot
z <- ggplot(steps_per_timedf, aes(time, steps))
z+geom_line(col="blue")+
  ggtitle("Average Steps Per Time Interval: Weekdays vs. Weekends")+
  xlab("Time")+
  ylab("Steps")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=12))+
  facet_grid(day_type ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


