---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First, I decompressed the activity.zip using the file archiver software 7-Zip.  
Second, I load the decompressed activity.csv file to R by read.csv().

```r
rawData = read.csv("activity.csv")
```

Here is the overview of the data loaded:

```r
str(rawData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean and median total number of steps taken per day?
First, I preprocessing the data using aggregate() function.  
Here is the total number of steps taken per day with NA removed.

```r
totalStepsPerDay <- aggregate(steps ~ date, data=rawData, FUN=sum, na.rm=TRUE)
head(totalStepsPerDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

Here is the histogram of the total number of steps taken per day.

```r
hist(totalStepsPerDay$steps, breaks=22,
     main="Total number of steps taken per day",
     xlab="Total Steps", ylab="Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Here is the mean.

```r
mean(totalStepsPerDay$steps)
```

```
## [1] 10766.19
```
Here is the median.

```r
median(totalStepsPerDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
First, I preprocessing the data using aggregate() function.  
Here is the average number of steps taken in each 5-minute interval with NA removed.

```r
avgDailyPattern <- aggregate(steps ~ interval, data=rawData, FUN=mean, na.rm=TRUE)
head(avgDailyPattern)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

Here is time series plot of the average number of steps taken

```r
plot(avgDailyPattern$interval, avgDailyPattern$steps, type="l",
     main="The average number of steps taken",
     xlab="Interval (minutes)", ylab="Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Here we can see which 5-minute interval on average contains the maximum number of steps.

```r
avgDailyPattern$interval[avgDailyPattern$steps == max(avgDailyPattern$steps)]
```

```
## [1] 835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values. The presence of missing days may introduce bias into some calculations or summaries of the data.  
The total number of missing values in the dataset:

```r
sum(is.na(rawData$steps))
```

```
## [1] 2304
```

I use the mean for that 5-minute interval for filling in all of the missing values in the dataset.

```r
# Merge rawData and avgDailyPattern
noNAData <- merge(rawData, avgDailyPattern, by="interval")
# Order data by date and interval since it is messed up in merging
noNAData <- noNAData[order(noNAData$date,noNAData$interval),]
# Find the raw that have NA
haveNA <- is.na(rawData$steps)
# Fill NA with the mean for that 5-minute interval
noNAData[haveNA,]$steps.x <- noNAData[haveNA,]$steps.y
# Remove the temporary column with the mean for that 5-minute interval
noNAData <- noNAData[,1:3]

# Summary of new Data
summary(noNAData)
```

```
##     interval         steps.x           date          
##  Min.   :   0.0   Min.   :  0.00   Length:17568      
##  1st Qu.: 588.8   1st Qu.:  0.00   Class :character  
##  Median :1177.5   Median :  0.00   Mode  :character  
##  Mean   :1177.5   Mean   : 37.38                     
##  3rd Qu.:1766.2   3rd Qu.: 27.00                     
##  Max.   :2355.0   Max.   :806.00
```

Let plot the histogram of the total number of steps taken each day again with NA filled in

```r
newTotalStepsPerDay <- aggregate(steps.x ~ date, data=noNAData, FUN=sum)
hist(newTotalStepsPerDay$steps.x, breaks=22,
     main="Total number of steps taken per day (NA filled in)",
     xlab="Total Steps", ylab="Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Here is the new mean.

```r
mean(newTotalStepsPerDay$steps)
```

```
## [1] 10766.19
```
Here is the new median.

```r
median(newTotalStepsPerDay$steps)
```

```
## [1] 10766.19
```

Recall the orginal data.  
Here is the orginal mean.

```r
mean(totalStepsPerDay$steps)
```

```
## [1] 10766.19
```
Here is the orginal median.

```r
median(totalStepsPerDay$steps)
```

```
## [1] 10765
```

These values differ from the estimates from the orginal data. Now, the mean and median of the data is same.

## Are there differences in activity patterns between weekdays and weekends?
First, I preprocessing the data and add a factor to weektype

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
# Add a factor to weektype
noNAData$weekdays <- wday(ymd(noNAData$date))
noNAData$weektype <- noNAData$weekdays == 1 | noNAData$weekdays == 7
noNAData$weektype <- factor(noNAData$weektype,  levels=c(FALSE, TRUE), 
                            labels=c("Weekdays", "Weekends"))

# Calculate the average number of steps taken in each 5-minute interval
newAvgDailyPattern <- aggregate(steps.x ~ interval + weektype, data=noNAData, FUN=mean)
```

Here is time series plot of the average number of steps taken

```r
library(lattice)
xyplot(steps.x ~ interval | weektype, newAvgDailyPattern, type="l", 
       layout=c(1,2), xlab="Interval (minutes)", ylab="Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
