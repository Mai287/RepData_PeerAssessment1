---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data


```r
library(datasets)
library(ggplot2)
```

```r
activity <- read.csv("activity.csv")
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
activity$date = as.Date(activity$date,format="%Y-%m-%d")
daily_data =aggregate(activity$steps, by=list(activity$date),FUN =sum, na.rm=TRUE)
colnames(daily_data) <- c("date","dailysteps")
summary(daily_data)
```

```
##       date              dailysteps   
##  Min.   :2012-10-01   Min.   :    0  
##  1st Qu.:2012-10-16   1st Qu.: 6778  
##  Median :2012-10-31   Median :10395  
##  Mean   :2012-10-31   Mean   : 9354  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```
## What is mean total number of steps taken per day?
### Mean total number of steps taken per day

```r
mean_daily_steps=mean(daily_data$dailysteps)
mean_daily_steps
```

```
## [1] 9354.23
```
### Median of the total number of steps taken per day

```r
median_daily_steps= median(daily_data$dailysteps)
median_daily_steps
```

```
## [1] 10395
```
### Histogram of total number of steps taken each day

```r
hist(daily_data$dailysteps,main =" Total number of steps taken each day", xlab = "Daily steps",ylim=c(0,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## What is the average daily activity pattern?
### Time series plot of the 5-minute interval and the average number of steps taken

```r
interval_data = aggregate(activity$steps, by=list(activity$interval),FUN =mean, na.rm=TRUE)
colnames(interval_data)= c("interval", "steps.mean")
plot(interval_data$interval, interval_data$steps.mean, xlab="interval", ylab=" average steps", main= "Average daily activity pattern", type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Which 5-minute interval , on average across all the days, in the dataset, contains the maximum number of steps

```r
max_steps_interval = interval_data[which.max(interval_data$steps.mean),1]
max_steps_interval
```

```
## [1] 835
```

## Imputing missing values
### Total number of missing values in the dataset

```r
number_na_value = sum(is.na(activity$steps))
number_na_value
```

```
## [1] 2304
```

### Filling missing value by using the median of number of steps for that interval

```r
new_data= activity
for (j in new_data$interval){
  new_data[is.na(new_data$steps) & new_data$interval== j,"steps"]= median(new_data[new_data$interval==j,"steps"],na.rm = TRUE)
}
```

### New histogram of the total number of steps taken each day

```r
hist(tapply(new_data$steps, new_data$date,sum),main =" Total number of steps taken each day", xlab = "Daily steps",ylim=c(0,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Mean and median total number of steps taken per day

```r
summary(tapply(new_data$steps, new_data$date,sum))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    6778   10395    9504   12811   21194
```

## Are there differences in activity patterns between weekdays and weekends?

### Create new factor variable indicating whether a given date is a weekday or weekend day

```r
day <- weekdays(new_data$date)
day<- factor(day, levels=c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), labels=c("weekday","weekday","weekday","weekday","weekday","weekend","weekend"))
new_data<- cbind(new_data,day)
```

### Create a panel plot comparing weekday days and weekend days

```r
library(lattice)
new_interval_data = aggregate(new_data$steps, by=list(new_data$interval,new_data$day),FUN =mean, na.rm=TRUE)
colnames(new_interval_data)= c("interval","day", "steps.mean")
xyplot(steps.mean~interval|day, data = new_interval_data,type="l", layout=c(ncol=1,nrow=2),ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
