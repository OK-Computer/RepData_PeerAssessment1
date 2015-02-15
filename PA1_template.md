---
title: "Reproducible Research | Peer Assessment 1"
author: "OK-Computer"
date: "Saturday, February 14, 2015"
output: html_document
---

## Loading and preprocessing the data



```r
echo=TRUE
data <- read.csv("C:\\Users\\activity.csv")
```

## 1. What is mean total number of steps taken per day?

#### Step 1: Calculate the total number of steps taken per day


```r
echo=TRUE
total_steps<-aggregate(steps~date,FUN=sum,data=data,na.action=na.omit)
```

#### Step 2: Produce a histogram of the number of steps per day


```r
echo=TRUE
hist(total_steps$steps,main="Histogram of number of steps per day", xlab="Total Daily Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


#### Step 3: Calculate and report the mean and median of the total number of steps taken per day


```r
echo=TRUE
mean_steps<-mean(total_steps$steps)
print(mean_steps)
```

```
## [1] 10766.19
```


```r
echo=TRUE
median_steps<-median(total_steps$steps)
print(median_steps)
```

```
## [1] 10765
```



## 2. What is the average daily activity pattern?

#### Step 1: Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Generate a data set of average number of steps taken across all days by interval


```r
echo=TRUE
interval_steps<-aggregate(steps~interval,FUN=mean,data=data,na.action=na.omit)
```

Make the time series plot


```r
echo=TRUE
plot(interval_steps$interval,interval_steps$steps,type="l",main="Average number of steps by interval",xlab="Daily 5-minute Interval",ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 


#### Step 2: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Subset the data to pinpoint the row containing the maximum number of steps and returning the interval for that subset


```r
echo=TRUE
max_interval<-subset(interval_steps,interval_steps$steps==max(interval_steps$steps))
print(max_interval$interval)
```

```
## [1] 835
```



## 3. Imputing missing values

#### Step 1: Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
echo=TRUE
NA_cases<-data[!complete.cases(data),]
nrow(NA_cases)
```

```
## [1] 2304
```


#### Step 2 and Step 3: Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.

We will complete the missing values using the mean for that day and create a new dataset "data_2"



```r
echo=TRUE
library(plyr)
data_2<-ddply(data,
      .(date),
      transform,
      steps=ifelse(is.na(steps),is.numeric(mean(steps,na.rm=TRUE)),steps))
```

#### Step 4: Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
echo=TRUE
total_steps2<-aggregate(steps~date,FUN=sum,data=data_2,na.action=na.omit)
hist(total_steps2$steps,main="Histogram of number of steps per day",xlab="Total Daily Steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 


```r
echo=TRUE
mean_steps2<-mean(total_steps2$steps)
print(mean_steps2)
```

```
## [1] 9392
```

This mean is lower than the mean calculated in point 1, which was:


```r
echo=TRUE
print(mean_steps)
```

```
## [1] 10766.19
```



```r
echo=TRUE
median_steps2<-median(total_steps2$steps)
print(median_steps2)
```

```
## [1] 10395
```

This median is lower than the median calculated in point 1, which was:


```r
echo=TRUE
print(median_steps)
```

```
## [1] 10765
```



## 4. Are there differences in activity patterns between weekdays and weekends?

#### Step 1: Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
echo=TRUE
data_2$day<-weekdays(as.Date(data_2$date))
data_2$weekday<-ifelse(data_2$day == "Saturday" | data_2$day == "Sunday", "weekend", "weekday")
```


#### Step 2: Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
echo=TRUE
library(lattice)
interval_steps2<-aggregate(steps~interval+weekday,FUN=mean,data=data_2,na.action=na.omit)
xyplot(steps ~ interval | weekday, data=interval_steps2, type="l", grid=T, layout=c(1,2), ylab="Number of steps", xlab="Interval")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 





