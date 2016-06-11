# Reproducible Research: Peer Assessment 1
Janice Hunnings  
June 10, 2016  

#This is an R Markdown document for Assignment 1 of Reproducible Research Coursera Course.


```
## Warning: package 'lattice' was built under R version 3.2.5
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

##Loading and Preprocessing the Data
1. Load the data.

```r
 act <- read.csv("C:/Program Files/Git/RepData_PeerAssessment1/activity/activity.csv", stringsAsFactors = FALSE)
```
2. Process/transform the date format of the data frame 

```r
 act$date <- as.Date(act$date, format = "%Y-%m-%d")
```

##What is the mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
totstep <- tapply(act$steps, act$date, sum, na.rm=TRUE) 
```
2. Make a historgram of the total number of steps taken each day

```r
hist(totstep, main = "Total Steps per Day", xlab = "Total Steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

##What is the mean and median number of steps taken per day?

```r
mean <- mean(totstep, na.rm = TRUE)
median <- median(totstep, na.rm = TRUE)
```
###The mean number of steps taken each day is: 

```
## [1] 9354.23
```
###The median number of steps taken each day is:

```
## [1] 10395
```
##Time Series plot of the average number of steps taken.  
First, compute the means of steps taken over all days by the time interval. Create a plot of the 5-minute
intervals, the mean number of steps and over all of the days.

```r
msbi <- tapply(act$steps, act$interval, mean, na.rm=TRUE)

plot(row.names(msbi), msbi, type = 'l', xlab = "5 Minute Time Intervals", 
     ylab = "Mean number of steps taken", main = "Average Number of Steps Taken by 5 Minute Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)

##The 5-Minute Interval that, on average, contains the maximum number of steps

```r
maxsteps <- which.max(msbi)
maxint <- names(maxsteps)
```
The 5-Minute interval which contains the maximum number of steps is the first number is the nth observation and the second is the actual number of max steps taken:

```
## 835 
## 104
```
##Code to describe and show a strategy for imputing missing data
1. Calculate and report the total number of missing values in the dataset (the total
number of rows with NA)

```r
naarray <- which(is.na(act))
```
The number of rows with NAs is:

```r
length(naarray)
```

```
## [1] 2304
```

2. The strategy used will be to substitute the mean value for the missing values which has the "benefit of not changing the sample mean for that variable"
http://en:wikipedia/wiki/Imputation_(statistics)#Single_imputation
This is completed by finding the location of missing values then filling them with the mean 
interval value.  After replacement, a check is made to sum the na values to validate that it is zero.

```r
replace <- msbi[as.character(act[naarray,3])]
names(replace) <- naarray
for (i in naarray) {act$steps[i] = replace[as.character(i)] }
```
The number of missing values is now: 

```
## [1] 0
```
##Histogram of the number of steps taken each day after missing values are imputed

```r
totstep2 <- tapply(act$steps, act$date, sum, na.rm=TRUE) 
hist(totstep2, main = "Total Steps per Day with Replaced NAs", xlab = "Total Steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)

##Panel Plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
wow <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
act$wday <- as.factor(sapply(act$date, FUN=wow))
str(act)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ wday    : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```
2. Make a panel plot containing a time series plot of the 5-minute interval(x-axis) and 
the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
msbi2 <- aggregate(act$steps, by=list(act$interval, act$wday),mean)
names(msbi2) <- c("interval", "wday", "steps")
xyplot(steps ~ interval | wday, msbi2, type = "l", layout = c(1,2),
       xlab = "5-Minute Interval", ylab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)
