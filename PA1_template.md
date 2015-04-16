# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", sep=",")
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
numOfStepsPerDay <- aggregate(steps~date,data,sum)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(numOfStepsPerDay$steps, col="red", main="Historgram of the total number of steps taken each day", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

```r
meanStepPerDay <- aggregate(steps~date,data,mean)
medianStepPerDay <- aggregate(steps~date,data,median)
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
a <- aggregate(steps~interval, data, mean)
plot(x=a$interval, y=a$steps, type="l", xlab="5-minute interval", ylab="Average number of steps taken (across all days)")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
a[order(a$steps,decreasing=TRUE),"interval"][1]
```

```
## [1] 835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
imputeMissingValue <- function (record) {
  if (is.na(record["steps"])) {
    record["steps"] <- a[a$interval == as.numeric(record["interval"]), "steps"]
  }
  
  return(record)
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newData <- as.data.frame(t(apply(data,1,imputeMissingValue)))
newData <- transform(newData, steps = as.numeric(as.character(steps)))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
numOfStepPerDay.new <- aggregate(steps~date, newData, sum)
hist(numOfStepPerDay.new$steps,col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
meanStepPerDay.new <- aggregate(steps~date,newData,mean)
medianStepPerDay.new <- aggregate(steps~date,newData,median)
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
data$dateType <- ifelse(format(strptime(data[,"date"],"%Y-%m-%d"),"%u") >= 6, "weekend", "weekday")

#Transform dataType to factor with 2 level
data <- transform(data, dateType=factor(dateType))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)
x <- aggregate(steps~interval+dateType, data, mean)
xyplot(steps~interval|dateType, data=x, layout=c(1,2), type="l", ylab="Number of steps", xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 


