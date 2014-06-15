Reproducible Research : Peer Assessment 1
========================================================

## Loading and preprocessing the data
First, load the data.


```r
data <- read.csv("activity.csv")
data$date<-as.Date(data$date)
```
## What is mean total number of steps taken per day?
Next, make a histogram of the total number of steps taken each day.


```r
x <- c(1, 10, 20, 30, 40, 50, 60)
stepssum = tapply(data$steps, data$date, sum, na.rm = TRUE)
# create a histogram of total steps per date
plot(stepssum, type="h", xlab="", ylab = "Total steps", xaxt="n")
axis(side =1, at=x, labels = names(stepssum)[x], las=2, cex.axis=.7)
title("Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Now, calculate the mean and median of the total number of steps taken each day.


```r
meansteps <- tapply(data$steps, data$date, mean, na.rm = TRUE)
mediansteps <- tapply(data$steps, data$date, median, na.rm = TRUE)
```

The means are NaN, 0.4375, 39.4167, 42.0694, 46.1597, 53.5417, 38.2465, NaN, 44.4826, 34.375, 35.7778, 60.3542, 43.1458, 52.4236, 35.2049, 52.375, 46.7083, 34.9167, 41.0729, 36.0938, 30.6285, 46.7361, 30.9653, 29.0104, 8.6528, 23.5347, 35.1354, 39.7847, 17.4236, 34.0938, 53.5208, NaN, 36.8056, 36.7049, NaN, 36.2465, 28.9375, 44.7326, 11.1771, NaN, NaN, 43.7778, 37.3785, 25.4722, NaN, 0.1424, 18.8924, 49.7882, 52.4653, 30.6979, 15.5278, 44.3993, 70.9271, 73.5903, 50.2708, 41.0903, 38.7569, 47.3819, 35.3576, 24.4688, NaN.

And the medians are NA, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, NA, 0, 0, 0, 0, NA, NA, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA.


## What is the average daily activity pattern?


```r
meaninterval <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(meaninterval, type="l", xlab = "Interval", ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
# find the interval with the maximum average number of steps.
maxinterval <- which.max(meaninterval)
```

The interval with the maximum average number of steps is 104 with a value of 206.1698


## Imputing missing values


```r
# find number of rows with missing data
numofna <- sum(is.na(data$steps))


# replace missing data with the value from the mean of its interval
# creating a new data set, imdata
imdata <- subset(data, is.na(data$steps) == FALSE)
missing <- subset(data, is.na(data$steps))
missing$steps <- meaninterval[as.character(missing$interval)]
imdata <- rbind(imdata, missing)

#repeat mean/median calculation from above
x <- c(1, 10, 20, 30, 40, 50, 60)
stepssum = tapply(imdata$steps, imdata$date, sum, na.rm = TRUE)
# create a histogram of total steps per date
plot(stepssum, type="h", xlab="", ylab = "Total steps", xaxt="n")
axis(side =1, at=x, labels = names(stepssum)[x], las=2, cex.axis=.7)
title("Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
meansteps <- tapply(imdata$steps, imdata$date, mean, na.rm = TRUE)
mediansteps <- tapply(imdata$steps, imdata$date, median, na.rm = TRUE)
```

The means are 37.3826, 0.4375, 39.4167, 42.0694, 46.1597, 53.5417, 38.2465, 37.3826, 44.4826, 34.375, 35.7778, 60.3542, 43.1458, 52.4236, 35.2049, 52.375, 46.7083, 34.9167, 41.0729, 36.0938, 30.6285, 46.7361, 30.9653, 29.0104, 8.6528, 23.5347, 35.1354, 39.7847, 17.4236, 34.0938, 53.5208, 37.3826, 36.8056, 36.7049, 37.3826, 36.2465, 28.9375, 44.7326, 11.1771, 37.3826, 37.3826, 43.7778, 37.3785, 25.4722, 37.3826, 0.1424, 18.8924, 49.7882, 52.4653, 30.6979, 15.5278, 44.3993, 70.9271, 73.5903, 50.2708, 41.0903, 38.7569, 47.3819, 35.3576, 24.4688, 37.3826.

And the medians are 34.1132, 0, 0, 0, 0, 0, 0, 34.1132, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 34.1132, 0, 0, 34.1132, 0, 0, 0, 0, 34.1132, 34.1132, 0, 0, 0, 34.1132, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 34.1132.

As we can see, the values that used to be NA are now different.  Since NAs seem to be lumped into days, there doesn't seem to be much difference in the other values.



## Are there differences in activity patterns between weekdays and weekends?


```r
# create a weekday/weekend factor
imdata$day<-as.factor(weekdays(imdata$date))
imdata$day<-factor(imdata$day, labels=c("weekday", "weekday", "weekend", "weekend", "weekday", "weekday","weekday"))
```

```
## Warning: duplicated levels in factors are deprecated
```

```r
levels(imdata$day) <- list(weekday = c(1,2,5,6,7), weekend = c(3,4))

# create panel plot of time series plot (like above) separating by weekday/weekend data
wkday <- subset(imdata, imdata$day=="weekday")
wkend <- subset(imdata, imdata$day=="weekend")

par(mfrow = c(2,1), cex.lab = .7, mar = c(2, 4, 4, 2))

meaninterval <- tapply(wkday$steps, wkday$interval, mean, na.rm=TRUE)
plot(meaninterval, type="l", xlab = "Interval", ylab = "Average number of steps")
title("Weekday")

meaninterval <- tapply(wkend$steps, wkend$interval, mean, na.rm=TRUE)
plot(meaninterval, type="l", xlab = "Interval", ylab = "Average number of steps")
title("Weekend")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 
