Reproducible Research : Peer Assessment 1
========================================================

## Loading and preprocessing the data
First, load the data.


```r
data <- read.csv("activity.csv")
```

```
## Warning: cannot open file 'activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
data$date<-as.Date(data$date)
```

```
## Error: object of type 'closure' is not subsettable
```
## What is mean total number of steps taken per day?
Next, make a histogram of the total number of steps taken each day.


```r
x <- c(1, 10, 20, 30, 40, 50, 60)
stepssum = tapply(data$steps, data$date, sum, na.rm = TRUE)
```

```
## Error: object of type 'closure' is not subsettable
```

```r
# create a histogram of total steps per date
plot(stepssum, type="h", xlab="", ylab = "Total steps", xaxt="n")
```

```
## Error: object 'stepssum' not found
```

```r
axis(side =1, at=x, labels = names(stepssum)[x], las=2, cex.axis=.7)
```

```
## Error: object 'stepssum' not found
```

```r
title("Total number of steps taken each day")
```

```
## Error: plot.new has not been called yet
```

Now, calculate the mean and median of the total number of steps taken each day.


```r
meansteps <- tapply(data$steps, data$date, mean, na.rm = TRUE)
```

```
## Error: object of type 'closure' is not subsettable
```

```r
mediansteps <- tapply(data$steps, data$date, median, na.rm = TRUE)
```

```
## Error: object of type 'closure' is not subsettable
```






