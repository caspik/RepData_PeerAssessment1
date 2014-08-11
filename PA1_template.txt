## Loading and preprocessing the data


```r
unzip("activity.zip")
activity.data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

1. The total number of steps taken each day


```r
steps.per.day <- aggregate(steps ~ date, data = activity.data, FUN = sum)
barplot(steps.per.day$steps, names.arg = steps.per.day$date, xlab = "date", ylab = "steps")
title(main = "Death Rates in Virginia", font.main = 4)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

2. The **mean** and **median** of the total number of steps taken per day
   

```r
mean(steps.per.day$steps)
```

```
## [1] 10766
```

```r
median(steps.per.day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute
   interval (x-axis) and the average number of steps taken, averaged
   across all days (y-axis)
   

```r
steps.5min <- aggregate(steps ~ interval, data = activity.data, FUN = mean)
plot(steps.5min, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

2. Which 5-minute interval, on average across all the days in the dataset,contains the maximum number of steps?


```r
steps.5min$interval[which.max(steps.5min$steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(steps.5min))
```

```
## [1] 0
```

2. Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.

I will use the means for the 5-minute intervals as fillers for missing
values.

3. Create a new dataset that is equal to the original dataset but with
   the missing data filled in.
   

```r
activity <- merge(activitydata, steps.5min, by = "interval", suffixes = c("", ".y"))
```

```
## Error: object 'activitydata' not found
```

```r
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]
```

4. Make a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?
   

```r
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
mean(steps.date$steps)
```

```
## [1] 10766
```

```r
median(steps.date$steps)
```

```
## [1] 10766
```

The impact of the missing data seems rather low, at least when
estimating the total number of steps per day.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.



```r
daytype <- function(date) {
        if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
                "weekend"
        } else {
                "weekday"
        }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
        steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$daytype == type, FUN = mean)
        plot(steps.type, type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
