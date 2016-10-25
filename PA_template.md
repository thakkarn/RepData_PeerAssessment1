# Reproducible Research - Peer Assessment 1
Naimish Thakkar  
October 24, 2016  



# Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. 

## Overview of the Data
The dataset can be download from the following link
Dataset: [Activity monitoring data] https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

```r
library(downloader)
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
download("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
         dest="dataset.zip", mode="wb")

tmp <- unzip("./dataset.zip", overwrite = TRUE, exdir = ".")

activity_df <- read.csv(tmp , sep = ",", stringsAsFactors = FALSE)
```

## What is mean total number of steps taken per day?
First let's plot the histogram of daily steps taken 

```r
activity_df$date <- as.Date(activity_df$date)

daily_steps <- tapply(activity_df$steps, activity_df$date, sum)

hist(daily_steps, xlab = "Daily Steps", col = "Green")
```

![](.\Figures\unnamed-chunk-1-1.png)<!-- -->

### Mean and median of total number of steps taken per day


```r
#mean
tapply(activity_df$steps, activity_df$date, mean)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
## 38.2465278         NA 44.4826389 34.3750000 35.7777778 60.3541667 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
## 43.1458333 52.4236111 35.2048611 52.3750000 46.7083333 34.9166667 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
## 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 29.0104167 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
## 53.5208333         NA 36.8055556 36.7048611         NA 36.2465278 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
## 28.9375000 44.7326389 11.1770833         NA         NA 43.7777778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
## 37.3784722 25.4722222         NA  0.1423611 18.8923611 49.7881944 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
## 52.4652778 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
## 50.2708333 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500 
## 2012-11-30 
##         NA
```

```r
tapply(activity_df$steps, activity_df$date, median)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA          0          0          0          0          0 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##          0         NA          0          0          0          0 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##          0          0          0          0          0          0 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##          0          0          0          0          0          0 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##          0          0          0          0          0          0 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##          0         NA          0          0         NA          0 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##          0          0          0         NA         NA          0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##          0          0         NA          0          0          0 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##          0          0          0          0          0          0 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##          0          0          0          0          0          0 
## 2012-11-30 
##         NA
```

## What is the average daily activity pattern?
Daily Activity Steps per interval: First Group by interval and then calculate average
steps per interval


```r
activity_df_grp <- group_by(activity_df, interval)

avg_steps_interval <- summarize(activity_df_grp, avg_steps = mean(steps, 
                                                                na.rm = TRUE))
qplot(x=interval, y=avg_steps, data = avg_steps_interval, 
      main = "Average Steps per Interval", xlab = "Interval", ylab = "Average Steps")
```

![](.\Figures\unnamed-chunk-3-1.png)<!-- -->

```r
max_avg_steps <- subset(avg_steps_interval, avg_steps == max(avg_steps_interval$avg_steps))

paste0("5-minute interval (on average across all the days in the dataset) with 
       the maximum number of steps is interval ", max_avg_steps$interval, 
       " with ", max_avg_steps$avg_steps)
```

```
## [1] "5-minute interval (on average across all the days in the dataset) with \n       the maximum number of steps is interval 835 with 206.169811320755"
```

## Imputing Missing Values

Total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
numNAs <- rowSums(is.na(activity_df))
print(paste0("Number of NAs in daily activities data are: ", 
              length(which(as.logical(numNAs)))))
```

```
## [1] "Number of NAs in daily activities data are: 2304"
```

The algorithm for imputing missing values is based on the following two factors. 

- Review of the daily pattern revealed that of the 288 five minute intervals, first 61 intervals and last 20 intervals were relatively inactive. It is expected that 
the these intervals are from around 10:30 PM to the morning and hence the suject 
did not have any steps noted. Hence the number of steps were set to 0 for first 61 intervals
- Interval 62 to 260(total number of intervals per day) were populated from the
average daily steps taken during those intervals that was calculated above. 


```r
NAvalues <- integer(288) 

NAvalues[62:260] <- as.integer(avg_steps_interval[62:260,]$avg_steps)

activity_na_grp <- group_by(activity_df, is.na(steps))


# Replace steps value in the groups of NA values.

activity_na_grp[activity_na_grp$`is.na(steps)`,]$steps = NAvalues
```

Here is the new dataset that is equal to the original dataset but with the missing data filled in.

```r
#modify the original data frame with new values
activity_df[,1:3] <- activity_na_grp[,1:3]
```

Histogram of the total number of steps taken each day as well as the mean and median total number of steps taken per day. 


```r
#calculate new steps
daily_steps <- tapply(activity_df$steps, activity_df$date, sum)

hist(daily_steps, xlab = "Daily Steps", col = "Green")
```

![](.\Figures\unnamed-chunk-7-1.png)<!-- -->

```r
#mean
tapply(activity_df$steps, activity_df$date, mean)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
## 36.5625000  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
## 38.2465278 36.5625000 44.4826389 34.3750000 35.7777778 60.3541667 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
## 43.1458333 52.4236111 35.2048611 52.3750000 46.7083333 34.9166667 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
## 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 29.0104167 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
## 53.5208333 36.5625000 36.8055556 36.7048611 36.5625000 36.2465278 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
## 28.9375000 44.7326389 11.1770833 36.5625000 36.5625000 43.7777778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
## 37.3784722 25.4722222 36.5625000  0.1423611 18.8923611 49.7881944 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
## 52.4652778 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
## 50.2708333 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500 
## 2012-11-30 
## 36.5625000
```

```r
tapply(activity_df$steps, activity_df$date, median)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##       33.5        0.0        0.0        0.0        0.0        0.0 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##        0.0       33.5        0.0        0.0        0.0        0.0 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##        0.0        0.0        0.0        0.0        0.0        0.0 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##        0.0        0.0        0.0        0.0        0.0        0.0 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##        0.0        0.0        0.0        0.0        0.0        0.0 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##        0.0       33.5        0.0        0.0       33.5        0.0 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##        0.0        0.0        0.0       33.5       33.5        0.0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##        0.0        0.0       33.5        0.0        0.0        0.0 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##        0.0        0.0        0.0        0.0        0.0        0.0 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##        0.0        0.0        0.0        0.0        0.0        0.0 
## 2012-11-30 
##       33.5
```

As can be seen, the height of the histogram is higher once the missing NA values
were imputed. Please note y-scale difference between two histograms. The same 
can be observed in mean and median values. The average steps per day is higher 
once the missing values were imputed. 

## Differences in activity patterns between weekdays and weekends?
In this section, we explore and articulate differences in activity patterns 
between weekdays and weekends. 
First let's create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
wday <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

activity_df <- mutate(activity_df, day_type = factor((weekdays(activity_df$date) %in% wday), levels = c(TRUE, FALSE), labels = c('Wkday', 'Wknd')))
```

Now let's compare the weekend and weekdays activity with a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
activity_df_grp <- group_by(activity_df, interval, day_type)

avg_steps_interval <- summarize(activity_df_grp, avg_steps = mean(steps))

g <- ggplot(avg_steps_interval, aes(x = avg_steps_interval$interval, 
                                    y=avg_steps_interval$avg_steps))

g + geom_line() + facet_wrap(~ day_type, ncol = 1) + 
    labs(x = "Interval") + labs(y = "Average Steps") +
    labs(title = "Activity Pattern: Weekdays and Weekends") 
```

![](.\Figures\unnamed-chunk-9-1.png)<!-- -->
