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
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
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

![](PA_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

### Mean and median of total number of steps taken per day


```r
#mean
mean(daily_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(daily_steps, na.rm = TRUE)
```

```
## [1] 10765
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

![](PA_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

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

![](PA_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
#mean
mean(daily_steps)
```

```
## [1] 10735.21
```

```r
median(daily_steps)
```

```
## [1] 10530
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

avg_steps_interval <- summarize(activity_df_grp, avg_steps = as.integer(mean(steps)))

activity_wkday_df <- filter(avg_steps_interval, day_type == "Wkday")

activity_wkend_df <- filter(avg_steps_interval, day_type == "Wknd")

g <- ggplot(activity_wkday_df, aes(x = interval, y = avg_steps))+ geom_line() + 
    labs(x = "Interval") + labs(y = "Average Steps") +
    labs(title = "Weekdays") 
   
h <- ggplot(activity_wkend_df, aes(x = interval, y = avg_steps)) + geom_line() + 
    labs(x = "Interval") + labs(y = "Average Steps") +
    labs(title = "Weekends") 

grid.arrange(g, h, ncol = 1, top="Activity Pattern: Weekdays and Weekends")
```

![](PA_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
