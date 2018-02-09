#Course project 1

## 1. Let's clean the data set first

```r
setwd('D:/heyimeng')
activity <- read.csv('activity.csv', na.strings = '')
activity$date <- as.Date(as.character(activity$date))  ## change the format of date
head(activity, 5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

## 2. Total number of steps taken per day

#### 1. Caculate the total number of steps taken per day 

```r
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
activity1 <- filter(activity, steps != 'NA')
activity1$steps <- as.numeric(activity1$steps)
activity1$date <- as.factor(activity1$date)
steps_per_day <- tapply(activity1$steps, activity1$date, sum)
print(steps_per_day)
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        915      25694      29337      25634      31225      30104 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##      25666      28697      23766      35113      32348      29394 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 
##      26703      28775      27953      20313      26850      25760 
## 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 
##      25915      25966      23700      23114      13250      19041 
## 2012-10-27 2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-02 
##      20467      29528      18047      23825      26095      24178 
## 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-11 
##      28422      23116      21716      25872      16028      29727 
## 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      19364      23871       1118      16854      26144      28406 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 
##      23846      12752      22309      34496      26549      24487 
## 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      23428      28816      25964      16005      17306
```

#### 2. Make a histgram of total number of steps per day

```r
hist(steps_per_day,
     xlab = expression('Steps'), 
     main = 'Histogram of steps per day')
```
[image](https://github.com/heyimeng97/RepData_PeerAssessment1/figures/histogram of steps per day.png )


#### 3. Caculate the mean and median value of steps per day

```r
meanvalue <- mean(steps_per_day)
medianvalue <- median(steps_per_day)
print(meanvalue)
```

```
## [1] 23848.47
```

```r
print(medianvalue)
```

```
## [1] 25666
```

## 3. Daily activity pattern

#### 1. Time series plot

```r
steps_per_interval <- tapply(activity1$steps, activity1$interval, mean)
plot(unique(activity1$interval), steps_per_interval,
            xlab = 'Interval',
            ylab = 'Steps',
            main = 'Time series plot',
            type = 'l')
```

[image](https://github.com/heyimeng97/RepData_PeerAssessment1/figures/Time series plot.png )

#### 2. Find the maximum number of steps

```r
location <- which(steps_per_interval == max(steps_per_interval))
steps_per_interval[location]
```

```
##     1845 
## 261.0755
```


## 4. Imputing missing value

#### 1. Calculate and report the total number of missing values in the dataset 

```r
NAS <- activity$steps == 'NA'
sum(NAS)
```

```
## [1] 2304
```

#### 2&3. Filling all the missing values(use the mean of 5-minute intervals)

```r
actna <- filter(activity, steps == 'NA')
Index<- data.frame()
        for (i in 1:288){
                Index[i,1] <- steps_per_interval[[i]]
        }
cn <- names(steps_per_interval)
meanIndex <- cbind(cn,Index)
names(meanIndex) = c('interval', 'steps')
actfilled <- cbind(meanIndex, actna)
```

```
## Warning in data.frame(..., check.names = FALSE): row names were found from
## a short variable and have been discarded
```

```r
actfilled <- actfilled[, -c(1,3)]
actfilled$interval <- as.integer(actfilled$interval)
activityModeifed <- rbind(actfilled, activity1)
head(activityModeifed)
```

```
##       steps       date interval
## 1 12.603774 2012-10-01        0
## 2  2.679245 2012-10-01        5
## 3 10.867925 2012-10-01       10
## 4 12.188679 2012-10-01       15
## 5  6.660377 2012-10-01       20
## 6 14.679245 2012-10-01       25
```

#### 4.Histogram of steps taken per day and mean and median valuue

```r
steps_per_day_Modified <- tapply(activityModeifed$steps, activityModeifed$date, sum)
hist(steps_per_day_Modified, main = 'Modified histogram', xlab = 'steps')
```

[image](https://github.com/heyimeng97/RepData_PeerAssessment1/figures/Modified histogram.png) 

```r
mean(steps_per_day_Modified)
```

```
## [1] 23848.47
```

```r
median(steps_per_day_Modified)
```

```
## [1] 23871
```

## 5. Differences in activity patterns between weekdays and weekends
#### 1. filter weekdays and weekend

```r
Sys.setlocale("LC_TIME", "English") 
```

```
## [1] "English_United States.1252"
```

```r
DATE <- weekdays(activityModeifed$date)
activityModeifed <- cbind(activityModeifed, DATE)
actweekday <- filter(activityModeifed, DATE != 'Sunday' & DATE != 'Saturday' )
actweekday$DATE <- 'Weekday'
actweekend <- filter(activityModeifed, DATE == 'Sunday' | DATE == 'Saturday')
actweekend$DATE <- 'Weekend'
activityModeifed <- rbind(actweekend, actweekday)
activityModeifed <- arrange(activityModeifed, date)
head(activityModeifed)
```

```
##       steps       date interval    DATE
## 1 12.603774 2012-10-01        0 Weekday
## 2  2.679245 2012-10-01        5 Weekday
## 3 10.867925 2012-10-01       10 Weekday
## 4 12.188679 2012-10-01       15 Weekday
## 5  6.660377 2012-10-01       20 Weekday
## 6 14.679245 2012-10-01       25 Weekday
```

#### 2. Average steps taken between weekday and weekend

```r
steps_per_interval_weekend <- tapply(actweekend$steps, actweekend$interval, mean)
steps_per_interval_weekday <- tapply(actweekday$steps, actweekday$interval, mean)
par(mfcol = c(2,1))
plot(unique(activityModeifed$interval), steps_per_interval_weekend, type = 'l',
     col = 'blue', main = 'weekend', xlab = 'Interval', ylab = 'Steps')
plot(unique(activityModeifed$interval), steps_per_interval_weekday, type = 'l',
     col = 'blue', main = 'weekday', xlab = 'Interval', ylab = 'Steps')  
```

[image](https://github.com/heyimeng97/RepData_PeerAssessment1/figures/weekday and weekend pattern.png) 
