Reproducible Research Course Project 1 (Week 2)
Loading and preprocessing the data
fitnessdata <- read.csv("activity.csv", header = TRUE)
head(fitnessdata)
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
What is mean total number of steps taken per day?
Calculate the total number of steps taken per day
Make a histogram of the total number of steps taken each day
stepsbyday <- aggregate(steps ~ date, fitnessdata, FUN = sum, na.rm=TRUE)

hist(stepsbyday$steps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)

![](PA_templetsFigures/Calculate the total number of steps taken per day; Make a histogram of the total number of steps taken each day-1


head(stepsbyday)
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
Calculate and report the mean and median of the total number of steps taken per day
print(mean(stepsbyday$steps))
## [1] 10766.19
print(median(stepsbyday$steps))
## [1] 10765
What is the average daily activity pattern?
#Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
library(ggplot2)
## Warning: package 'ggplot2' was built under R version 3.5.3
stepsbyinterval <- aggregate(steps ~ interval, fitnessdata, FUN = mean, na.rm=TRUE)
ggplot(stepsbyinterval, aes(x=interval, y=steps))+ geom_line()


#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
stepsbyinterval[which(stepsbyinterval$steps== max(stepsbyinterval$steps)),]
##     interval    steps
## 104      835 206.1698
Imputing missing values
fitnessdata_filled <- fitnessdata

nrow(fitnessdata_filled[is.na(fitnessdata_filled$steps),])
## [1] 2304
fitnessdata_narm <- fitnessdata_filled[!is.na(fitnessdata_filled$steps),]

missingData <- is.na(fitnessdata_filled$steps)
meanVals <- tapply(fitnessdata_narm$steps, fitnessdata_narm$interval, mean, na.rm=TRUE, simplify=TRUE)
fitnessdata_filled$steps[missingData] <- meanVals[as.character(fitnessdata_filled$interval[missingData])]
head(fitnessdata_filled)
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
write.csv(fitnessdata_filled, file = "activity_filled.csv")

stepsbyday_filled <- aggregate(steps ~ date, fitnessdata_filled, FUN = sum, na.rm=TRUE)

hist(stepsbyday_filled$steps, xlab = "Total daily Steps",main="Histogram of Total Steps by day (NA Mean Filled)", breaks = 20)


Are there differences in activity patterns between weekdays and weekends?
1 of 3
# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. 
mean(stepsbyday$steps)
## [1] 10766.19
mean(stepsbyday_filled$steps)
## [1] 10766.19
median(stepsbyday$steps)
## [1] 10765
median(stepsbyday_filled$steps)
## [1] 10766.19
2 of 3
#head(as.Date(stepsbyday_filled$date))
fitnessdata_filled$weekday <- weekdays(as.Date(fitnessdata_filled$date))
fitnessdata_filled$weekend <- ifelse (fitnessdata_filled$weekday == "Saturday" | fitnessdata_filled$weekday == "Sunday", "Weekend", "Weekday")

head(fitnessdata_filled,7)
##       steps       date interval weekday weekend
## 1 1.7169811 2012-10-01        0  Monday Weekday
## 2 0.3396226 2012-10-01        5  Monday Weekday
## 3 0.1320755 2012-10-01       10  Monday Weekday
## 4 0.1509434 2012-10-01       15  Monday Weekday
## 5 0.0754717 2012-10-01       20  Monday Weekday
## 6 2.0943396 2012-10-01       25  Monday Weekday
## 7 0.5283019 2012-10-01       30  Monday Weekday
3 of 3
weekendagg <- aggregate(steps ~ weekend + interval, fitnessdata_filled, FUN = mean)

names(weekendagg) <- c("weekend", "interval", "steps")

ggplot(weekendagg, aes(x = interval, y=steps, color=weekend)) + geom_line() + facet_grid(weekend ~ .)
