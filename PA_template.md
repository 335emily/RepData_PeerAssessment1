# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
This is the code used to download, unzip, and load the data into R, and transform the date column into date-type data.

```r
library(ggplot2)

if(!file.exists("activity.csv")) {
	URL1 <- "https://github.com/rdpeng/RepData_PeerAssessment1/blob/master/activity.zip?raw=true"
	dateDownloaded <- date()
	temp <- tempfile()
	download.file(URL1,temp, method = "auto")
	unzip(temp)
	unlink(temp)
}

activityData <-read.csv("activity.csv")

activityData$date <- as.Date(activityData$date)
```

## What is mean total number of steps taken per day?
###1. Calculate the total number of steps taken per day
First, I will calculate the number of steps taken on each day, by summing all steps on each day (excluding NAs)

```r
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
```

###2. Make a histogram of the total number of steps taken each day
The distribution of the steps looks like:

```r
stepsHist <- hist(stepsByDay, col="blue", ylab="Frequency", xlab="Steps per Day", main="Histogram of Steps")
```

![](PA_template_files/figure-html/unnamed-chunk-3-1.png) 

###3. Calculate and report the mean and median of the total number of steps taken per day

```r
stepsByDayMean <-mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
```
The mean number of steps is: 9354.2295082
The median number of steps per day is: 10395

## What is the average daily activity pattern?
###1. Make a time series plot
The average daily activity pattern looks like:

```r
stepsByIntervalMean <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)

colnames(stepsByIntervalMean) <- c("Interval", "avgSteps")

with(stepsByIntervalMean,plot(Interval, avgSteps, main="Average Number of Steps \n Taken During 5 Minute Intervals", ylab="Average Steps in Interval", type="l"))
```

![](PA_template_files/figure-html/unnamed-chunk-5-1.png) 

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxRow <- which.max(stepsByIntervalMean[,2])
maxInterval <- stepsByIntervalMean[maxRow,1]
maxIntervalTimeHour <- floor(maxInterval/60)
maxIntervalTimeMinute <- (maxInterval/60 - floor(maxInterval/60))*60
maxIntervalTime <- paste(maxIntervalTimeHour, ":", maxIntervalTimeMinute, sep="")
```
The 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps is the interval beginning at 13:55

## Imputing missing values
###1. Calculate and report the total number of missing values in the dataset

```r
numMissingCounts <- length(which(is.na(activityData$steps)))
```
The numer of missing values is 2304

###2. Devise a strategy for filling in all of the missing values in the dataset.
In order to fill in the missing values, I will replace the NAs with the mean of all 5-minute intervals.

The process to do so is: 
1. Find the locations of the NA values
2. Make a vector with the length = number of NAs, fully populated with the mean of all 5-minute intervals

```r
naLocations <- which(is.na(activityData$steps))
meanVec <- rep(mean(activityData$steps, na.rm=TRUE), times=length(naLocations))
```

###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
Duplicate the dataset and replace the NA values with the mean of all 5-minute interval.

```r
activityData2 <- activityData
activityData2[naLocations, "steps"] <- meanVec
```

###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
After adding replacing the NA values with the estimated values, the number steps per day increases, as seen in this distribution.

```r
stepsByDay2 <- tapply(activityData2$steps, activityData2$date, sum, na.rm=TRUE)
stepsHist2 <- hist(stepsByDay2, col="blue", ylab="Frequency", xlab="Steps per Day", main="Histogram of Steps \n (NAs Altered)")
```

![](PA_template_files/figure-html/unnamed-chunk-10-1.png) 

The increase to the mean and median steps per day can also be seen.

```r
stepsByDayMean2 <-mean(stepsByDay2)
stepsByDayMedian2 <- median(stepsByDay2)
```

Specifically, before removing the NA values, the mean was 9354.2295082 and now it is 1.0766189\times 10^{4}
For the median, before removing the NA values, the median was 10395 and now it is 1.0766189\times 10^{4}

## Are there differences in activity patterns between weekdays and weekends?

###1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
Using the weekdays function, I've determined the day of the week for each, and then used an ifelse to classify into weekends and weekdays. I created a new data frame called activityDataDays with the new column.

```r
dayOfWeekVec <- weekdays(activityData2$date)
dayOfWeekVec <- ifelse (dayOfWeekVec=="Saturday" | dayOfWeekVec=="Sunday","Weekend","Weekday")

activityDataDays <- cbind(activityData2, dayOfWeekVec)
activityDataDays <- data.frame(activityDataDays)
```

###2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
I've created a new table that includes the average number of steps per interval, broken out over weekends and weekdays. Then I've plotted the line graph for the average number of steps per interval.

```r
stepsByIntervalMean2 <- aggregate(steps ~ interval + dayOfWeekVec, data=activityDataDays, mean)

colnames(stepsByIntervalMean2) <- c("Interval","dayOfWeek","avgSteps")

finalPlot <- ggplot(stepsByIntervalMean2, aes(Interval, avgSteps)) + 
	geom_line() + 
	facet_grid(dayOfWeek ~ .) +
	xlab("Interval") + 
	ylab("Average Steps in Interval")

finalPlot
```

![](PA_template_files/figure-html/unnamed-chunk-13-1.png) 

As the graph shows, people tend to be more active on weekdays mornings, with a notable spike around 800 on the x axis. However, people are more consistently active throughout the weekend, with less fluctuations.
