setwd("D:/Coursera/Data Science R/5. Reproducible Research")
activityData<-read.csv("activity.csv")

#Calculate the total number of steps taken per day
stepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)

#histogram of the total number of steps taken each day
hist(stepsPerDay$steps)

#Calculate and report the mean and median of the total number of steps taken per day
meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay
medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay

#average daily activity pattern
stepsPerInterval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps

#the total number of rows with NAs
totalValuesMissings <- sum(is.na(activityData$steps))
totalValuesMissings

#strategy for filling in all of the missing values in the dataset
#function that will return, for a particular interval, the mean value
getMeanStepsPerInterval<-function(interval){
    stepsPerInterval[stepsPerInterval$interval==interval,]$steps
}

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
activityDataNoNA<-activityData
for(i in 1:nrow(activityDataNoNA)){
    if(is.na(activityDataNoNA[i,]$steps)){
        activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
    }
}

# histogram of the total number of steps taken each day
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)

# mean and median total number of steps taken per day
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
meanStepsPerDayNoNA
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA


#Are there differences in activity patterns between weekdays and weekends?
#  factor variable in the dataset with two levels - "weekday" and "weekend" 
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
    if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
        activityDataNoNA[i,]$day<-"weekend"
    }
    else{
        activityDataNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)


# panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all weekday days or weekend days (y-axis)    

names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
           xlab = "Interval", ylab = "Number of steps")
    
}
