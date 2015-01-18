---
title  : "RR_PA1"
author : "RoelRules"
date   : "Wednesday, January 17, 2015"
output : html_document
keep_md: true
---

#Loading and preprocessing the data

activity = read.csv(unz("./repdata_data_activity.zip", "activity.csv"))

# Remove Rows with Missing Data with aggregate(default without NA)

steps.date = aggregate(steps ~ date, data=activity, FUN=sum)

# 1 make a barplot(plot1_RR_PA1.png) with steps by date(day)


barplot(steps.date$steps, main = "Steps by date \n (without NA)", names.arg=steps.date$date, xlab="Date", ylab="Number of steps")


# 2.Calculate and report the mean(10766.19) and median(10765) per total number of steps taken per day

mean(steps.date$steps)
median(steps.date$steps) 

# What is the average daily activity pattern?

# 1.Make a time series plot(plot2_RR_PA1.png,i.e. type = "l") of the 5-minute interval (x-axis) and the 
# average number of steps taken, averaged across all days (y-axis)

steps.interval =  aggregate(steps ~ interval, data=activity, FUN=mean)

plot(steps.interval, type="l", main = "Average steps by day", xlab = "5-minute interval across all days", ylab= "average number of steps")

# 2.Which 5-minute interval, on average across all the days in the dataset, contains the 
# maximum number of steps?

steps.interval$interval[which.max(steps.interval$steps)]

# Imputing missing values
# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with  NA's= 2304 rows)

nMissingdata = sum(is.na(activity))

# 2.Devise a strategy for filling in all of the missing values in the dataset. The   
# strategy does not need to be sophisticated. For example, you could use the mean/median
# for that day, or the mean for that 5-minute interval, etc

# I use the means off the 5-min intervalsteps.
# I created a new dataset activity2, filled with this means instead of the missing data.

activity2 = merge(activity, steps.interval, by="interval", suffixes=c("",".y"))
nas = is.na(activity2$steps)
activity2$steps[nas] = activity2$steps.y[nas]
activity2 = activity2[,c(1:3)]

# 1.Make a histogram(plot3_RR_PA1.png) of the total number of steps taken each day and Calculate and 
# report the mean and median total number of steps taken per day. Do these values differ from the 
# estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

steps.date <- aggregate(steps ~ date, data=activity2, FUN=sum)

barplot(steps.date$steps, names.arg=steps.date$date, 
main = "Steps by date \n (with replacing NA)",xlab="Date", ylab="Number of steps")

mean(steps.date$steps) 
median(steps.date$steps) 

# Mean=10766.19 and median= 10766.19 The impact of the missing data seems rather low, at least when 
# estimating the total number of steps per day.

# Are there differences in activity patterns between weekdays and weekends?

# 1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating 
# whether a given date is a weekday or weekend day.

daytype = function(date) {
  if (weekdays(as.Date(date)) %in% c("zaterdag", "zondag", "Saturday","Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
activity2$daytype = as.factor(sapply(activity2$date, daytype))


# 1.Make a panel plot containing a time series plot (i.e.  type = "l" ) of the 5-minute interval (x-axis) # and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

# Aggregate activity2 by steps as a function of interval + day  

stepsByDay = aggregate(activity2$steps ~ activity2$interval + activity2$day, activity2, mean)

# Rename the column names

names(stepsByDay) = c("interval", "day", "steps")

# Plot(plot4_RR_PA1.png) weekday versus weekend steps

par(mfrow=c(1,1))  
with(stepsByDay, plot(steps ~ interval, type="n", main="Activity weekday versus weekend(Avg)",
xlab ="5-minute interval across all days",   ylab = "Average number of steps "))  
with(stepsByDay[stepsByDay$day == "weekday",], lines(steps ~ interval, type="l", col="red"))  
with(stepsByDay[stepsByDay$day == "weekend",], lines(steps ~ interval, type="l", col="green" ))  
legend("topright", lty=c(1,1), col = c("red", "green"), legend = c("weekday", "weekend"), seg.len=3)

# The walking person likes mostly activ on the weekends.
