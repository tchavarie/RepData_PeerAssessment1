# Loading and preprocessing the data

# Unzip files and load the data
unzip("activity.zip")
activity <- read.csv(file = "activity.csv")

# What is mean total number of steps taken per day?

# Calculate the total number of steps taken per day
steps.per.day <- aggregate(steps ~ date, activity, FUN = sum)

# Make a histogram of the total number of steps taken each day
hist(steps.per.day$steps, breaks = 10, main = "Total Steps Per Day", xlab = "Total Steps")
abline(v = median(steps.per.day$steps), col = "blue", lwd = 4)

# Calculate and report the mean and median of the total number of steps taken per day
paste("Mean number of steps in a day:", mean(steps.per.day$steps), sep = " ")
paste("Median number of steps in a day:", median(steps.per.day$steps), sep = " ")

# Calculate the average number of steps taken per 5-minute interval 
steps.per.interval <- aggregate(steps ~ interval, activity, FUN = mean)

# What is the average daily activity pattern?

# Make a time series plot of the 5-minute interval and the 
# average number of steps taken, averaged across all days 
plot(steps.per.interval$interval, steps.per.interval$steps, 
     type = "l", xlab = "5-minute interval", 
     ylab = "Average steps taken", 
     main = "Average Steps Per Interval")

# Which 5-minute interval, on average across all the days 
# in the dataset, contains the maximum number of steps?
paste("Interval with maximum number of steps:", 
      steps.per.interval[which.max(steps.per.interval$steps), 1], sep = " ")

# Imputing missing values

# Calculate and report the total number of missing values 
# in the dataset
nrow(subset(activity, is.na(steps)))

# Impute missing values using the average steps per interval
activity.notna <- activity[!is.na(activity$steps), ]
activity.na <- activity[is.na(activity$steps), ]

activity.imputed <- merge(activity.na, steps.per.interval, by=c("interval"))[, c(4, 3, 1)]
colnames(activity.imputed) <- colnames(activity)

# Create a new dataset that is equal to the original dataset 
# but with the missing data filled in
activity.imputed <- rbind(activity.imputed, activity.notna)

# Make a histogram of the total number of steps taken each day 
# and Calculate and report the mean and median total 
# number of steps taken per day

# Calculate the total number of steps taken per day
steps.per.day2 <- aggregate(steps ~ date, activity.imputed, FUN = sum)

# Make a histogram of the total number of steps taken each day
hist(steps.per.day2$steps, breaks = 10, main = "Total Steps Per Day", xlab = "Total Steps", col = "red")
hist(steps.per.day$steps, breaks = 10, main = "Total Steps Per Day", xlab = "Total Steps", col = "blue", add = T)

# Calculate and report the mean and median of the total number 
# of steps taken per day
paste("Mean number of steps in a day:", mean(steps.per.day$steps), sep = " ")
paste("Median number of steps in a day:", median(steps.per.day$steps), sep = " ")

# Are there differences in activity patterns between weekdays 
# and weekends?
activity.imputed$dow <- weekdays(as.Date(activity.imputed$date))
activity.imputed$wd <- as.factor(ifelse(activity.imputed$dow %in% c("Saturday","Sunday"), 
                              "Weekend", "Weekday"))    


# Calculate the average number of steps taken per 5-minute interval 
weekday <- aggregate(steps ~ interval, activity.imputed[activity.imputed$wd == "Weekday", ], FUN = mean)
weekend <- aggregate(steps ~ interval, activity.imputed[activity.imputed$wd == "Weekend", ], FUN = mean)


# Make a panel plot containing a time series plot of the 5-minute 
# interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis)
par(mfrow = c(2,1))
plot(weekday$interval, weekday$steps, 
     type = "l", xlab = "5-minute interval", 
     ylab = "Average steps taken", 
     main = "Average Steps Per Interval - Weekday")
plot(weekend$interval, weekend$steps, 
     type = "l", xlab = "5-minute interval", 
     ylab = "Average steps taken", 
     main = "Average Steps Per Interval - Weekend")

