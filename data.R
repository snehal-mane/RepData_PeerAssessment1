# Loading and preprocessing the data
activity<-read.csv("activity.csv")
dim(activity)
names(activity)
head(activity)
sum(is.na(activity))

#What is mean total number of steps taken per day?
steps_date <- aggregate(steps~date, activity, sum, na.rm=TRUE)
hist(steps_date$steps,
     main = "Histogram of the total number of steps taken each day",
     xlab = "Steps",
     col = "cadetblue"
     )


#What is the average daily activity pattern?
stepsinterval <- aggregate(steps~interval, activity, mean, na.rm=TRUE)
with(stepsinterval,plot(x=interval,
                        y=steps,
                        type="l",
                        main = "Average Daily Activity Pattern",
                        xlab = "Interval",
                        ylab = "Number of steps",
                        col = "cadetblue",
                        col.main = "cadetblue"))

#Imputing missing values
activity2 <- activity
for(i in 1:nrow(activity2)){
        if(is.na(activity2[i,]$steps)){
                activity2[i,]$steps <- stepsinterval[stepsinterval$interval==activity2[i,]$interval,]$steps
        }
}

steps_date2 <- aggregate(steps~date, activity2, sum)
hist(steps_day$steps,
     main = "Total number of steps taken each day with missing values removed",
     xlab = "Steps",
     col = "cadetblue",
     col.main = "cadetblue"
)


# Are there differences in activity patterns between weekdays and weekends?
activity2$date <- as.Date(strptime(activity2$date,"%Y-%m-%d"))
activity2$day <- weekdays(activity2$date)
x = c("Saturday","Sunday")
for(i in 1:nrow(activity2)){
        if(activity2[i,]$day %in% x){
                activity2[i,]$day <- "weekend"
        }
        else {
                activity2[i,]$day <- "weekday"
        }
}

steps_day <- aggregate(activity2$steps ~ activity2$interval + activity2$day, activity2, mean)
names(steps_day) <- c("interval","day","steps")

xyplot(steps~interval|day,
       steps_day,
       type="l",
       layout = c(1,2),
       main = "5 minute interval and number of steps taken averaged  across all weekday days or weekend days",
       xlab = "Interval",
       ylab = "Average No. of Steps",
       col = "cadetblue"
       )
 