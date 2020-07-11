# Loading and preprocessing the data
activity<-read.csv("activity.csv")
dim(activity)
names(activity)
head(activity)
sum(is.na(activity))

#What is mean total number of steps taken per day?
steps_day <- aggregate(steps~date, activity, sum, na.rm=TRUE)
hist(steps_day$steps,
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

steps_day2 <- aggregate(steps~date, activity2, sum)
hist(steps_day$steps,
     main = "Total number of steps taken each day with missing values removed",
     xlab = "Steps",
     col = "cadetblue",
     col.main = "cadetblue"
)




