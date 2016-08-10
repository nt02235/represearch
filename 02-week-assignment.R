get.data <- function() {
   zipname <- "repdata-data-acitivity.zip"
   filename <- "activity.csv"
   
   if(!file.exists(filename)) {
      if(!file.exists(zipname)) 
         download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", zipname)
      unzip(zipname, exdir = ".")
   }
   
   read.csv(filename, header = T)
}
   
activity <- get.data()

library(dplyr)
install.packages("ggplot2")
library(ggplot2)

# What is mean total number of steps taken per day?
# ----------------------------------------------------------------
# For this part of the assignment, you can ignore the missing values in the dataset.

# 1. Calculate the total number of steps taken per day
daily_steps <- activity %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = T))
daily_steps$date <- strptime(daily_steps$date, "%Y-%m-%d")

# 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
ggplot(daily_steps, aes(total_steps)) + geom_histogram()

# 3. Calculate and report the mean and median of the total number of steps taken per day
mean(daily_steps$total_steps)
median(daily_steps$total_steps)

# What is the average daily activity pattern?
# ----------------------------------------------------------------
# 1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
tof_steps <- activity %>% group_by(interval) %>% summarise(average_steps = mean(steps, na.rm = T))
plot(tof_steps$interval, tof_steps$average_steps, type = "l", xlab = "5-min Interval", ylab = "Average Steps")

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
filter(tof_steps, average_steps == max(average_steps))

# Imputing missing values
# ----------------------------------------------------------------
# Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰)
nrow(filter(activity, is.na(steps)))

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
fill_na <- function(x) {
   for (i in 1:nrow(x)) {
      if (!is.na(x[i, "steps"]))
         x[i, "steps"] <- tof_steps[which(tof_steps$interval == x[i, "interval"]), "average_steps"]
   }
}

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
# Are there differences in activity patterns between weekdays and weekends?
# 
# For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.
# 
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
