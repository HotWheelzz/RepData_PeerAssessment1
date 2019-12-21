
Reproducible Research: Week 2 Assigment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.


##Loading and preprocessing the data

```r
#Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
if (!file.exists('activity.csv')) {unzip(zipfile = "activity.zip")}
#Process/transform the data (if necessary) into a format suitable for your analysis.
CollectedData <- read.csv(file="activity.csv", header=TRUE)
```

##What is mean total number of steps taken per day?

```r
# Calculate the total steps taken per day
TotalSteps <- aggregate(steps ~ date, CollectedData, FUN=sum)

# Make a histogram of the total number of steps taken per day
hist(TotalSteps$steps,
     main = "Total Steps per Day",
     xlab = "Number of Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)



```r
# Calculate and report the mean and median of the total number of steps taken per day
MeansSteps <- mean(TotalSteps$steps, na.rm = TRUE)
MedianSteps <- median(TotalSteps$steps, na.rm = TRUE)
```
Means Step = 10766.19
Median Step = 10765

##What is the average daily activity pattern?

```r
# Make a time series plot (i.e. type = "1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
library(ggplot2)
MeansStepsByInt <- aggregate(steps ~ interval, CollectedData, mean)
ggplot(data = MeansStepsByInt, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5 min Int") +
  ylab("Avg Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)


```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxInt <- MeansStepsByInt[which.max(MeansStepsByInt$steps),]
```


##Imputing missing values

```r
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missingVals <- is.na(CollectedData$steps)

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```



```r
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
imp_CollectedData <- transform(CollectedData,
                              steps = ifelse(is.na(CollectedData$steps),                                   MeansStepsByInt$steps[match(CollectedData$interval,  MeansStepsByInt$interval)],
CollectedData$steps))

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
impStepsByInt <- aggregate(steps ~ date, imp_CollectedData, FUN=sum)
hist(impStepsByInt$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)



```r
impMeansSteps <- mean(impStepsByInt$steps, na.rm = TRUE)
impMedianSteps <- median(impStepsByInt$steps, na.rm = TRUE)
diffMean = impMeansSteps - MeansSteps
diffMed = impMedianSteps - MedianSteps
diffTotal = sum(impStepsByInt$steps) - sum(TotalSteps$steps)
```


##Are there differences in activity patterns between weekdays and weekends?

```r
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
DayType <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
      return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
      return ("weekend")
  else
      stop ("Invalid Date Format.")
}
imp_CollectedData$date <- as.Date(imp_CollectedData$date)
imp_CollectedData$day <- sapply(imp_CollectedData$date, FUN = DayType)

# Make a panel plot containing a time series plot (i.e. type="1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
MeansStepsByDay <- aggregate(steps ~ interval + day, imp_CollectedData, mean)
ggplot(data = MeansStepsByDay, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)



