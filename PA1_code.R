# This code must be run in a directory that has one of 'activity.csv' or the
# activity folder or activity.zip folder.

# Read in the raw data set if not already in environment.
# Check for the data frame, then file, then folder(s) where it may exist.
# If not found, exit the program.
if (exists("rawData")) {
    print("Raw activity data already loaded.")
} else if (file.exists("activity.csv")) {
    print("Found file 'activity.csv'. Reading in data.")
    rawData <- read.csv("activity.csv", header = TRUE)
} else if (file.exists("activity")) {
    print("Found 'activity' folder. Reading in data.")
    rawData <- read.csv("./activity/activity.csv", header = TRUE)
} else if (file.exists("activity.zip")) {
    print("Found 'activity.zip' folder. Extracting and reading in data.")
    unzip("./activity.zip")
    rawData <- read.csv("activity.csv", header = TRUE)
} else {
    stop("Unable to locate data file or folder. Exiting program.")
}

# Copy rawData to activityData.
# Change date column to POSIXct format and discard rawData.
activityData <- rawData
activityData$date <- as.POSIXct(strptime(rawData$date, "%Y-%m-%d"))
intervalTime <- strptime(sprintf("%04d", activityData$interval), "%H%M")
activityData$intervalTime <- as.POSIXct(intervalTime, format = "%H:%M")
rm('rawData')
rm('intervalTime')

# Calculate total number of steps taken each day. Using dplyr for this.
require(dplyr)
dailyStepTotals <- summarise(group_by(activityData, date),
                        totalSteps = sum(steps, na.rm = FALSE))
dailyStepTotals <- dailyStepTotals[which(!is.na(dailyStepTotals$totalSteps)), ]

# Create a histogram of the step totals.
# First, load in ggplot2 for plotting.
require(ggplot2)

# Create histogram.
bw <- 2 * IQR(dailyStepTotals$totalSteps) / length(dailyStepTotals$totalSteps)^(1/3)

h <- {ggplot(dailyStepTotals, aes(x = totalSteps)) +
          geom_histogram(aes(y=..density..), binwidth = bw, color = 'black', 
                         fill = 'slateblue', alpha = .75) +
          geom_density(alpha=.35, fill="firebrick") +
          xlab("Total Steps") +
          ggtitle("Histogram of Daily Step Totals")
}

# Open png graphics device, print out graphic, and close device.
png(file = "stepTotalHist.png")
print(h)
dev.off()

# Calculate the mean and median.
print("Mean Steps Per Day: ")
print(mean(dailyStepTotals$totalSteps))
print("Median Steps Per Day: ")
print(median(dailyStepTotals$totalSteps))

# Make a time series plot of the average steps taken by interval.
require(scales)
intervalMeans <- summarise(group_by(activityData, intervalTime),
                           intMean = mean(steps, na.rm = TRUE))

t <- {ggplot(intervalMeans, aes(x = intervalTime, y = intMean)) + 
          geom_line() +
          scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M")) +
          xlab("Interval") +
          ylab("Average Number of Steps") +
          ggtitle("Average Number of Steps by Daily Interval")
}

# Open png graphics device, print out graphic, and close device.
png(file = "intervalAverages.png")
print(t)
dev.off()

# Print out the interval number with the maximum number of steps.
print("Maximum average number of steps taken during interval: ")
print(select(filter(intervalMeans, intMean == max(intMean)), intervalTime))

# Count the number of rows with NAs.
print("Number of rows with NAs: ")
print(length(which(is.na(activityData$steps))))

# Fill in the NA values with the average for that interval.
imputedActivityData <- activityData
naRows = which(is.na(imputedActivityData$steps))
for (i in seq_along(naRows)) {
    imputedActivityData[naRows[i], 'steps'] =
        intervalMeans[intervalMeans$intervalTime ==
                          imputedActivityData[naRows[i], 'intervalTime'], 'intMean']
}

# Create a histogram and determine mean and median of total steps per day for
# the imputed activity data set.
imputedDailyStepTotals <- summarise(group_by(imputedActivityData, date),
                             totalSteps = sum(steps))

bwI <- 2 * IQR(imputedDailyStepTotals$totalSteps) / length(imputedDailyStepTotals$totalSteps)^(1/3)

hI <- {ggplot(imputedDailyStepTotals, aes(x = totalSteps)) +
          geom_histogram(aes(y=..density..), binwidth = bwI, color = 'black', 
                         fill = 'slateblue', alpha = .75) +
          geom_density(alpha=.35, fill="firebrick") +
          xlab("Total Steps") +
          ggtitle("Histogram of Daily Step Totals with NAs Imputed to Interval Mean")
}

# Open png graphics device, print out graphic, and close device.
png(file = "imputedStepTotalHist.png")
print(hI)
dev.off()

# Calculate the mean and median.
print("The mean and median for the activity data with NAs imputed to the interval mean are: ")
print(mean(imputedDailyStepTotals$totalSteps))
print(median(imputedDailyStepTotals$totalSteps))

# Determine days of week (i.e. if a weekday or weekend) for dates in imputed data set.
weekendRows <- which(weekdays(imputedActivityData$date) %in% c("Saturday", "Sunday"))
imputedActivityData <- mutate(imputedActivityData,
                              dayType = factor(x = "weekday", levels = c("weekday", "weekend")))
imputedActivityData[weekendRows, 'dayType'] <- factor(x = "weekend", levels = c("weekday", "weekend"))

# Create panel (facet) plot of average steps in an interval for weekdays and weekends.
# Make a time series plot of the average steps taken by interval.
imputedIntervalMeans <- summarise(group_by(imputedActivityData, dayType, intervalTime),
                           intMean = mean(steps))
tI <- {ggplot(imputedIntervalMeans, aes(x = intervalTime, y = intMean)) + 
          geom_line() + 
          scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M")) +
          facet_grid(dayType ~ .) +
          xlab("Interval") +
          ylab("Average Number of Steps") +
          ggtitle("Average Number of Steps by Daily Interval")
}

# Open png graphics device, print out graphic, and close device.
png(file = "imputedIntervalAverages.png")
print(tI)
dev.off()