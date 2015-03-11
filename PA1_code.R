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
# rm('rawData')

# Calculate total number of steps taken each day. Using dplyr for this.
require(dplyr)
dailyStepTotals <- summarise(group_by(activityData, date),
                        totalSteps = sum(steps, na.rm = TRUE))

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