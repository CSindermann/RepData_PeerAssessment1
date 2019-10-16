Preparing Data
==============

Downloading data from Internet and reading it into R:

    #download
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 
    tempFile <- tempfile()
    download.file(url, tempFile, method = "curl")
    #read into R
    Data_Assignment <- read.csv(unz(tempFile, "activity.csv"), header = TRUE)
    unlink(tempFile)

Columns of the file are not in the appropriate format; this needs to be
changed

    Data_Assignment$steps <- as.numeric(Data_Assignment$steps)
    Data_Assignment$date <- as.Date(Data_Assignment$date)
    Data_Assignment$interval <- as.numeric(Data_Assignment$interval)

What to see from the data:

1.  There are 61 different days  
2.  Each interval occurs 61 times (hence, for each day)

Questions / Assignments:
========================

What is mean total number of steps taken per day?
-------------------------------------------------

1.  Calculate the total number of steps taken per day

<!-- -->

    Total_Steps_Day <- aggregate(Data_Assignment[,1], by=list(Data_Assignment$date), FUN=sum, na.rm=TRUE)

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    hist(Total_Steps_Day[,2],
         main = "Histogram of steps taken per day",
         xlab="Number of steps taken per day")

![](PA1_template_files/figure-markdown_strict/StepsHisto-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    #mean
    mean(Total_Steps_Day[,2])

    ## [1] 9354.23

    #median
    median(Total_Steps_Day[,2])

    ## [1] 10395

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e. type = “l”) of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)

<!-- -->

    #Calculate steps per intervall across all days
    Mean_Steps_Interv <- aggregate(Data_Assignment[,1], by=list(Data_Assignment$interval), FUN=mean, na.rm=TRUE)

    #design time series plot
    plot(Mean_Steps_Interv[,1], Mean_Steps_Interv[,2], type="l", 
         main="Mean number of steps taken per interval across days", 
         xlab ="Time interval",
         ylab="Mean number across days")

![](PA1_template_files/figure-markdown_strict/StepsInterv-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    #search interval with highest value in "Mean_Steps_Interv"
    max_MeanStep <- max(Mean_Steps_Interv[,2], na.rm=TRUE)
    #Highest value is above 206 --> Therefore, select case were x is greater than 206
    max_interval <- subset(Mean_Steps_Interv, x > 206)

Time interval 835 has highest mean steps taken across days.

Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

<!-- -->

    #Count NAs across all rows and columns
    sum(is.na(Data_Assignment))

    ## [1] 2304

    #Count NAs within each column
    sum(is.na(Data_Assignment[,1])) 

    ## [1] 2304

    sum(is.na(Data_Assignment[,2])) # 0 NAs

    ## [1] 0

    sum(is.na(Data_Assignment[,3])) # 0 NAs

    ## [1] 0

The total number of NAs and also the total number of rows with NAs is
2304 (all NAs are in first column)

1.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

<!-- -->

    #I use the mean steps per interval to be imputed for the NAs
    Mean_Steps_Interv_2 <- aggregate(steps ~ interval, data=Data_Assignment, FUN=mean)
    Impute_NA <- numeric()
    for (i in 1:nrow(Data_Assignment)) {
        obs <- Data_Assignment[i, ]
        if (is.na(obs$steps)) {
            steps <- subset(Mean_Steps_Interv_2, interval == obs$interval)$steps
        } else {
            steps <- obs$steps
        }
        Impute_NA <- c(Impute_NA, steps)
    }

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    Imputed_Data_Assignment <- Data_Assignment
    Imputed_Data_Assignment$steps <- Impute_NA

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    #1. Calculate total number of steps taken each day (for the imputed dataset)
    Total_Steps_Day_2 <- aggregate(Imputed_Data_Assignment[,1],
                                   by=list(Imputed_Data_Assignment$date), FUN=sum, na.rm=TRUE)

    #2. Build histogram 
    hist(Total_Steps_Day_2[,2], 
         main="Histogram of steps taken per day for the imputed dataset", 
         xlab="Number of steps taken per day (imputed dataset)")

![](PA1_template_files/figure-markdown_strict/Histo_Etc-1.png)

    #3. Calculate mean
    mean(Total_Steps_Day_2[,2]) #10766.19

    ## [1] 10766.19

    #4. Calculate median
    median(Total_Steps_Day_2[,2]) #10766.19

    ## [1] 10766.19

    # Do mean and median differ (without vs. with imputed data)?
    # New mean and median (imputed data): 10766.19; old mean and median (no imputed data): 9354.23, 10395

New mean and median are the same AND both are higher than previous mean
and median

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    #Built days (7 days)
    Days <- weekdays(Data_Assignment$date)

    #Built Weekdays vs. Weekend
    Daylevel <- vector()
    for (i in 1:nrow(Data_Assignment)) {
        if (Days[i] == "Saturday") {
            Daylevel[i] <- "Weekend"
        } else if (Days[i] == "Sunday") {
            Daylevel[i] <- "Weekend"
        } else {
            Daylevel[i] <- "Weekday"
        }
    }

    Data_Assignment$Daylevel <- Daylevel
    Data_Assignment$Daylevel <- as.factor(Data_Assignment$Daylevel)

1.  Make a panel plot containing a time series plot (i.e. type = “l”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- -->

    StepsWeekEnd <- aggregate(steps ~ interval + Daylevel, data = Data_Assignment, FUN=mean)

    library(lattice)
    xyplot(steps ~ interval | Daylevel, StepsWeekEnd, type = "l", layout = c(2, 1), 
        xlab = "Time interval", ylab = "Mean number of steps")   

![](PA1_template_files/figure-markdown_strict/LinePlot-1.png)
