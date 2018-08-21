Reproducible Research Project 1
===============================

Loading and preprocessing the data
----------------------------------

##### 1. Load the data (i.e. read.csv())

    fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    zipfile <- "repdata%2Fdata%2Factivity.zip"
    filename <- "repdata%2Fdata%2Factivity"

     if(!file.exists(zipfile)){
        donwload.file(fileurl, destfile = zipfile, method = "curl")
    }

    if(!file.exists(filename)){
        unzip(zipfile)
    }

    act.data <- read.csv("activity.csv", header = TRUE, sep = ",")

##### 2. Process/transform the data (if necessary) into a format suitable for your analysis

    act.data$date <- as.Date(act.data$date, "%Y-%m-%d")

    summary(act.data)

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

------------------------------------------------------------------------

What is mean total number of steps taken per day?
-------------------------------------------------

##### 1. Calculate the total number of steps taken per day

    tot.step.day <- act.data %>%
        group_by(date) %>%
        summarize(tot.day = sum(steps, na.rm = TRUE))

##### 2. Make a histogram of the total number of steps taken each day

    tot.step.day.plot <- with(tot.step.day, 
                             hist(tot.day, col = "cadetblue1",
                                  breaks = seq(0,25000, by=2500),
                                  ylim = c(0,20),
                                  main = "Total number of steps per day",
                                  xlab = "Total steps per day",
                                  ylab = "Frequency"))

![](PA1_files/figure-markdown_strict/unnamed-chunk-4-1.png)

##### 3. Calculate and report the mean and median of the total number of steps taken per day

    tot.step.day.mean <- mean(tot.step.day$tot.day)
    tot.step.day.median <- median(tot.step.day$tot.day)

    print(tot.step.day.mean)

    ## [1] 9354.23

    print(tot.step.day.median)

    ## [1] 10395

------------------------------------------------------------------------

What is the average daily activity pattern?
-------------------------------------------

##### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    ave.step.int <- act.data %>%
            group_by(interval) %>%
            summarize(ave.step = mean(steps, na.rm = TRUE))
        
    ave.step.int.plot <- with(ave.step.int, 
                              plot( x = interval,
                                    y = ave.step,
                                    type = "l",
                                    col = "dodgerblue4",
                                    main = "Average daily activity pattern",
                                    xlab = "Minute",
                                    ylab = "Average Steps Taken"))

![](PA1_files/figure-markdown_strict/unnamed-chunk-6-1.png)

##### 2. Show the 5-minutes interval containing the maximum number of steps.

    max.int <- ave.step.int$ave.step %>%
        which.max() %>%
        ave.step.int[.,"interval"] %>%
        as.numeric()

    print(max.int)

    ## [1] 835

Imputing missing values
-----------------------

##### 1. Calculate and report the total number of missing values in the dataset.

    tot.miss <- act.data$steps %>%
            is.na() %>%
            sum()

    print(tot.miss)

    ## [1] 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.

    data.fill <- data.frame(act.data)
        for (i in seq_along(data.fill$steps)){
            if(is.na(data.fill[i,"steps"])){
                data.fill[i,"steps"] <- ave.step.int$ave.step[which(data.fill[i,"interval"] == ave.step.int$interval)]
            }
        }

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

    tot.step.day.filled <- data.fill %>%
            group_by(date) %>%
            summarize(tot.day = sum(steps, na.rm = TRUE))

##### 4. Make a histogram of the total number of steps taken each day and Calculate.

    tot.day.filled.plot <- with(tot.step.day.filled,
                                hist(x = tot.day ,
                                     breaks = seq(0,25000, by = 2500),
                                     main = "Total number of steps per day (filled)",
                                     xlab = "Total steps per day",
                                     ylab = "Frequency"))

![](PA1_files/figure-markdown_strict/unnamed-chunk-11-1.png)

##### 5. Report the mean and median total number of steps taken per day with filled values.

    tot.step.day.mean.filled <- mean(tot.step.day.filled$tot.day)
    print(tot.step.day.mean.filled)

    ## [1] 10766.19

    tot.step.day.median.filled <- median(tot.step.day.filled$tot.day)
    print(tot.step.day.median.filled)

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    weekdays.list <- with(act.data, sapply(date, function(x){
        if (weekdays(x) == "Saturday"| weekdays(x) == "Sunday")
            { week.type <- "Weekend"} else
            { week.type <- "Weekday"}
        week.type
    }))

    act.data.week <- cbind(act.data, weekdays.list)

    ave.step.int.week <- act.data.week %>%
        group_by(interval,weekdays.list) %>%
        summarize(step.int.week = mean(steps, na.rm = TRUE))

##### 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

    week.plot <- ggplot(ave.step.int.week, aes(x = interval, y = step.int.week, color = weekdays.list)) + 
            geom_line() +
            facet_wrap( ~weekdays.list, ncol = 1, nrow = 2) +
            xlab("Minutes") +
            ylab("Averaged number of steps taken") +
            ggtitle("Average number of steps taken, averaged across all weekday days or weekend days")

    print(week.plot)

![](PA1_files/figure-markdown_strict/unnamed-chunk-15-1.png)
