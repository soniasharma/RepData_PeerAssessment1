---
title: "Reproducible Research Course Project I"
author: "Sonia Sharma"
date: "August 12, 2015"
output: html_document
---

### STEP 1. Loading and preprocessing the data


```r
library(ggplot2)
library(grid)
```

Download and unzip the file. Then read the file.

```r
#Url<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#zipfile<-download.file(Url, destfile="ActivityData.zip")
#unzip("ActivityData.zip")
activityData<-read.csv("activity.csv", header=T)
activityData<-transform(activityData, interval=factor(interval))
```

Lets see what the data looks like:

```r
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```



### STEP 2. Mean Number of Steps Per Day

For this part of the assignment, we  ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. Make a histogram of the total number of steps taken each day

3. Calculate the mean and median of the total number of steps taken per day


```r
totalSteps<-aggregate(steps~date, FUN=sum, data=activityData)
ggplot(totalSteps, aes(steps))+geom_histogram(fill="blue")+xlab("Histogram of total number of steps")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

<img src="figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />


```r
mu<-round(mean(totalSteps$steps), digits=1)
M<-median(totalSteps$steps)
```

The mean is 1.07662 &times; 10<sup>4</sup> and the median is 10765. 


### STEP 3. Average Daily Activity Pattern

#### AIM:

In this part we: 

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Report the 5-minute interval which on average across all the days in the dataset, contains the maximum number of steps.




```r
# average number of steps averaged across all days  
averageSteps<-aggregate(steps~interval, FUN=mean, data=activityData) 
ggplot(averageSteps, aes(interval, steps, group=1))+geom_line(color="green")+xlab("Interval")+ylab("Average number of steps across all days") # time-series plot
```

<img src="figure/unnamed-chunk-6-1.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />

```r
# the interval containing the max number of steps
Max<-subset(averageSteps, averageSteps$steps==max(averageSteps$steps)) 
```
The interval which contains the maximum average number of steps across all days (206.1698113 steps), is 835.


### STEP 4. Imputing missing values

#### AIM:
In this part we:

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Use the mean for that 5-minute interval to fill in for the missing values in the dataset. 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day, and calculate and report the mean and median total number of steps taken per day. 

5. Compare the estimates from this part to the first part of the assignment, and  analyse the impact of imputing missing data on the estimates of the total daily number of steps.


```r
count<-sum(is.na(activityData)) # total number of NA values in the data
```
There are 2304 missing values in the data.

The following code creates a new data frame where the missing values are filled in by the mean number of steps taken an interval.

```r
newActivitydf<-activityData # create a copy of the activity data frame
I<-which(is.na(activityData$steps)) #indices corresponding to missing values

#create a 'key-value' list
avgStepsList<-list()
for (i in 1:288) {avgStepsList[[as.character(averageSteps$interval[i])]]<-averageSteps$steps[i]}

# Finally replace NAs with the average number of steps taken in that interval
for (i in I) {newActivitydf$steps[i]<-avgStepsList[[activityData$interval[i] ]] }
```

The new data frame without any missing values looks like: 

```r
str(newActivitydf)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

Now we make a histogram of the total number of steps taken each day from the new data frame and campare it with the histogram in part 1.

```r
totalSteps.new<-aggregate(steps~date, FUN=sum, data=newActivitydf)

# Histogram of total steps for this new imputed data
hist1<-ggplot(totalSteps.new, aes(steps))+geom_histogram(fill="orange")+xlab("Total Steps (Histogram for imputed data)")
# Histogram of total steps for original data with missing values 
hist2<-ggplot(totalSteps, aes(steps))+geom_histogram(fill="blue")+xlab("Total Steps (Histogram for original data with missing values)") +ylab("count") 

#arrange the two histograms in a grid
pushViewport(viewport(layout = grid.layout(2, 1)))
print(hist1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

```r
print(hist2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

<img src="figure/unnamed-chunk-10-1.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" style="display: block; margin: auto;" />

```r
mufill<-round(mean(totalSteps.new$steps), digits=1)
Mfill<-median(totalSteps.new$steps)
```

So for the imputed data, the mean and the median of the total number of steps taken per day are 1.07662 &times; 10<sup>4</sup> and 1.0766189 &times; 10<sup>4</sup> respectively. For the original data, these values were 1.07662 &times; 10<sup>4</sup> and 10765 respecively. It appears that imputing the missing values does not affect the distribution of total number of steps taken each day too much.


### STEP 5. Comparison between weekday and weekend acivity patterns

#### AIM:

For this part we use the dataset with the filled-in missing values to

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
#create a new column variable "Date" with date in the POSIXct class
newActivitydf$Date<-strptime(newActivitydf$date, format="%Y-%d-%m")
#create a new variable "weekdays" in the data frame
newActivitydf$weekdays<-weekdays(newActivitydf$Date)

N<-dim(newActivitydf)[1]
for (i in 1:N) if (newActivitydf$weekdays[i] %in% c("Saturday","Sunday")) newActivitydf$dayType[i] <- "weekend" else newActivitydf$dayType[i] <- "weekday"

# Convert the variable to a factor variable
newActivitydf$dayType<-factor(newActivitydf$dayType)

#create a new df with intervals, daytype and the average steps for each weekday/weekend
averageSteps.dayType<-aggregate(steps~interval+dayType, FUN=mean, data=newActivitydf)

#  time series plots with different panels for weekday and weekend
ggplot(averageSteps.dayType, aes(interval, steps, group=1, color=dayType))+geom_line()+facet_grid(dayType~.)+xlab("Interval")+ylab("Number of Steps")+theme(legend.position="none")
```

<img src="figure/unnamed-chunk-11-1.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" style="display: block; margin: auto;" />

