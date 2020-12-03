---
title: "Reproducible Research Project_1"
author: "DMuriungi"
date: "12/3/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Loading and preprocessing the data

```{r echo=FALSE}
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,destfile = "C:/Users/DMuriungi/Desktop/Doris/Doris DOCs/dm/Data Science/Reproducible Research/activity.zip")
unzip("C:/Users/DMuriungi/Desktop/Doris/Doris DOCs/dm/Data Science/Reproducible Research/activity.zip")
act_dat<-read.csv("C:/Users/DMuriungi/Desktop/Doris/Doris DOCs/dm/Data Science/Reproducible Research/activity.csv",stringsAsFactors = F)
head(act_dat)
```

Loading libraries

```{r} 
library(ggplot2)
library(lubridate)
library(dplyr)
```

Coverting dates to date format 

```{r}
act_dat$date<-as.Date(act_dat$date)
```

Mean total number of steps taken per day

```{r}
total_steps<-sum(act_dat$steps,na.rm = TRUE)
total_steps

````

Histogram of the total number of steps taken each day

```{r}
total_steps_day<-aggregate(steps~date,data=act_dat, FUN = sum,na.rm=TRUE)
qplot(total_steps_day$steps,main = "Total number of steps taken each day",xlab = "No. of Steps")
```

Mean of the total number of steps taken per day
 
```{r}
 steps_mean<-mean(total_steps_day$steps)
 
```
 
Median of the total number of steps taken per day
  
```{r}
   steps_median<-median(total_steps_day$steps)
```

Average daily activity pattern

```{r}
avg_mean_step<-aggregate(steps~interval,data = act_dat, FUN = mean,na.rm=TRUE)
ggplot(avg_mean_step,aes(interval,steps)) + geom_line() + ggtitle("Average daily activity pattern")+xlab("5 Minute Intervals")+ylab("Average No. of Steps")
```

5-minute interval, on average across all the days in the dataset that contains the maximum number of steps

```{r}
maxInterval<-avg_mean_step[which.max(avg_mean_step$steps),]

```

Imputing missing values

```{r}
#Total number of rows with missing values
misval<-is.na(act_dat$steps)

sum(misval)
```


Filling in all of the missing values in the dataset
 
```{r}
act_dat_imp<-act_dat %>% transform( steps = ifelse(is.na(act_dat$steps),
                                             avg_mean_step$steps[match(act_dat$interval, 
                                                                                          avg_mean_step$interval)],
                                            act_dat$steps))
str(act_dat_imp)
```
 
Histogram of the total number of steps taken each day using dataset with imputed missing values

```{r}
total_steps_day_imp<-aggregate(steps~date,data=act_dat_imp, FUN = sum,na.rm=TRUE)
qplot(total_steps_day_imp$steps,main = "Total number of steps taken each day",xlab = "No. of Steps")
```

Comupting mean and median total number of steps taken per day

```{r}
steps_mean_imp<-mean(total_steps_day_imp$steps)
                     
steps_median_imp<-median(total_steps_day_imp$steps)

#Checking for difference between the mean and Median before and after imputing missing values
diff_mean<-steps_mean-steps_mean_imp
diff_mean
diff_median<-steps_median-steps_median_imp
diff_median

#Difference in total daily number of steps?
diff_total_steps<-sum(total_steps_day$steps)-sum(total_steps_day_imp$steps)
diff_total_steps
```
 
 There is no difference between the mean.
 The medians have a -1.188679 difference
 The total steps have a -86129.51 difference
 
 Differences in activity patterns between weekdays and weekends
 
```{r}
 #Creating a new factor variable in the dataset with two levels – “weekday” and “weekend”
day_type <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
      return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
      return ("weekend")
  else
      stop ("Date Format Invalid.")
}
#Converting date into date format
act_dat_imp$date <- as.Date(act_dat_imp$date)
act_dat_imp$day <- sapply(act_dat_imp$date, FUN = day_type)
```
 
Panel plotting
```{r} 
 #Panel plot containnig a time-series plot of the 5-minute interval
#and the average number of steps taken across all weekdays or weekends
steps_by_day_mean <-aggregate(steps ~ interval+day, act_dat_imp, mean)
ggplot(data = steps_by_day_mean, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5 Minute Interval") +
  ylab("Average No. of Steps") +
        theme_bw()
```
 

