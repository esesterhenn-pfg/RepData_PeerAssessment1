---
title: "Reproducible Research Week 2 Project"
author: "Eric Sesterhenn"
date: "February 5, 2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Reproducible Research Week 2 Project

##Loading and preprocessing the data

Below are the set of code chunks needed to load the data.

The following code chunk loads in the needed R packages.
```{r load_libraries, echo=TRUE}
library(ggplot2)
library(dplyr)
library(lattice)
```

The following code chunk loads in the needed dataset.
```{r load_data, echo=TRUE}
setwd("C:/Users/s109158/Desktop/John Hopkins Course/RepData_PeerAssessment1")
unzip('repdata_data_activity.zip')
data = read.csv('activity.csv')
```

The following code chunk processes the data.
```{r preprocessing, echo=TRUE}
data$date = as.Date(data$date)
data$day = weekdays(data$date)
data$DateTime = as.POSIXct(data$date, format='%Y-%m-%d')
clean_data = data[complete.cases(data),]
```

## What is mean total number of steps taken per day?

Determine the number of steps each day.

```{r steps, echo=TRUE}
step_sum = aggregate(clean_data$steps, by = list(date = clean_data$date), FUN=sum)
colnames(step_sum) = c("Date","Step_Count")
```

Create a histogram of steps each day.
```{r hist_plot, echo=TRUE}
hist(step_sum$Step_Count,xlab = "Steps",main = "Number of Steps in a Day")
```

##What is the average daily activity pattern?
Create a dataframe with average steps per interval.
```{r interval_avg, echo=TRUE}
interval_avg = aggregate(clean_data$steps, by = list(interval = clean_data$interval),FUN=mean)
colnames(interval_avg) = c("interval","steps")
```

Create a time series plot of the steps per interval.
```{r interval_plot, echo=TRUE}
interval_plot = ggplot(interval_avg, aes(x=interval, y=steps), xlab = "Interval", ylab = "Average Steps") + geom_line() + ggtitle("Average Steps per Interval") + ylab("Average Steps")
interval_plot
```

Which 5 minute interval had the most steps
```{r interval_max, echo=TRUE}
interval_avg[interval_avg$steps==max(interval_avg$steps),1]
```

##Imputing missing values into dataframe
Impute the mean value in na values
```{r impute_missing, echo=TRUE}
interval_avg_day = aggregate(clean_data$steps, by = list(interval = clean_data$interval,day = clean_data$day),FUN=mean)
colnames(interval_avg_day) = c("interval","day","steps")
na_rows = data[is.na(data$steps),]
merged_data = merge(na_rows,interval_avg_day,by=c("interval","day"))
new_data = merged_data %>% select(interval,day,steps.y,date,DateTime)
colnames(new_data) = c("interval","day","steps","date","DateTime")
final_data = rbind(data[!is.na(data$steps),],new_data)
```

Compute total number of steps per date
```{r total_steps, echo=TRUE}
total_steps = aggregate(final_data$steps, by = list(date = final_data$date),FUN=sum)
colnames(total_steps) = c("date","steps")
```

Histogram of total steps per date
```{r total_steps_hist, echo=TRUE}
hist(total_steps$steps, xlab = "Steps", main = "Total Steps per Day with Imputed NAs",breaks = 5, col = "red")
hist(step_sum$Step_Count, xlab = "Steps", main = "Total Steps per Day with Imputed NAs",breaks = 5, col = "blue", add = TRUE)
legend("topright",c("Imputed Data","Non-Imputed Data"), fill = c("red","blue"))
```

##Are there differences in activity patterns between weekdays and weekends?

Create variable with weekend/weekday
```{r weekend_weekday, echo=TRUE}
final_data$Day_Type = ifelse(final_data$day %in% c("Saturday","Sunday"), "Weekend","Weekday")
```

Compute average steps over day type, interval
```{r weekend_weekday_avg, echo=TRUE}
interval_avg_dayType = aggregate(final_data$steps, by = list(interval = final_data$interval,Day_Type = final_data$Day_Type),FUN=mean)
colnames(interval_avg_dayType) = c("interval","Day_Type","steps")
```

Plot the weekday and weekend average steps per interval
```{r weekend_weekday_plot, echo=TRUE}
xyplot(steps~interval|Day_Type,data = interval_avg_dayType, type ="l", layout = c(1,2),main = "Average Steps Based on Day Type", ylab = "Average Steps",xlab = "Interval")
```

There does appear to be a difference between the weekends and weekays.