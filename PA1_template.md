PA1\_template
================
Sreepriya Premmohan
April 02,2016

install.packages("rmarkdown", type = "source")

Loading and preprocessing the data
----------------------------------

library(ggplot2) library(plyr) activity \<- read.csv("activity.csv") activity\(day <- weekdays(as.Date(activity\)date)) activity\(DateTime<- as.POSIXct(activity\)date, format="%Y-%m-%d")

What is mean total number of steps taken per day?
-------------------------------------------------

pulling data without nas
------------------------

clean \<- activity[!is.na(activity$steps),]

summarizing total steps per date
--------------------------------

sumTable \<- aggregate(activity\(steps ~ activity\)date, FUN=sum, ) colnames(sumTable)\<- c("Date", "Steps")

Creating the historgram of total steps per day
----------------------------------------------

hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")

Mean of Steps
-------------

as.integer(mean(sumTable$Steps))

Median of Steps
---------------

as.integer(median(sumTable$Steps))

What is the average daily activity pattern?
-------------------------------------------

create average number of steps per interval
-------------------------------------------

interval \<- ddply(clean, .(interval), summarize, Avg = mean(steps))

Create line plot of average number of steps per interval
--------------------------------------------------------

p \<- ggplot(interval, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps") p + geom\_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

Maximum steps by interval
-------------------------

maxStepsinter \<- max(interval$Avg)

Which interval contains the maximum average number of steps
-----------------------------------------------------------

interval[interval$Avg==maxStepsinter,1]

Imputing missing values
-----------------------

Number of NAs in original data set
----------------------------------

nrow(activity[is.na(activity$steps),])

Create the average number of steps per weekday and interval
-----------------------------------------------------------

avgdata \<- ddply(clean, .(interval, day), summarize, Avg = mean(steps))

Create dataset with all NAs for substitution
--------------------------------------------

nadata\<- activity[is.na(activity$steps),]

Merge NA data with average weekday interval for substitution
------------------------------------------------------------

newdata\<-merge(nadata, avgdata, by=c("interval", "day"))

Reorder the new substituded data in the same format as clean data set
---------------------------------------------------------------------

newdata2\<- newdata[,c(6,4,1,2,5)] colnames(newdata2)\<- c("steps", "date", "interval", "day", "DateTime")

Merge the NA averages and non NA data together
----------------------------------------------

mergeData \<- rbind(clean, newdata2)

Create sum of steps per date to compare with step 1
---------------------------------------------------

sumTable2 \<- aggregate(mergeData\(steps ~ mergeData\)date, FUN=sum, ) colnames(sumTable2)\<- c("Date", "Steps")

Mean of Steps with NA data taken care of
----------------------------------------

as.integer(mean(sumTable2$Steps))

Median of Steps with NA data taken care of
------------------------------------------

as.integer(median(sumTable2$Steps))

Creating the histogram of total steps per day, categorized by data set to show impact
-------------------------------------------------------------------------------------

hist(sumTable2\(Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Blue") hist(sumTable\)Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T) legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("blue", "grey") )

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Create new category based on the days of the week
-------------------------------------------------

mergeData\(DayCategory <- ifelse(mergeData\)day %in% c("Saturday", "Sunday"), "Weekend", "Weekday") library(lattice)

Summarize data by interval and type of day
------------------------------------------

intervalTable2 \<- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

Plot data in a panel plot
-------------------------

xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l", layout = c(1,2), main="Average Steps per Interval Based on Type of Day", ylab="Average Number of Steps", xlab="Interval")
