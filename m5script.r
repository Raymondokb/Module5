library(ggplot2)
aa = read.csv("D:\\Users\\msboon\\Desktop\\For Data science coursera\\Reproducible Research\\repdata%2Fdata%2Factivity\\activity.csv")
#colnames(aa)

table1 = matrix(data=0, ncol  =4, nrow = length(unique(aa$date)))
colnames(table1) = c("Day", "Total steps", "Mean steps", "Median steps")
all_dates = as.character(unique(aa$date))
for(i in 1:length(all_dates)){
     mdate = aa[which(aa$date==all_dates[i]),]
     table1[i, ] = c(all_dates[i], as.numeric(sum(mdate$steps)),mean(mdate$steps), median(mdate$steps))
}

histTable = data.frame(Date = 1:length(unique(aa$date)), Total_Steps = as.numeric(table1[,2]), stringsAsFactors=FALSE)
#histogram
qplot(na.omit(histTable$Total_Steps), geom="histogram", bins=10,xlab="Total number of steps", ylab = "Frequency", main="Total steps daily")
#median and mean
mean(na.omit(histTable$Total_Steps))
median(na.omit(histTable$Total_Steps))


steps = rep(NA, 61)
day = rep("NA", 61)
stepsday = tapply(aa$steps, aa$date, sum, na.rm=T)
length(stepsday)


for(i in 1:61){
     steps[i] <- stepsday[[i]]
     day[i] = names(stepsday)[i]
}

df = data.frame(day, steps)
head(df)

#hist(df$steps, main = "Total steps by day", xlab = "Day",col = "green" )
qplot(df$steps, xlab = "Day", ylab = "Frequency", main = "Total steps daily", bins = 15)

##Average daily activty pattern
time_series = tapply(aa$steps, aa$interval, mean, na.rm=TRUE)
plot(row.names(time_series), time_series, type="l", xlab = 
          "5-min interval", ylab = "Average across all Days",
     main = "Average number of steps taken", col = "red")

max_interval = which.max(time_series)
names(max_interval)

##Imputing missing values
activity_NA = sum(is.na(aa))
activity_NA


stepsAverage = aggregate(steps ~ interval, data = aa, FUN = mean)
fillNA = numeric()
for(i in 1:nrow(aa)){
     obs = aa[i,]
     if(is.na(obs$steps)){
          steps = subset(stepsAverage, interval == obs$interval)$steps
     } else {
          steps = obs$steps
     }
     fillNA = c(fillNA, steps)
}


new_activity = aa
new_activity$steps = fillNA



stepstotal2 = aggregate(steps ~ date, data = new_activity, sum, na.rm=TRUE)

hist(stepstotal2$steps, main = "Total steps by day", xlab = "Day", col = "red")

mean(stepstotal2$steps)
median(stepstotal2$steps)

library(lubridate)
day = weekdays(as.Date(aa$date))
daylevel = vector()
for(i in 1:nrow(aa)){
     if (day[i] == "Saturday"){
          daylevel[i] = "Weekend"
     } else if (day[i]== "Sunday"){
          daylevel[i] = "Weekend"
     } else{
          daylevel[i] = 'Weekday'
     }
}
aa$daylevel = daylevel
aa$daylevel = factor(aa$daylevel)

stepsbyday = aggregate(steps ~ interval + +daylevel, data = aa, mean)
names(stepsbyday) = c("interval", "daylevel", "steps")
library(ggplot2)
library(lattice)
xyplot(steps ~ interval |daylevel, stepsbyday, type = "l", layout = c(1,2),
       xlab = "Interval", ylab = "Number of steps")
