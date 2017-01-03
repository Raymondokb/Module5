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

qplot(na.omit(histTable$Total_Steps), geom="histogram", bins=30,xlab="Total number of steps")


library(xtable)
library(datasets)
data(airquality)
fit = lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
xt = xtable(summary(fit))
print(xt, type = "html")

