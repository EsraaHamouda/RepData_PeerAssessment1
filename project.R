data <- read.csv("D:/crs5/activity.csv")
View(data)
head(data)
str(data)
hist(tapply(data$steps,  data$date , FUN=sum), main = "Histogram", xlab = "Steps")
summary(data)

total_number_steps <- with(data, tapply(steps, as.factor(data$date), sum, na.rm = T))
###############
dataMedian<-median(total_number_steps, na.rm = FALSE)
dataMean<-mean(total_number_steps, na.rm = FALSE)

plot(total_number_steps, type = "l")
######################

steps_by_interval <- aggregate(steps ~ interval, data, mean)
 
plot(steps_by_interval$interval, steps_by_interval$steps  , type = "l")

##################

s<-tapply(data$steps, data$interval) 
m<-max(s)
######################   
cleandata<- data[!is.na(data$steps),]

hist(tapply(cleandata$steps,  cleandata$date , FUN=sum), main = "Histogram", xlab = "Steps")
###########
data$steps[is.na(data$steps)]<-dataMean
head(data)
hist(tapply(data$steps,  data$date , FUN=sum), main = "Histogram2", xlab = "Steps2")
#########################
 newdataMedian<-median(data$steps, na.rm = FALSE)
newdataMean<-mean(data$steps, na.rm = FALSE)
#######################
install.packages("chron")
library(chron)
data$weekend = chron::is.weekend(data$date) 
weekdaysData<-subset(data, data$weekend )
weekendData<-subset(data, !data$weekend )

 #######################
par(mfrow=c(1,2))
hist(tapply(weekdaysData$steps,  weekdaysData$interval , FUN=sum), main = "weekdaysData", xlab = "weekdaysData")

hist(tapply(weekendData$steps,  weekendData$interval , FUN=sum), main = "weekendData", xlab = "weekendData")

