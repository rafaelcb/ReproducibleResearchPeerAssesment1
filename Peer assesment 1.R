setwd("~/Coursera/Reproducible Research/Peer assignment 1")
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
data_non_NA <- data[complete.cases(data), ]
day_total_steps <- tapply(data_non_NA$steps, data_non_NA$date,sum)
hist(day_total_steps, main = "Histogram of steps by day", col = "sky blue",
     xlab = "Steps by day", breaks = 10)
mean(day_total_steps)
median(day_total_steps)
summary(day_total_steps,digits = 8)
five_min_int <- tapply(data_non_NA$steps, data_non_NA$interval,mean)
plot(names(five_min_int), five_min_int, type="l", xlab = "5 min interval", 
     ylab = "Daily average", main = "Average steps per 5 minute interval",
     lwd = 2.5, col = "sky blue")
which.max(five_min_int)
sum(is.na(data$steps))
imputed <- data
imputed[!complete.cases(imputed), "steps"] <- five_min_int[
        as.character(imputed[!complete.cases(imputed), "interval"])]
day_total_steps_imputed<- tapply(imputed$steps, imputed$date,sum)
hist(day_total_steps_imputed, main = "Histogram of steps by day", 
     col = "sky blue", xlab = "Steps by day", breaks = 10)
mean(day_total_steps_imputed)
median(day_total_steps_imputed)
wdays <- weekdays(as.Date(imputed$date))
wdays[wdays == "domingo" | wdays == "sábado"] <- "weekend"
wdays[wdays != "weekend"] <- "weekday"
imputed <- data.frame(imputed, wdays)

library(reshape2)
five_min_int_wday <- with(imputed,tapply(steps, list(interval,wdays),mean))
melted <- melt(five_min_int_wday,id = row.names(five_min_int_wday))
library(ggplot2)
ggplot(melted, aes(x = Var1, y = value, col = Var2)) + geom_line(size = 0.75) + 
        facet_grid(Var2~.) + xlab("5 minute interval") + 
        ylab ("Average number of steps") +
        ggtitle("Average number of steps by 5 minute interval") + 
        theme(legend.position="none")



