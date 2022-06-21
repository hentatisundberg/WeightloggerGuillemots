

# ******************************************************* #
# Saummrize and plot
# ******************************************************* #

# ******************************************************* #

library(ggplot2)


# Plots
ggplot(data = evdat, aes(x = Dur, y = w08, label = event)) + geom_text()

pd = subset(evdat, Dur > 6 & wmax < 1.2)
n = nrow(pd)
ggplot(data = pd, aes(x = wmax*1000)) + geom_histogram(fill = "red") + ggtitle("Weights at Farallon 3, 4 - 29 maj 2022") + scale_x_continuous(name = "Max weight per event [g]") + theme_classic() + annotate("text", x = 800, y = 200, label = paste("n =", n)) 


# Weight as a function of time of the day
evdat$H = as.numeric(format(evdat$Start, "%H"))
evdat$daynum = as.numeric(format(evdat$Start, "%j"))
evdat$day = as.Date(evdat$Start)


# NUMBER OF WEIGHT EVENTS OVER TIME                                                                                         
nweight = as.data.frame(table(evdat$day))
nweight$Var1 = as.Date(nweight$Var1)
ggplot(nweight, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", col = NA) + theme_classic() + scale_x_date(name = "Date") + scale_y_continuous(name = "Number of measurements")



ggplot(evdat, aes(x = daynum, group = daynum, y = w08)) + geom_boxplot(fill = "lightblue") + theme_classic()
ggplot(evdat, aes(x = H, group = H, y = w08*1000)) + geom_boxplot(fill = "lightblue") + theme_classic() + scale_y_continuous(name = "Max weight per event [g]") + ggtitle("Weights per hour")


# Histogram of weights 
ggplot(evdat, aes(x = w08)) + geom_histogram(fill = "red") + theme_classic()



# Example weight
pd = subset(datx, event == 236)
Time = pd$DateTime[1]
ggplot(data = pd, aes(x = DateTime, y = Far3)) + geom_point() + geom_line() + theme_classic() + ggtitle(paste("Far3", Time, ""))



