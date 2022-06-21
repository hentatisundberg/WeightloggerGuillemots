

# ******************************************************* #
# Summqrize and plot weight logger data 
# ******************************************************* #


library(ggplot2)


# Plots
start = evdat[1,"day"]
end = evdat[nrow(evdat),"day"]
titlestring = paste("Weights at Farallon 3", "between", start, "&", end, sep = " ")
pd = subset(evdat, Dur > 6 & wmax < 1.2)
n = nrow(pd)
ggplot(data = pd, aes(x = wmax*1000)) + geom_histogram(fill = "red") + ggtitle(titlestring) + scale_x_continuous(name = "Max weight per event [g]") + theme_classic() + annotate("text", x = 800, y = 200, label = paste("n =", n)) 


# NUMBER OF WEIGHT EVENTS OVER TIME                                                                                         
nweight = as.data.frame(table(evdat$day))
nweight$Var1 = as.Date(nweight$Var1)
ggplot(nweight, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", fill = "lightblue", col = NA) + theme_classic() + scale_x_date(name = "Date") + scale_y_continuous(name = "Number of measurements")

# WEIGHT AS A FUNCTION OF SEASON
pd = subset(evdat, w08 < 1.2)
ggplot(pd, aes(x = daynum, group = daynum, y = w08)) + geom_boxplot(fill = "lightblue") + theme_classic()

# WEIGHT AS A FUNCTION OF TIME OF THE DAY
pd = subset(evdat, w08 < 1.2)
ggplot(pd, aes(x = H, group = H, y = w08*1000)) + geom_boxplot(fill = "lightblue") + theme_classic() + scale_y_continuous(name = "Max weight per event [g]") + ggtitle("Weights per hour")
pd = subset(evdat, w08 < 1.2)


# Example weight event
pd = subset(datx, event == 1200)
Time = pd$DateTime[1]
ggplot(data = pd, aes(x = DateTime, y = Far3)) + geom_point() + geom_line() + theme_classic() + ggtitle(paste("Far3", Time, ""))



