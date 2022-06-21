

# ******************************************************* #
# Compile weight logger data 
# ******************************************************* #

# ******************************************************* #
# Local working directory
macdir <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/data/work - synced/"


# ******************************************************* #
# Install packages
library(ggplot2)
library(plyr)
library(reshape2)
library(gdata)
library(plotly)


# ******************************************************* #
#### READ DATA ####

path = paste(macdir, "research/seabird/Weights/data/", sep = "")
allfiles = list.files(path)

df = data.frame()
for (i in allfiles) {
  p2 = paste(path, (i), sep = "")
  p3 = read.csv(p2, sep = ";")
  df = rbind.fill(df, p3)
}

df = df[,c(1, 2, 4:6)]
df$DateTime = as.POSIXct(paste(df$Date, df$Time), format = "%d/%m/%Y %H:%M:%S")
df$Far3 = as.numeric(df$FARALLON3)
df$Bon1 = as.numeric(df$BONDEN1)
df$Toms = as.numeric(df$TOMS)

# New calibration method - forward rolling min value
inputdata = df$Far3 

windowsize = 1500
rollmin = vector(length = length(inputdata))
end = length(inputdata - windowsize)
for (i in 1:end) {
  rollmin[i] = min(inputdata[i:(i+windowsize)])
}

# Span gaps in rollmin
rollmin_spangaps = approx(rollmin, xout = 1:length(rollmin), method = "constant")$y
df$rollmin = rollmin_spangaps



# Weight events
datx = df[!is.na(df$Far3c),]
datx = df[df$Far3c > -.1 & df$Far3c < 2,]

birdon = ifelse(datx$Far3c > .7, 1, 0)
event = diff(birdon); event[event == -1] <- 0; event[is.na(event)] <- 0
eventnum = cumsum(event)
eventnum[birdon == 0] <- NA
datx$event = eventnum

# Stats for each each weight event 
s1 = aggregate(cbind(DateTime, Far3c) ~ event, data = datx, FUN = max); s1$wmax = s1$Far3c
s2 = aggregate(cbind(DateTime, Far3c) ~ event, data = datx, FUN = min); s2$wmin = s2$Far3c
s3 = aggregate(Far3c ~ event, data = datx, FUN = median); s3$wmedian = s3$Far3c
s4 = aggregate(Far3c ~ event, data = datx, FUN = function(x) quantile(x, probs = .8)); s4$w08 = s4$Far3c
s1$End = as.POSIXct(s1$DateTime, origin = "1970-01-01")
s2$Start = as.POSIXct(s2$DateTime, origin = "1970-01-01")
evdat = data.frame(s1$event, s2$Start, s1$End, s2$wmin, s1$wmax, s3$wmedian, s4$w08)
colnames(evdat) = c("event", "Start", "End", "wmin", "wmax", "wmedian", "w08")
evdat$Dur = as.numeric(difftime(evdat$End, evdat$Start))
evdat$Day = format(evdat$Start, "%m-%d")


# Weight as a function of time of the day etc. 
evdat$H = as.numeric(format(evdat$Start, "%H"))
evdat$daynum = as.numeric(format(evdat$Start, "%j"))
evdat$day = as.Date(evdat$Start)

