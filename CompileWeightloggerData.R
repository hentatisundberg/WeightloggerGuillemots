

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

path = paste(macdir, "research/seabird/WeightLoggerGuillemots/data/", sep = "")
allfiles = list.files(path)

df = data.frame()
for (i in allfiles) {
  p2 = paste(path, (i), sep = "")
  p3 = read.csv(p2, sep = ";")
  p3$DateTime = as.POSIXct(paste(p3$Date, p3$Time), format = "%d/%m/%Y %H:%M:%S")
  p3 = p3[!duplicated(p3$DateTime),]
  df = rbind.fill(df, p3)
}

df = df[,c(15, 4:6, 17)]
df = df[!is.na(df[,"Far3"]),]

# Change data format
df$Far3 = as.numeric(df$FARALLON3)
df$Bon1 = as.numeric(df$BONDEN1)
df$Toms = as.numeric(df$TOMS)
df$Far8D = as.numeric(df$FAR8D)


# Post-calibration function for scales that are being re-calibrated because of bird colissions or other reasons
calib_scale = function(inputdata, windowsize) {
  rollmin = vector(length = length(inputdata))
  end = length(inputdata - windowsize)
  for (i in 1:end) { rollmin[i] = min(inputdata[i:(i+windowsize)]) }
  
  # Span gaps in rollmin
  rollmin_spangaps = approx(rollmin, xout = 1:length(rollmin), method = "constant")$y
  
  # Calibrated data 
  output = inputdata - rollmin_spangaps
  
  # Return
  return(output)
    
}

# Calibrate Far3 scale
df$Far3c = calib_scale(df$Far3, 1500)


inputdata = df$Toms 


# Function for defining weight events 
eventnum = function(inputdata, threshold) {
  birdon = ifelse(inputdata > threshold, 1, 0)
  event = diff(birdon); event[event == -1] <- 0; event[is.na(event)] <- 0
  eventnum = cumsum(event)
  eventnum[birdon == 0] <- NA
  
  return(eventnum)
}


# Define events 
df$F3event = eventnum(df$Far3c, .7) 
df$TLevent = eventnum(df$Toms, .7)
df$F8event = eventnum(df$Far8D, .7)
df$B1event = eventnum(df$Bon1, .7)


# Long format better for aggregating data for separate ledges 
dflong = data.frame(DateTime = rep(df$DateTime, 4), Ledge = rep(c("Far3", "Bon1", "Toms", "Far8"), each = nrow(df)), rbind(data.frame(w = df$Far3, e = df$F3event), data.frame(w = df$Bon1, e = df$B1event), data.frame(w = df$Toms, e = df$TLevent), data.frame(w = df$Far8, e = df$F8event)))

# Remove NA
dflong = dflong[!is.na(dflong$DateTime) & dflong$e > 0,]

# Stats for each each weight event 
evdat = aggregate(w ~ Ledge+e, data = dflong, FUN = function(x) quantile(x, probs = .8));  
s2 = aggregate(DateTime ~ Ledge+e, data = dflong, FUN = min); 
s3 = aggregate(DateTime ~ Ledge+e, data = dflong, FUN = max);
evdat[,c("Start", "End")] = data.frame(s2$DateTime, s3$DateTime)


# Weight as a function of time of the day etc. 
evdat$Dur = difftime(evdat$End, evdat$Start, "secs")
evdat$Day = format(evdat$Start, "%m-%d")
evdat$H = as.numeric(format(evdat$Start, "%H"))
evdat$daynum = as.numeric(format(evdat$Start, "%j"))
evdat$day = as.Date(evdat$Start)

