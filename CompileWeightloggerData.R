

# ******************************************************* #
# Compile weight logger data 
# ******************************************************* #

# ******************************************************* #
# Working directories
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
  #print(p2)
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

#p = ggplot(data = df, aes(x = DateTime)) + geom_line(aes(y = rollmin)) + geom_line(aes(y = Far3), col = "red", size = .2) + theme_classic(); p
#pp = ggplotly(p)
#pp
