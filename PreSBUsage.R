# importing data from SQL as data frame
library(readr)
plugin_device_usage_view <- read_csv("~/Documents/SYS 2202/Final/Aparna PreSB Data/plugin_device_usage_view.csv")
View(plugin_device_usage_view)

# creating new DateTime column based on the timestamp
time <- as.vector(plugin_device_usage_view$timestamp) 
time <- data.matrix(time)
UsageDateTime <- vector(mode = "character", length = 5927)

row <- 1
for(val in time){ # converts and increments through each row
  calc <- val/1000
  final <- as.POSIXct(calc,origin = "1970-01-01",tz ="EST")
  final <- as.character(final)
  UsageDateTime[row] <- final
  row = row + 1
}

library(dplyr)
usage <- mutate(plugin_device_usage_view, date_times = UsageDateTime) # adds column to data frame

# binning by date
Dates <- vector(mode = "character", length = 5927)
row <- 1
for(val in usage$date_times){
  Dates[row] <- substr(val,0,10)
  row = row + 1
}
usage <- mutate(usage, dates = Dates)

# filtering out only the dates I need (pre Spring Break)
usage <- slice(usage, 1:(n()-5973))

# cleaning columns more
usage$`_id` = NULL
usage$timestamp = NULL
usage$date_times = NULL
usage$elapsed_device_off = NULL
usage <- usage[, c(3,1,2)]

# adding total elapsed on
usage <- with(usage, aggregate(list(elapsed_device_on = elapsed_device_on), 
                               list(dates = dates),sum))
usage$elapsed_device_on = usage$elapsed_device_on/1000

# binning by week
usage$week <- ifelse(usage$dates == "2020-01-27","1",
ifelse(usage$dates == "2020-01-28","1",
ifelse(usage$dates == "2020-01-29","1",
ifelse(usage$dates == "2020-01-30","1",
ifelse(usage$dates == "2020-01-31","1",
ifelse(usage$dates == "2020-02-01","1",
ifelse(usage$dates == "2020-02-02","2",
ifelse(usage$dates == "2020-02-03","2",
ifelse(usage$dates == "2020-02-04","2",
ifelse(usage$dates == "2020-02-05","2",
ifelse(usage$dates == "2020-02-06","2",
ifelse(usage$dates == "2020-02-07","2",
ifelse(usage$dates == "2020-02-08","2",
ifelse(usage$dates == "2020-02-09","2",
ifelse(usage$dates == "2020-02-10","3",
ifelse(usage$dates == "2020-02-11","3",
ifelse(usage$dates == "2020-02-12","3",
ifelse(usage$dates == "2020-02-18","4",
ifelse(usage$dates == "2020-02-25","5",
ifelse(usage$dates == "2020-02-26","5",
ifelse(usage$dates == "2020-02-27","5",
ifelse(usage$dates == "2020-02-28","5",
ifelse(usage$dates == "2020-02-29","5",
"0")))))))))))))))))))))))

# binning by means for weeks and plotting
usage <- usage[, c(1,3,2)]
usage <- aggregate(usage[, 3], list(usage$week), mean)
names(usage)[1] <- "week"
names(usage)[2] <- "elapsed time"

