# Settings -----
pacman::p_load(chrone, dplyr, RMySQL, lubridate)

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(current_path)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

 ## Data ser INFORMATION -----
# sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered). 
# sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light. 
# sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

## Load data-----
j <- c("yr_2006", "yr_2007", "yr_2008", "yr_2009", "yr_2010") # Should it be only from 2007 to 2009?
df <- c()
for (i in 1:length(j)) {
  X <- dbGetQuery(con, 
                  paste("SELECT * FROM ",
                        j[i]))
  df <- rbind(df,X)
}
rm(X, i, j)
df$id <- NULL

df <- df %>% rename(kitchen = Sub_metering_1, laundry = Sub_metering_2, 
                    conditioning = Sub_metering_3)

head(df)
tail(df)

## Combine Date and Time attribute values in a new attribute column ----
df <- cbind(df,paste(df$Date,df$Time), stringsAsFactors = FALSE)
colnames(df)[10] <- "DateTime"
df <- df[,c(ncol(df), 1:(ncol(df)-1))]
df$DateTime <- as.POSIXct(df$DateTime, "%Y/%m/%d %H:%M:%S")
attr(df$DateTime, "tzone") <- "Europe/Paris"

## Create attributes from "DateTime" ----

df$year <- as.integer(year(df$DateTime))
df$month <- as.integer(month(df$DateTime))
df$day <- day(df$DateTime)
df$weekday <- weekdays.POSIXt(df$DateTime)
df$week <- as.integer(week(df$DateTime))
df$hour <- as.integer(hour(df$DateTime))
df$minute <- as.integer(minute(df$DateTime))
df$quarter <- as.integer(quarter(df$DateTime))

# check if there are any time gap ----
df$gap <- c(NA, with(df, minute[-1] - minute[-nrow(df)]))

which(df$gap > 1)

df$gap <- NULL

 #Power Fares----
# Off-peak time is between 23:30 and 7:30
# Off-peak price per kWh is 0,1244 €
# Peak time is between 7:31 and 23:29
# Peak Price per kWh is 0,1593
normal_fare <- read.csv("dataset/NormalFares.csv")
peak_fare <- read.csv("dataset/PeakFares.csv")

