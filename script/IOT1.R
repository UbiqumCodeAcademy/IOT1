# Settings -----
pacman::p_load(chrone, plyr, dplyr, RMySQL, lubridate, ggplot2, reshape2, 
               quantmod, scales, RColorBrewer, sqldf, ggfortify)

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
attr(df$DateTime, "tzone") <- "Europe"

df <- df %>% mutate(Total_Power = Global_active_power + Global_reactive_power)

## Create attributes from "DateTime" ----

df$year <- year(df$DateTime)
df$month <- month(df$DateTime)
df$day <- day(df$DateTime)
df$weekday <- weekdays.POSIXt(df$DateTime)
df$week <- week(df$DateTime)
df$hour <- hour(df$DateTime)
df$minute <- minute(df$DateTime)
df$quarter <- quarter(df$DateTime)
df$Time2 <- hms(df$Time)
df$yearmonth <- as.yearmon(df$DateTime)

# Time Series plotting by day ----
df2 <- df %>% group_by(Date) %>% dplyr::summarise(sum_total_power = sum(Total_Power))
df2$year <- year(df2$Date)
df2$month <- month(df2$Date)
df2$monthf <- factor(df2$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
df2$week <- week(df2$Date)
df2$yearmonth <- as.yearmon(df2$Date)
df2$yearmonthf <- as.character(df2$yearmonth)
df2$weekday <- as.POSIXlt(df2$Date)$wday
df2$weekdayf <- factor(df2$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
df2$Date <- date(df2$Date)

df2 <- ddply(df2,.(yearmonthf),transform,monthweek=1+week-min(week))

ggplot(df2, aes(monthweek, weekdayf, fill = sum_total_power)) + 
  geom_tile(colour = "white") + facet_grid(year~month) + scale_fill_gradient(low="red", high="yellow") +
  ggtitle("Total Power Consume") +  xlab("\n\nWeek of Month") + ylab("") + theme_bw()

# Now by month ----
df3 <- df2 %>% group_by(yearmonth) %>% dplyr::summarise(mean_total_power = mean(sum_total_power))
df3$year <- year(df3$yearmonth)
df3$month <- month(df3$yearmonth)
df3$yearmonthf <- as.character(df3$yearmonth)

ggplot(df3, aes(x = yearmonth, y = mean_total_power)) + 
  geom_line(color = "#00AFBB", size = 1) + ylab("Whatts") +
  xlab("") + ggtitle("Mean power consume per month")

#df %>% filter(year >= 2007 & year <= 2009) %>% group_by(yearmonth) %>% 
  #summarise(sum_global_active_power = sum(conditioning))

# check if there are any time gap ----
df$gap <- c(NA, with(df, hour[-1] - hour[-nrow(df)]))

which(df$gap > 1)

x1 <- df[(c((which(df$gap > 1)-1),(which(df$gap > 1)))),1]
x1 <- as.data.frame(x1)

rm(x1)

df$gap <- NULL

# check if there are any time gap ----
df$gap <- c(NA, with(df, minute[-1] - minute[-nrow(df)]))

which(df$gap > 1)

df$gap <- NULL

 #Power Fares----
# Off-peak time is between 08:00 and 12:00; and between 17:00 and 20:00
# Off-peak price per kWh is 0,1244 €
# Peak time is between 20:00 and 08:00; and between 12:00 and 17:00
# Peak Price per kWh is 0,1593
normal_fare <- read.csv("dataset/NormalFares.csv")
peak_fare <- read.csv("dataset/PeakFares.csv")

# Creating a new variable for "Peak" consumes ----
x <- as.numeric(hms(c("08:00", "12:00", "17:00", "20:00")))

df$tariff <- ifelse(df$Time2 > x[1] & df$Time2 < x[2] | df$Time2 > x[3] & 
                      df$Time2 < x[4], "off_peak", "peak")

calendarheat