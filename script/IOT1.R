# Settings -----
pacman::p_load(chrone, dplyr, plyr, RMySQL, lubridate, ggplot2, reshape2, 
               quantmod, scales, RColorBrewer, sqldf, ggfortify, tidyr, 
               compareDF, reshape, rstudioapi, stringi, plotly)

current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
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

df <- df %>% dplyr::rename(kitchen = Sub_metering_1, laundry = Sub_metering_2, 
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
summary(df$Total_Power) # Because the highest "Total Power" is 11.3 kW/h, the power hiredmust be up to 12 kVA

## Create attributes from "DateTime" ----

df$year <- year(df$DateTime)
df$month <- month(df$DateTime)
df$day <- day(df$DateTime)
df$weekday <- weekdays.POSIXt(df$DateTime)
df$week <- week(df$DateTime)
df$hour <- hour(df$DateTime)
df$minute <- minute(df$DateTime)
df$quarter <- quarter(df$DateTime)
df$Time2 <- as.numeric(hms(df$Time))
df$yearmonth <- as.yearmon(df$DateTime)

#Power Fares----
# Off-peak time is between 02:00 and 07:00; and between 14:00 and 17:00
# Off-peak price per kWh is 0,1230 €
# Peak time is between 17:00 and 02:00; and between 07:00 and 14:00
# Peak Price per kWh is 0,1580 €
normal_fare <- read.csv("dataset/NormalFares.csv")
peak_fare <- read.csv("dataset/PeakFares.csv")

# Creating a new variable for "Peak" consumes ----
x <- as.numeric(hms(c("07:00:00", "10:00:00", "17:00:00", "22:00:00")))

df$tariff <- ifelse(df$Time2 > x[1] & df$Time2 < x[2] | df$Time2 > x[3] & 
                      df$Time2 < x[4], "valey", "peak")
rm(x)
# Time Series plotting by day ----
df2 <- df %>% filter(year > 2006) %>% group_by(Date, Time) %>% 
  summarise(x = sum(Global_active_power/60))
df2 <- df2 %>% group_by(Date) %>% summarise(Energy = sum(x))
df2$year <- year(df2$Date)
df2$month <- month(df2$Date)
df2$monthf <- factor(df2$month, levels = as.character(1:12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                     ordered = TRUE)
df2$week <- week(df2$Date)
df2$yearmonth <- as.yearmon(df2$Date)
df2$yearmonthf <- as.character(df2$yearmonth)
df2$weekday <- as.POSIXlt(df2$Date)$wday
df2$weekdayf <- factor(df2$weekday, levels = rev(0:6), 
                       labels = rev(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", 
                                      "Sat")), ordered = TRUE)
df2$Date <- date(df2$Date)

df2 <- ddply(df2,.(yearmonthf),transform,monthweek=1+week-min(week))

ggplot(df2, aes(monthweek, weekdayf, fill = Energy)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + 
  scale_fill_gradient(low="gold", high="red") + 
  ggtitle("Total Power Consume") + xlab("Week of Month") + ylab("") + 
  theme_bw()

# Now by month ----
df3 <- df2 %>% group_by(yearmonth) %>% summarise(Energy = sum(Energy))
df3$year <- year(df3$yearmonth)
df3$month <- month(df3$yearmonth)
df3$monthf <- factor(df3$month, levels = as.character(1:12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                     ordered = TRUE)

ggplot(df3, aes(x = yearmonth, y = Energy)) + 
  geom_line(color = "#00AFBB", size = 1) + ylab("Energy (kW/h)") +
  xlab("Year") + ggtitle("Energy consumed per month")

df3$Year <- as.factor(df3$year)
ggplot(df3, aes(x = monthf, y = Energy, colour = Year, group = Year)) +
  geom_line() + ylab("Energy (kW/h)") + xlab("Month") +
  ggtitle("Energy consume per month") + geom_point() 

# Now by month and tariff ----
df4 <- df %>% group_by(Date, hour, tariff) %>% summarise(sum_total_power = 
                                                     sum(Global_active_power/60))
df4$yearmonth <- as.yearmon(df4$Date)
df4$PeakPower <- ifelse(df4$tariff == "peak", (df4$sum_total_power), 0)
df4$OffPeakPower <- ifelse(df4$tariff == "valey", (df4$sum_total_power), 0)
df4$PeakCost <- df4$PeakPower * peak_fare$Peak_Price_per_kWh[3]
df4$OffPeakCost <- df4$OffPeakPower * peak_fare$Off_Peak_Price_per_kWh[3]
df4$variable_tariff_cost <- df4$PeakCost + df4$OffPeakCost
df4 <- df4 %>% group_by(yearmonth) %>% 
  summarise(Monthly_fare = sum(variable_tariff_cost) + 
              peak_fare$Subscription_price[3]/12)
head(df4)
df4$year <- year(df4$yearmonth)
df4$month <- month(df4$yearmonth)

ggplot(df4, aes(x = yearmonth, y = Monthly_fare)) + 
  geom_line(color = "#01EC13", size = 1) + ylab("Euros") +
  xlab("") + ggtitle("Peak/Valey tariff monthly fares")

df5 <- df %>% group_by(yearmonth) %>% summarise(sum_total_power = 
                                                  sum(Global_active_power/60))
head(df5)
df5$Monthly_fare <- df5$sum_total_power * normal_fare$Price._per_kWh[4] + 
  (normal_fare$Subscrition_price[4]/12)
df5$year <- year(df5$yearmonth)
df5$month <- month(df5$yearmonth)

ggplot(df5, aes(x = yearmonth, y = Monthly_fare)) + 
  geom_line(color = "#FF5733", size = 1) + ylab("Euros") +
  xlab("") + ggtitle("Normal tariff monthly fares")

df5$sum_total_power <- NULL
df4$Tariff <- "Peak/Valey Tariff"
df5$Tariff <- "Normal Tariff"

comparison <- compare_df(df4, df5, "yearmonth")
comparison$comparison_df
comparison$html_output

df6 <- as.data.frame(rbind(df4,df5))
df6$year <- NULL
df6$month <- NULL

ggplot(data = df6, aes(x = yearmonth, y = Monthly_fare)) + 
  geom_col(color = "blue", fill = "lightblue") + ylab("Monthly bill (€)") +
  facet_wrap(Tariff~., scales = "free") + xlab("Year")

ggplot(df6, aes(x = yearmonth, y = Monthly_fare, colour = Tariff)) +
  geom_line() + ylab("Monthly bill (€)") + xlab("Year")

df7 <- as.data.frame(cbind(df4,df5))
df7[ ,c(3, 4, 6, 8, 9, 10)] <- NULL
colnames(df7) <- c("yearmonth", "Monthly_fare", "Tariff", "Normal_Monthly_fare")
df7$ratio <- round((df7$Monthly_fare/df7$Normal_Monthly_fare)*100,2)

df8 <- as.data.frame(cbind(df4,df5))
df8[ ,c(3, 4, 5, 6, 8, 9)] <- NULL
colnames(df8) <- c("yearmonth", "Monthly_fare", "Normal_Monthly_fare", "Tariff")
df8$ratio <- round((df8$Normal_Monthly_fare/df8$Normal_Monthly_fare)*100,2)
df8 <- df8[ ,c(1, 2, 4, 3, 5)]

df9 <- as.data.frame(rbind(df7,df8))
rm(df7, df8)
df9$Monthly_fare <- NULL
df9$Normal_Monthly_fare <- NULL
ggplot(df9, aes(x = yearmonth, y = ratio, colour = Tariff)) +
  geom_line() + ylab("Normal fare ratio") + xlab("Year")

# Plotting some weeks ----
df7 <- df2 %>% filter(year == 2007, month == 3) %>% 
  group_by(weekday, week, Date) %>% 
  summarise(Energy = sum(Energy))
df7$week <- stringi::stri_datetime_fields(df7$Date)$WeekOfMonth
df7$weekdayx <- factor(df7$weekday, labels = c(7, 1, 2, 3, 4, 5, 
                                               6), ordered = TRUE)
df7$weekdayf <- factor(df7$weekdayx, levels = (1:7), 
                       labels = c("Mon", "Tue", "Wed", "Thu", "Fri", 
                                      "Sat", "Sun"), ordered = TRUE)

ggplot(df7, aes(x = weekdayf, y = Energy, colour = week, group = week)) +
  geom_line() + geom_point() + ylab("Energy (kW/h)") + xlab("Week day") +
  ggtitle("Energy consume per day")

# Plotting by submeters----
df8 <- df %>% filter(year == 2007) %>% group_by(Date, Time) %>% 
  summarise(kitchen_energy = sum(kitchen/1000), laundry_energy = sum(laundry/1000), 
            conditioning_energy = sum(conditioning/1000))
df8$yearmonth <- as.yearmon(df8$Date)

df8 <- df8 %>% group_by(yearmonth) %>% 
  summarise(kitchen_energy = sum(kitchen_energy), 
            laundry_energy = sum(laundry_energy), 
            conditioning_energy = sum(conditioning_energy))
df8$year <- year(df8$yearmonth)
df8$month <- as.integer(month(df8$yearmonth))
df8$monthf <- factor(df8$month, levels = as.character(1:12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                     ordered = TRUE)
df8$yearmonthf <- as.character(df8$yearmonth)

ggplot(df8, aes(x = monthf, group = 1)) + 
  geom_line(aes(y = kitchen_energy), color = "#1A237E") + 
  geom_line(aes(y = conditioning_energy), color = "#C62828") + 
  geom_line(aes(y = laundry_energy), color = "#2E7D32") + 
  ylab("Energy (kW/h)") + xlab("Month") + ggtitle("Energy consumed per submeter")

 # By time of the day ----
df9 <- df %>% group_by(DateTime) %>% 
  summarise(kitchen_energy = sum(kitchen/1000), laundry_energy = sum(laundry/1000), 
            conditioning_energy = sum(conditioning/1000), 
            Global_Energy = sum(Global_active_power/60))

df9$hour <- hour(df9$DateTime)
df9$Energy_no_submetered <- (df9$Global_Energy - df9$kitchen_energy - 
                               df9$laundry_energy - df9$conditioning_energy)
df9$day <- day(df9$DateTime)

df9 <- df9 %>% group_by(day, hour) %>% 
  summarise(kitchen_energy = round(sum(kitchen_energy),0), 
            laundry_energy = round(sum(laundry_energy),0), 
            conditioning_energy = round(sum(conditioning_energy),0), 
            Global_Energy = round(sum(Global_Energy),0), 
            Energy_no_submetered = round(sum(Energy_no_submetered),0))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df9 <- df9 %>% group_by(hour) %>% 
  summarise(kitchen_energy = getmode(kitchen_energy), 
            laundry_energy = getmode(laundry_energy), 
            conditioning_energy = getmode(conditioning_energy), 
            Global_Energy = getmode(Global_Energy), 
            Energy_no_submetered = getmode(Energy_no_submetered))

df9$Hour <- factor(df9$hour, levels = as.character(0:23), 
                   labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                              "10", "11", "12", "13", "14", "15", "16", "17",
                              "18", "19", "20", "21", "22", "23"), 
                   ordered = TRUE)

plot_ly(df9, x = df9$Hour, y = df9$kitchen_energy, name = "Kitchen", 
        type = "scatter", mode = "lines") %>% 
  add_trace(y = df9$conditioning_energy, name = "Conditioning", 
            mode = "lines") %>%
  add_trace(y = df9$laundry_energy, name = "Laundry", mode = "lines") %>%
  add_trace(y = df9$Energy_no_submetered, name = "Not Submetered", 
            mode = "lines") %>%
  add_trace(y = df9$Global_Energy, name = "Global", mode = "lines") %>%
  layout(title = "Representative day of Energy consumed per submeter",
         xaxis = list(title = "Time"), yaxis = list(title = "Energy (kW/h)"))

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

