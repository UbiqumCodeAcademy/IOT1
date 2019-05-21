# General comments

# take care with variable names
#

# Settings -----
pacman::p_load(chron, dplyr, plyr, RMySQL, lubridate, ggplot2, reshape2, 
               quantmod, scales, RColorBrewer, sqldf, ggfortify, tidyr, 
               compareDF, reshape, rstudioapi, stringi, plotly, padr, 
               DescTools, anytime, ggfortify, forecast, tslm)

current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# Data set INFORMATION -----
 ## sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered). 
 ## sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light. 
 ## sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

# Load data-----
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
  # Only 0.65 % of the time the total power needed is higher than 5.5 kVA. By hiring 6kVA, customer will reduce the power bill up to 50€ a year.
summary(df$Global_intensity)
summary(df$Voltage) # Voltage is a bit higher than expected, as the standard voltage in France is 230V

df$year <- year(df$DateTime) # To filter in or out the years

# check if there are any time gap----
df$gap <- c(NA, with(df, DateTime[-1] - DateTime[-nrow(df)]))

which(df$gap > 1)

x1 <- df[(c((which(df$gap > 3)-1),(which(df$gap > 3)))),1]
x1 <- as.data.frame(x1)

rm(x1)
df$gap <- NULL

# PAD function (package PADR) to "fill the gaps" with NAs----
df1 <- rbind(df %>% filter(year == 2007) %>% 
               pad(), df %>% filter(year == 2008) %>% 
               pad(), df %>% filter(year == 2009) %>% 
               pad(), df %>% filter(year == 2010) %>%
               pad())

df1$year <- NULL

# Fill NAs with data ----
  # For the ones that are less than three minutes:
for (i in 4:ncol(df1)){
  df1[ ,i] <- na.locf(df1[ ,i], maxgap = 3)
  } #We consider that the 3 min gap is the time the meters and submeters need for software updates.

  # For all the others
for (i in 4:ncol(df1)) {
  df1[which(is.na(df1[ ,i]) == TRUE), i] <- Mode(df1[ ,i])
}

# Create attributes from "DateTime" ----

df1$Date <- date(df1$DateTime)
df1$year <- year(df1$DateTime)
df1$month <- month(df1$DateTime)
df1$day <- day(df1$DateTime)
df1$weekday <- weekdays.POSIXt(df1$DateTime)
df1$week <- week(df1$DateTime)
df1$hour <- hour(df1$DateTime)
df1$minute <- minute(df1$DateTime)
df1$quarter <- quarter(df1$DateTime)
df1$Time <- strftime(df1$DateTime,format="%H:%M:%S")
df1$Time2 <- as.numeric(hms(df1$Time))
df1$yearmonth <- as.yearmon(df1$DateTime)

# z <- zoo(1:nrow(df1), as.POSIXct(c(df1$DateTime)))
# g <- seq(start(z), end(z), by = "min")
# na.locf(z, xout = g)

## Power Fares----
# Off-peak time is between 02:00 and 07:00; and between 14:00 and 17:00
# Off-peak price per kWh is 0,1230 €
# Peak time is between 17:00 and 02:00; and between 07:00 and 14:00
# Peak Price per kWh is 0,1580 €
normal_fare <- read.csv("dataset/NormalFares.csv")
peak_fare <- read.csv("dataset/PeakFares.csv")

## Creating a new variable for "Peak" consumes ----
x <- as.numeric(hms(c("02:00:00", "07:00:00", "14:00:00", "17:00:00")))

df1$tariff <- ifelse(df1$Time2 > x[1] & df1$Time2 < x[2] | df1$Time2 > x[3] & 
                      df1$Time2 < x[4], "valey", "peak")
rm(x)

# Splitting the data by day ----
df2 <- df1 %>% filter(year > 2006) %>% group_by(Date, Time) %>% 
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

 #Converting it to a Time Series
df2ts <- ts(df2$Energy, frequency = 365, start = c(2007,1))

autoplot(df2ts)
 ## Applying time series linear regression to the Time series:

fit_df2 <- tslm(df2ts ~ trend + season)
summary(fit_df2)
plot(forecast(fit_df2, h=5))

checkresiduals(fit_df2)
CV(fit_df2) # Horrible!

## Decomposing the Time series:
decomposed_df2ts <- decompose(df2ts)
plot(decomposed_df2ts)
summary(decomposed_df2ts)
decomposed_df2ts$random

x <- stl(df2ts, "periodic")
seasonal_stl_df2ts <- x$time.series[,1]
trend_stl_df2ts <- x$time.series[,2]
random_stl_df2ts <- x$time.series[,3]
y <- (sum(abs(x$time.series[,3])))/nrow(df2) # Absolute Mean Error

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

 #Converting it to a Time Series
df3ts <- ts(df3$Energy, frequency = 12, start = c(2007,1))

plot.ts(df3ts)
autoplot(df3ts)

## Applying time series linear regression to the Time series:

fit_df3 <- tslm(df3ts ~ trend + season)
summary(fit_df3)
plot(forecast(fit_df3, h=5, level=c(80,90)))

checkresiduals(fit_df3)
CV(fit_df3)

## Decomposing the Time series:
decomposed_df3ts <- decompose(df3ts)
plot(decomposed_df3ts)
summary(decomposed_df3ts)
decomposed_df3ts$random

x <- stl(df3ts, "periodic")
seasonal_stl_df3ts <- x$time.series[,1]
trend_stl_df3ts <- x$time.series[,2]
random_stl_df3ts <- x$time.series[,3]
y <- (sum(abs(x$time.series[,3])))/nrow(df3) # Absolute Mean Error

## Applying Holt-Winters to the Time series:
adjusted_df3ts <- df3ts - decomposed_df3ts$seasonal
autoplot(adjusted_df3ts)
plot(decompose(adjusted_df3ts))

df3ts_HW <- HoltWinters(adjusted_df3ts, beta=FALSE, gamma=FALSE)
plot(df3ts_HW, ylim = c(575, 950))

 ## Forecast Holt-Winters:
df3ts_HW_forecast<- forecast(df3ts_HW, h=5)
plot(df3ts_HW_forecast)

# Now by month and tariff ----
df4 <- df1 %>% group_by(Date, hour, tariff) %>% summarise(sum_total_power = 
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

df5 <- df1 %>% group_by(yearmonth) %>% summarise(sum_total_power = 
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

#Converting it to a Time Series
df4ts <- ts(df4$Monthly_fare, frequency = 12, start = c(2007,1))
df5ts <- ts(df5$Monthly_fare, frequency = 12, start = c(2007,1))

plot.ts(df3ts)
autoplot(df4ts)

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
rm(df4, df5, df7, df8)
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

# Plotting for a representative day ----
df9 <- df1 %>% group_by(DateTime, weekday) %>% 
  summarise(kitchen_energy = sum(kitchen/1000), laundry_energy = sum(laundry/1000), 
            conditioning_energy = sum(conditioning/1000), 
            Global_Energy = sum(Global_active_power/60))

df9$hour <- hour(df9$DateTime)
df9$day <- day(df9$DateTime)

df9 <- df9 %>% group_by(day, hour) %>% 
  summarise(kitchen_energy = round(sum(kitchen_energy),0), 
            laundry_energy = round(sum(laundry_energy),0), 
            conditioning_energy = round(sum(conditioning_energy),0), 
            Global_Energy = round(sum(Global_Energy),0))

df9 <- df9 %>% group_by(hour) %>% 
  summarise(kitchen_energy = getmode(kitchen_energy), 
            laundry_energy = getmode(laundry_energy), 
            conditioning_energy = getmode(conditioning_energy), 
            Global_Energy = getmode(Global_Energy))

df9$Energy_no_submetered <- (df9$Global_Energy - df9$kitchen_energy - 
                               df9$laundry_energy - df9$conditioning_energy)

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




# Data splitting ----
trainSet <- window(df3ts, 2007, c(2009,12))
df3fit1 <- meanf(trainSet,h=12)
df3fit2 <- rwf(trainSet,h=12)
df3fit3 <- snaive(trainSet,h=12)

testSet <- window(df3ts, 2010)
accuracy(df3fit1, testSet)
accuracy(df3fit2, testSet)
accuracy(df3fit3, testSet)

gglagplot(df3ts) # The relationship is strongly positive at lag 12, reflecting the strong seasonality in the data.
ggAcf(df3ts, lag=24)
ggsubseriesplot(df3ts)
ggseasonplot(df3ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Power consumed (kW/h)") +
  ggtitle("Seasonal plot: Monthly power consume")


autoplot(window(df3ts, start=2007)) +
  autolayer(df3fit1, series="Mean", PI=FALSE) +
  autolayer(df3fit2, series="Naïve", PI=FALSE) +
  autolayer(df3fit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Energy (kW/h)") +
  ggtitle("Forecasts for monthly power consume") +
  guides(colour=guide_legend(title="Forecast"))

trainSet <- window(df2ts, 2007, c(2010,300))
df2fit1 <- meanf(trainSet,h=65)
df2fit2 <- rwf(trainSet,h=65)
df2fit3 <- snaive(trainSet,h=65)

testSet <- window(df2ts, c(2010,300))
accuracy(df2fit1, testSet)
accuracy(df2fit2, testSet)
accuracy(df2fit3, testSet)

autoplot(window(df2ts, start = c(2009,300))) +
  autolayer(df2fit1, series="Mean", PI=FALSE) +
  autolayer(df2fit2, series="Naïve", PI=FALSE) +
  autolayer(df2fit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Energy (kW/h)") +
  ggtitle("Forecasts for monthly power consume") +
  guides(colour=guide_legend(title="Forecast"))

# IDEA ----

df4 <- df1 %>% filter(year > 2006) %>% group_by(Date, Time, hour, month, year) %>% 
  summarise(x = sum(Global_active_power/60))
df4 <- df4 %>% group_by(Date, hour, month, year) %>% summarise(x = sum(x))
df4 <- df4 %>% group_by(year, month, hour) %>% summarise(GAP_mean = mean(x))
df4ts <- ts(df4$GAP_mean, frequency = 24*12, start = c(2007,1))
autoplot(df4ts)

df4ts_forecast<- forecast(df4ts, h=24)
plot(df4ts_forecast)

trainSet <- window(df4ts, 2007, c(2010,24*9))
df4fit1 <- meanf(trainSet,h=240)
df4fit2 <- rwf(trainSet,h=240)
df4fit3 <- snaive(trainSet,h=240)

testSet <- window(df4ts, c(2010,24*9))
accuracy(df4fit1, testSet)
accuracy(df4fit2, testSet)
accuracy(df4fit3, testSet)

# IDEA 2----

df5 <- df1 %>% filter(year > 2006) %>% group_by(Date, Time, hour, quarter, year) %>% 
  summarise(x = sum(Global_active_power/60))
df5 <- df5 %>% group_by(Date, hour, quarter, year) %>% summarise(x = sum(x))
df5 <- df5 %>% group_by(year, quarter, hour) %>% summarise(GAP_mean = mean(x))
df5ts <- ts(df5$GAP_mean, frequency = 24*4, start = c(2007,1))
autoplot(df5ts)

df5ts_forecast<- forecast(df5ts, h=24)
plot(df5ts_forecast)

trainSet <- window(df5ts, 2007, c(2009,48))
df5fit1 <- meanf(trainSet,h=48)
df5fit2 <- rwf(trainSet,h=48)
df5fit3 <- snaive(trainSet,h=48)

testSet <- window(df5ts, c(2009,48))
accuracy(df5fit1, testSet)
accuracy(df5fit2, testSet)
accuracy(df5fit3, testSet)

e <- tsCV(df3ts, rwf, drift=TRUE, h=1)
e2 <- tsCV(df3ts, snaive, drift=TRUE, h=1)

sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(e2^2, na.rm=TRUE))

sqrt(mean(residuals(rwf(df3ts, drift=TRUE))^2, na.rm=TRUE))
sqrt(mean(residuals(snaive(df3ts, drift=TRUE))^2, na.rm=TRUE))


