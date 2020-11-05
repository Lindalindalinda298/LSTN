# JP8 Demand Forecasting Model

setwd("D:/workspace/JP8_Demand_Forecast1081102")
# You can change to your working directory path

# Load required packages
library(xts)
library(ggplot2)
library(forecast)
library(readxl)

# Data Preparation & Exploratory data analysis
# Loading data into memory
df <- read_excel("JP8H2.xlsx", sheet=1 ,na="NA")
View(df)
str(df)

# Convert date strings to POSIX dates
#df$date <- strptime(df$date, format = '%d-%m-%y')
#str(df)

# Day of the week
df$day <- as.factor(strftime(df$date, format = '%A'))
str(df)

# Day of the year
df$yearday <- as.factor(strftime(df$date, format = '%m%d'))

# Final structure for the study
str(df)
summary(df)
# Data preparation for model evaluation later
# The last 20% of dataset is extracted into "test" dataset by time stamp '10-01-2017'
df_test <- subset(df, date >= strptime('01-10-2017', format = '%d-%m-%Y'))
df <- subset(df, date < strptime('01-10-2017', format = '%d-%m-%Y'))
ts <- ts(df$demand, frequency = 1)

# A time series plot helps visualizing the demand evolution across the time period
# Dataframe and time series objects
demandts <- xts(df$demand, df$date)
plot(demandts, main = 'JP8 demand evolution', xlab = 'Date', ylab = 'Demand (GL)')

hist(demandts, col="yellow", xlab="", main="")
# Demand by day of the week
ggplot(df, aes(day, demand)) + geom_boxplot() + xlab('Day') + ylab('Demand (GL)') + ggtitle('Demand per day of the week')

# Aggregating demand by day of the year (average)
avg_demand_per_yearday <- aggregate(demand ~ yearday, df, 'mean')

# Computing the smooth curve for the time series. Data is replicated before computing the curve in order to achieve continuity
smooth_yearday <- rbind(avg_demand_per_yearday, avg_demand_per_yearday, avg_demand_per_yearday, avg_demand_per_yearday, avg_demand_per_yearday)
smooth_yearday <- lowess(smooth_yearday$demand, f = 1 / 45)
l <- length(avg_demand_per_yearday$demand)
l0 <- 2 * l + 1
l1 <- 3 * l
smooth_yearday <- smooth_yearday$y[l0:l1]

# Plotting the result
par(mfrow = c(1, 1))
# Setting year to 2016 to allow existence of 29th February
dates <- as.Date(paste(levels(df$yearday), '2016'), format = '%m%d%Y')
plot(dates, avg_demand_per_yearday$demand, type = 'l', main = 'Average daily demand', xlab = 'Date', ylab = 'Demand (GL)')
lines(dates, smooth_yearday, col = 'blue', lwd = 2)

# The graphics bellow show the errors. Notice how the biggest errors are all negative
par(mfrow = c(1, 2))
diff <- avg_demand_per_yearday$demand - smooth_yearday
abs_diff <- abs(diff)
barplot(diff[order(-abs_diff)], main = 'Smoothing error', ylab = 'Error')
boxplot(diff, main = 'Smoothing error', ylab = 'Error')

head(strftime(dates[order(-abs_diff)], format = '%B %d'), 10)

par(mfrow = c(1, 2))
acf(df$demand, 100, main = 'Autocorrelation')
acf(df$demand, 1500, main = 'Autocorrelation')
pacf(df$demand, 100, main = 'Partial autocorrelation')
pacf(df$demand, 1500, main = 'Partial autocorrelation')

par(mfrow = c(1, 1))
acf(df$demand, 30, main = 'Autocorrelation')
pacf(df$demand, 30, main = 'Partial autocorrelation')

#p值遠小於0.05，因此我們拒絕了虛無假設，非定態，帶有趨勢
Box.test(df$demand)

# Further analysis
# Decomposition of the weekly seasonal time series
wts <- ts(ts, frequency = 7)
dec_wts <- decompose(wts)
plot(dec_wts)

# Demand minus week seasonal
df$demand_mws <- df$demand - as.numeric(dec_wts$season)

# Decomposition of the yearly seasonal time series
yts <- ts(subset(df, yearday != '0229')$demand_mws, frequency = 365)
dec_yts <- decompose(yts)
plot(dec_yts)

days365 <- which(df$yearday != '0229')
february29ths <- which(df$yearday == '0229')
df$demand_mwys[days365] <- df$demand_mws[days365] - as.numeric(dec_yts$season)

# Fill values on February 29th
df$demand_mwys[february29ths] <- df$demand_mws[february29ths]

# A new time series is formed out of the original observation minus the weekly and the yearly seasonal data
par(mfrow = c(1, 1))
ts_mwys <- ts(df$demand_mwys, frequency = 1)
demandts_mwys <- xts(df$demand_mwys, df$date)
plot(demandts_mwys, main = 'JP8 demand minus seasonal data', xlab = 'Date', ylab = 'Demand (GL)')

ded1 = diff(demandts_mwys, differences = 1)
par(mfrow = c(2, 1))
plot(demandts, main = 'JP8 demand evolution', xlab = 'Date', ylab = 'Demand (GL)')
plot(ded1, main = 'diff JP8 demand evolution', xlab = 'Date', ylab = 'Demand (GL)')

#拒絕零假設，並將其分類為定態
tseries::adf.test(df$demand)

# Plotting the average daily demand of the demand minus the seasonal data shows 
# a new error rate much lower than the one seen before
# Aggregating demand by day of the year (average)
avg_demand_mwys_per_yearday <- aggregate(demand_mwys ~ yearday, df, 'mean')

# Computing the smooth curve for the time series. Data is replicated before computing the curve in order to achieve continuity
smooth_yearday <- rbind(avg_demand_mwys_per_yearday, avg_demand_mwys_per_yearday, avg_demand_mwys_per_yearday, avg_demand_mwys_per_yearday, avg_demand_mwys_per_yearday)
smooth_yearday <- lowess(smooth_yearday$demand_mwys, f = 1 / 45)
l <- length(avg_demand_mwys_per_yearday$demand_mwys)
l0 <- 2 * l + 1
l1 <- 3 * l
smooth_yearday <- smooth_yearday$y[l0:l1]

# Plotting the result
par(mfrow = c(1, 1))
# Setting year to 2016 to allow existence of 29th February
dates <- as.Date(paste(levels(df$yearday), '2016'), format = '%m%d%Y')
plot(dates, avg_demand_mwys_per_yearday$demand_mwys, type = 'l', main = 'Average daily demand', xlab = 'Date', ylab = 'Demand (GWh)')
lines(dates, smooth_yearday, col = 'blue', lwd = 2)


par(mfrow = c(1, 2))
diff <- avg_demand_mwys_per_yearday$demand_mwys - smooth_yearday
abs_diff <- abs(diff)
barplot(diff[order(-abs_diff)], main = 'Smoothing error', ylab = 'Error')
boxplot(diff, main = 'Smoothing error', ylab = 'Error')

# The new ACF and PACF
par(mfrow = c(1, 2))
acf(df$demand_mwys, 100, main = 'Autocorrelation')
pacf(df$demand_mwys, 100, main = 'Partial autocorrelation')

# SARIMA model
(fit <- forecast::auto.arima(df$demand, seasonal=TRUE))
(fit <- forecast::auto.arima(df$demand_mws, seasonal=TRUE))
(fit <- forecast::auto.arima(df$demand_mwys, seasonal=TRUE))

model <- Arima(ts, order = c(5, 0, 2), list(order = c(1, 1, 1), period = 7))
model <- Arima(ts, order = c(5, 3, 2))
# Forecasting error can be calculated iterating through the test data frame
auxts <- ts
auxmodel <- model
errs <- c()
pred <- c()
perc <- c()
for (i in 1:nrow(df_test)) {
  p <- as.numeric(predict(auxmodel, newdata = auxts, n.ahead = 1)$pred)
  pred <- c(pred, p)
  errs <- c(errs, p - df_test$demand[i])
  perc <- c(perc, (p - df_test$demand[i]) / df_test$demand[i])
  auxts <- ts(c(auxts, df_test$demand[i]), frequency = 7)
  auxmodel <- Arima(auxts, model = auxmodel)
}
par(mfrow = c(1, 1))
plot(errs, type = 'l', main = 'Error in the forecast')

plot(pred, type = 'l', main = 'Real vs. forecast', col = 'red')
lines(df_test$demand)
legend('topright', c('Real', 'Forecast'), lty = 1, col = c('black', 'red'))

abserr <- mean(abs(errs))
percerr <- mean(abs(perc)) * 100

# Forecast
plot(forecast(Arima(tail(ts, 200), model = model)))

round(forecast::accuracy(model),3)

r1<-model$residuals
Box.test(r1,lag = 1,type = "Ljung-Box")
