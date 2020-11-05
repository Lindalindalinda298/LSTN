install.packages("xlsx")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-14.0.1')
Sys.getenv("JAVA_HOME")
library("xlsx")
sourceURL <- "https://raw.githubusercontent.com/jzuniga123"
file <- "/SPS/master/DATA%20624/ResidentialCustomerForecastLoad-624.xlsx"
download.file(paste0(sourceURL, file), "temp.xlsx", mode="wb")
energy <- xlsx::read.xlsx("temp.xlsx", sheetIndex=1, header=T)

# the “YYYY-MMM” format dates are interpreted as factors. They must be converted to dates
sessionInfo()
Sys.setlocale("LC_TIME","English")
energy$YYYY.MMM<-as.Date(paste0(energy$YYYY.MMM,"-01"),format = "%Y-%b-%d")
Sys.setlocale("LC_TIME","Chinese(Traditional)_Taiwan")
head(energy)

# preview the class of the dataset
class(energy)
#結構
str(energy)

# preview descriptive statistics on quantitative and qualitative variables
summary(energy)

# 預覽資料集區間
xts::periodicity(unique(energy$YYYY.MMM))

# preview observations in the dataframe that have no missing values
energy[!complete.cases(energy), ]

# plots each observed value against the time of the observation, with a single line connecting each observation across the entire period
kWh <- xts::xts(energy$KWH, order.by=energy$YYYY.MMM)
par(mfrow=c(2, 1), mar = c(3, 5, 0, 0), oma = c(0, 0, 0.5, 0.5))
plot(kWh, main="kWh")

# display frequency at which values in a vector occur
hist(kWh, col="yellow", xlab="", main="")

par(mfrow=c(2, 1), mar = c(3, 5, 0, 0), oma = c(0, 0, 0.5, 0.5))
# ACF autocorrelations between each observation and its immediate predecessor (lagged observation)
acf(na.omit(kWh), ylab="kWh", main="") 

# PACF autocorrelations between the current observation and each individual lagged observation
pacf(na.omit(kWh), ylab="kWh", main="")

# data cleaning w/ forecast::tsclean() and converted to a time series object using the ts(). 
# tsclean() function imputes nulls and removes outliers.
# ts() function converts data to a time series object which is compatible with the forecast package.
kWh <- ts(forecast::tsclean(energy$KWH, replace.missing=T), 
          frequency = 12, start=start(energy$YYYY.MMM)) # data sampled monthly = 12
kWh[kWh==min(kWh)] <- mean(kWh[kWh!=min(kWh)])

plot(kWh, col=8, xaxt = "n", ylab="ATM1")
lines(forecast::ma(kWh, order=6), col=6)  # pink line biannual period
lines(forecast::ma(kWh, order=12), col=4) # blue line annual period

plot(decompose(kWh), col=5)

tseries::adf.test(kWh)
index_train <- 1:(length(kWh) - 12)
kWh_train <- ts(kWh[index_train], frequency=12)
kWh_test <- ts(kWh[index_train], frequency=12)
(lambda <- forecast::BoxCox.lambda(kWh_train))

(fit <- forecast::auto.arima(kWh_train, stepwise=F, approximation=F, d=0, lambda=lambda))

par(mfrow=c(2, 1), mar = c(3, 5, 0, 0), oma = c(0, 0, 0.5, 0.5))
acf(residuals(fit), ylab="ACF kWh"); pacf(residuals(fit), ylab="PACF kWh")

Box.test(residuals(fit), lag=7, fitdf=sum(fit$arma[1:2]), type="Ljung-Box")

fcast <- forecast::forecast(fit, h=15)
plot(fcast, ylab="kWh", main="kWh Predictions", xaxt="n")
lines(lag(kWh_test, -length(kWh_train)), col=6)

round(forecast::accuracy(fcast, length(kWh_test)), 3)
