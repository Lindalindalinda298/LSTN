# JP8 Demand Forecasting Model
#設定檔案路徑
setwd("D:\\R語言\\workspace\\jp8_demand_forecast109打包資料\\JP8全區")

#下載軟體(載過就不用)
installed.packages("xts")

# Load required packages
library(xts)
library(ggplot2)
library(forecast)
library(readxl)

# 數據準備，探索性數據分析
# 輸入數據
df <- read_excel("JP8.xlsx", sheet=1 ,na="NA")
View(df)
#研究的最終結構
str(df)

# 將日期字符串轉換為POSIX日期
#df$date <- strptime(df$date, format = '%d-%m-%y')
#str(df)

# 一周的天 '%A'當前語言環境中的完整工作日名稱。（還匹配輸入的縮寫名稱。）
df$day <- as.factor(strftime(df$date, format = '%A'))
str(df)

# 一年中的天
df$yearday <- as.factor(strftime(df$date, format = '%m%d'))

# 研究的最終結構
str(df)
#最低耗油量及最高耗油料數據，中位數及平均數 Mean中位數 Median，四分位及標準差
summary(df)
# 數據準備以用於模型評估
# 透過時間記號將最後 20%  '10-01-2017'用做測試集
df_test <- subset(df, date >= strptime('01-10-2017', format = '%d-%m-%Y'))
df <- subset(df, date < strptime('01-10-2017', format = '%d-%m-%Y'))
ts <- ts(df$demand, frequency = 1)

# 時間序列可視化整個時間段內的需求變化
# Dataframe and tilot helps vme series objects
demandts <- xts(df$demand, df$date)
#設定圖表(標題 X軸 Y軸)
plot(demandts, main = 'JP8 demand evolution', xlab = 'Date', ylab = 'Demand (GL)')
#長條圖(滿足信效度-常態分配)
hist(demandts, col="yellow", xlab="", main="")
# # 一星期中，當天的需求量(箱子圖呈現)
ggplot(df, aes(day, demand)) + geom_boxplot() + xlab('Day') + ylab('Demand (GL)') +labs(title = "Demand per day of the week")+theme(plot.title = element_text(hjust = 0.5, size = 14, color = "red"))   # Center title position and size

# 按一年中的天（平均）匯總需求
avg_demand_per_yearday <- aggregate(demand ~ yearday, df, 'mean')

#計算時間序列的平滑曲線。為了實現連續性，在計算曲線之前先複製數據
smooth_yearday <- rbind(avg_demand_per_yearday, avg_demand_per_yearday, avg_demand_per_yearday, avg_demand_per_yearday, avg_demand_per_yearday)
smooth_yearday <- lowess(smooth_yearday$demand, f = 1 / 45)
l <- length(avg_demand_per_yearday$demand)
l0 <- 2 * l + 1
l1 <- 3 * l
smooth_yearday <- smooth_yearday$y[l0:l1]


#  自我回歸&非自我回歸
par(mfrow = c(1, 2))
acf(avg_demand_per_yearday, 100, main = 'Autocorrelation')
pacf(avg_demand_per_yearday, 100, main = 'Partial autocorrelation')

# 繪製結果
par(mfrow = c(1, 1))
# 將年份設置為2016年，以允許2月29日存在
dates <- as.Date(paste(levels(df$yearday), '2016'), format = '%m%d%Y')
plot(dates, avg_demand_per_yearday$demand, type = 'l', main = 'Average daily demand', xlab = 'Date', ylab = 'Demand (GL)')
lines(dates, smooth_yearday, col = 'blue', lwd = 2)

#下圖顯示了錯誤。 注意最大的錯誤都是負的
par(mfrow = c(1, 2))
diff <- avg_demand_per_yearday$demand - smooth_yearday
abs_diff <- abs(diff)
barplot(diff[order(-abs_diff)], main = 'Smoothing error', ylab = 'Error')
boxplot(diff, main = 'Smoothing error', ylab = 'Error')

head(strftime(dates[order(-abs_diff)], format = '%B %d'), 10)

#p值遠小於0.05，因此我們拒絕了虛無假設，非定態，帶有趨勢
Box.test(df$demand)

# 更深入的分析
# 分解每周的季節性時間序列
wts <- ts(ts, frequency = 7)
dec_wts <- decompose(wts)
plot(dec_wts)

# 需求减去周季節性
df$demand_mws <- df$demand - as.numeric(dec_wts$season)

#分解年度季節性時間序列
yts <- ts(subset(df, yearday != '0229')$demand_mws, frequency = 365)
dec_yts <- decompose(yts)
plot(dec_yts)

days365 <- which(df$yearday != '0229')
february29ths <- which(df$yearday == '0229')
df$demand_mwys[days365] <- df$demand_mws[days365] - as.numeric(dec_yts$season)

# 2月29日填充值
df$demand_mwys[february29ths] <- df$demand_mws[february29ths]

#根據原始觀測值減去每周和每年的季節性數據形成一個新的時間序列
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
acf(demandts, 10, main = 'Autocorrelation')
pacf(demandts, 10, main = 'Partial autocorrelation')

# SARIMA model
(fit <- forecast::auto.arima(df$demand_mws, seasonal=TRUE))
# SARIMA model 試誤法選取
model <- Arima(ts, order = c(8, 1, 7), list(order = c(1, 1, 1), period = 7))

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

#forecast
plot(forecast(Arima(tail(ts, 200), model = model)))
#準確度測試
round(forecast::accuracy(model),3)

#殘差序列執行檢驗，p-value應該顯著大於0.05
r1<-model$residuals
Box.test(r1,lag = 1,type = "Ljung-Box")
model
