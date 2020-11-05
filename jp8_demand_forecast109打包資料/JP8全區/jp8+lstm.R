
setwd("C:/Users/user/Desktop/數據科學/jp8_demand_forecast109打包資料/JP8全區")
library(dwapi) 
library(keras)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tensorflow)
library(readxl)
library(xts)
library(forecast)

#讀取資料
df <- read_excel("JP8.xlsx", sheet=1 ,na="NA")
str(df)                      

# 一周的天 '%A'當前語言環境中的完整工作日名稱。（還匹配輸入的縮寫名稱。）
df$day <- as.factor(strftime(df$date, format = '%A'))
str(df)

# 一年中的天
df$yearday <- as.factor(strftime(df$date, format = '%m%d'))
str(df)# 研究的最終結構

#最低耗油量及最高耗油料數據，中位數及平均數 Mean中位數 Median，四分位及標準差
summary(df)
# 數據準備以用於模型評估
# 透過時間記號將最後 20%  '10-01-2017'用做測試集
df_test <- subset(df, date >= strptime('01-10-2017', format = '%d-%m-%Y'))
df <- subset(df, date < strptime('01-10-2017', format = '%d-%m-%Y'))
ts <- ts(df$demand, frequency = 1)


ggplot(df, aes(x=date, y=demand)) +        #點點圖的顏色
  geom_point(color='blue') +                
  theme_grey(base_size = 16)

hist(x=df$demand,col="blue", xlab="需求", ylab="", main="需求")
# 可觀察出最常出現的價位
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(df$demand)
#盒鬚圖
df$day <- as.factor(strftime(df$date, format = '%A'))
ggplot(df, aes(x=day, y=demand)) +  geom_boxplot(color = "red") +
  xlab('Day') +ylab('Demand (GL)')+ 
  ggtitle('Demand per day of the week')

# 標準差
sd(df$demand)

# 變異數
var(df$demand)

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

#資料前處理
Series = df$demand  # 目標

#創建數據值, e.g. t-1, t

lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
supervised = lag_transform(Series,1)


# 分割數據集並得到斜率
diffed = diff(Series, differences = 1)

supervised = lag_transform(diffed, 1)

N = nrow(supervised)
n = round(N *0.7, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]

## 將資料正常化
normalize <- function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}

## 正常化的反矩陣
inverter = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  n = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(n)
  
  for( i in 1:n){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

# 正規化

Scaled = normalize(train, test, c(-1, 1))


y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]

y_test = Scaled$scaled_test[, 2]
x_test = Scaled$scaled_test[, 1]

#取得訓練集的資料長度與維度
dim(x_train) <- c(length(x_train), 1, 1) 
X_shape2 = dim(x_train)[2]               
X_shape3 = dim(x_train)[3]            
batch_size = 1
units = 10                 # 神經元數目

# 建模
model <- keras_model_sequential() 
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3),return_sequences = TRUE, stateful= TRUE)%>%
  layer_lstm(units, stateful= TRUE,return_sequences = FALSE)%>%
  layer_dense(units = 1)



model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02 , decay = 1e-6 ),  
  metrics = c('accuracy')
)

summary(model)

Epochs = 50
nb_epoch = Epochs   
for(i in 1:nb_epoch ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}

#開始預測
L = length(x_test)
scaler = Scaled$scaler
predictions = numeric(L)

for(i in 1:L){
  X = x_test[i]
  dim(X) = c(1,1,1)
  yhat = model %>% predict(X, batch_size=batch_size)
  # 逆縮放資料
  yhat = inverter(yhat, scaler,  c(-1, 1))
  # 逆縮放不同處
  yhat  = yhat + Series[(n+i)]
  # 储存
  predictions[i] <- yhat
}

 #視覺化
list1 <- rep(NA,4602)
list2 <- c(list1,predictions)
data_plot1 <- as.data.frame(Series)
data_plot2 <- as.data.frame(list2)
ggplot(data_plot1,aes(x=seq_along(Series), y=Series)) + 
  geom_line(color='#56B4E9') + 
  geom_line(data = data_plot2,aes(x=seq_along(list2), y=list2),color='red') +
  theme_grey(base_size = 16) +
  ggtitle("Prediction") +
  labs(x = "time index", y = "price")

# 平均絕對百分比誤差損失
library(MLmetrics)
MAPE(y_pred = round(predictions), y_true = Series[4602:6573])

