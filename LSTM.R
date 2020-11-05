setwd("D:/workspace/LSTM")
library(dwapi) 
library(keras)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tensorflow)

data <- read.csv('D:\\研究\\研究生\\畢業生研究成果\\109年-運籌20期\\羅婉甄-運用長短期記憶網路演算法建構公部門船用油料\\給郭老師\\給郭老師\\論文\\程式\\LSTM/0324.csv')          # read Data
summary(data)                         # summary

ggplot(data, aes(x=date, y=demand)) +        # scatter polt
  geom_point(color='blue') +                #點點圖顏色
  theme_grey(base_size = 16)

# mode function 可觀察出最常出現的價位
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(data$demand)

# standard deviation 標準差
sd(data$demand)

# variance 變異數
var(data$demand)

#資料前處理
Series = data$demand  # target

#create shift dataset, e.g. t-1, t

lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
supervised = lag_transform(Series,1)


# split transfer dataset and get Slope
diffed = diff(Series, differences = 1)

supervised = lag_transform(diffed, 1)

N = nrow(supervised)
n = round(N *0.7, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]

## normalize function
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

## inverse-normalize
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

# normalization

Scaled = normalize(train, test, c(-1, 1))


y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]

y_test = Scaled$scaled_test[, 2]
x_test = Scaled$scaled_test[, 1]

#取得training的資料長度與維度
dim(x_train) <- c(length(x_train), 1, 1) 
X_shape2 = dim(x_train)[2]               
X_shape3 = dim(x_train)[3]            
batch_size = 1
units = 10                 # 神經元數目

# 建立模型
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
  # invert scaling
  yhat = inverter(yhat, scaler,  c(-1, 1))
  # invert differencing
  yhat  = yhat + Series[(n+i)]
  # store
  predictions[i] <- yhat
}

 #資料視覺化
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

# MAPE function in packages MLmetrics
library(MLmetrics)
MAPE(y_pred = round(predictions), y_true = Series[4602:6573])

