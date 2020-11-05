#設定工作資料夾
getwd()  #顯示目前工作資料夾
setwd("C:/Users/linda/Desktop") #設定工作資料夾
dir("C:/Users/linda/Desktop")#讀取工作資料夾
#載入資料  
data<-read.csv('C:/Users/linda/Desktop/資料/AAPL.csv',sep=',')
library(ggplot2)
library(keras)
library(dplyr)
library(ggthemes)
library(lubridate)
library(tensorflow)
#看基本資料
summary(data)
View(data)
#畫基本圖形
ggplot(data, aes(x=Date, y=Close)) + #x，y軸名稱
  geom_point(color='#56B4E9') +    #點的顏色
  theme_grey(base_size = 16)  #大小
#眾數函式
getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
#查看基本基本資料
getmode(data$Close)
sd(data$Close)
var(data$Close)
#資料預先處理
Series=data$Close
#轉換
lag_transform<-function(x, k=1){
  lagged=c(rep(NA, k), x[1:(length(x)-k)])
  DF=as.data.frame(cbind(lagged, x))
  colnames(DF)<-c(paste0('x-',k),'x')
  DF[is.na(DF)]<-0
  return(DF)
}
supervised = lag_transform(Series, 1)
#差分運算,擷取資料
diffed=diff(Series, differences = 1)
supervised=lag_transform(diffed,1)

N=nrow(supervised)
n=round(N*0.8, digits = 0)
train=supervised[1:n, ]#訓練級資料
test=supervised[(n+1):N, ]#測試及資料
#創建正規化
normalize <- function(train, test, feature_range = c(0, 1)) {
x= train
fr_min= feature_range[1]
fr_max= feature_range[2]
std_train= ((x - min(x) ) / (max(x) - min(x)  ))
std_test= ((test - min(x) ) / (max(x) - min(x)  ))
scaled_train= std_train *(fr_max -fr_min) + fr_min
scaled_test= std_test *(fr_max -fr_min) + fr_min
return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}
inverter= function(scaled, scaler,feature_range = c(0, 1)){
  min= scaler[1]
  max= scaler[2]
  n= length(scaled)
  mins= feature_range[1]
  maxs= feature_range[2]
  inverted_dfs= numeric(n)
  
  for( i in 1:n){
    X= (scaled[i]-mins)/(maxs-mins)
    rawValues=X *(max-min)+min
    inverted_dfs[i]<-rawValues
    
  }
  return(inverted_dfs)
  
}
#正規化
Scaled= normalize(train, test, c(-1,1))

y_train= Scaled$scaled_train[, 2]
x_train= Scaled$scaled_train[, 1]

y_test= Scaled$scaled_test[, 2]
x_test= Scaled$scaled_test[, 1]

#設定模型參數
##取得訓練的資料長度與維度
dim(x_train) <- c(length(x_train),1,1)
X_shape2= dim(x_train)[2]
X_shape3= dim(x_train)[3]
batch_size= 1
units= 10   ##神經元數目

#建立模型，準備框架
model <- keras_model_sequential() 
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3),return_sequences = TRUE, stateful= TRUE)%>%
  layer_lstm(units, stateful= TRUE,return_sequences = FALSE)%>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02 , decay = 1e-6 ),  #優化器選擇，學習率
  metrics = c('accuracy')   #正確率選擇
)

#確認模型結構
summary(model)

#模型訓練
Epochs = 50  #疊貸，每筆資料跑幾次
nb_epoch = Epochs   
for(i in 1:nb_epoch ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}

##取得預測的資料長度，逆正規化，將數值恢復到原本的單位，最後利用迴圈的設計，將值順序性的存取
L = length(x_test)
scaler = Scaled$scaler
predictions = numeric(L)

for(i in 1:L){
X = x_test[i]
dim(X) = c(1,1,1)
yhat = model %>% predict(X, batch_size=batch_size)
##逆正規化
yhat = inverter(yhat, scaler,  c(-1, 1))
##求差值(正確-預測)
yhat  = yhat + Series[(n+i)]
##儲存
predictions[i] <- yhat
}

#呈現圖表
list1 <- rep(NA,201)
list2 <- c(list1,predictions)
data_plot1 <- as.data.frame(Series)
data_plot2 <- as.data.frame(list2)
ggplot(data_plot1,aes(x=seq_along(Series), y=Series)) + 
  geom_line(color='#56B4E9') +
  geom_line(data = data_plot2,aes(x=seq_along(list2), y=list2),color='red') +
  theme_grey(base_size = 16) +
  ggtitle("Prediction") +
  labs(x = "time index", y = "price")

