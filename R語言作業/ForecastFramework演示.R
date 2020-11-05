#下載軟件
library(devtools)
devtools::install_github('HopkinsIDD/ForecastFramework')
##只需安裝一次的軟件
install.packages(c('R6','devtools','forecast','ggplot2',
                   'gridExtra','data.table','knitr','kableExtra','RCurl'))
install.packages("dplyr", repos = "https://cloud.r-project.org") ##資料庫來源更新
#下載軟件
library(devtools)
devtools::install_github('HopkinsIDD/ForecastFramework')
devtools::install_github('reichlab/sarimaTD') 
devtools::install_github("hrbrmstr/cdcfluview")
#測試軟件包是否安裝正確
##預測框架依存關係
library(ForecastFramework)
library(R6)
library(forecast)

##資料依存關係
library(cdcfluview)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)
library(knitr)
library(kableExtra)

##Github模型的源函數
source_github <- function(u) {
  library(RCurl)
  script <- getURL(u, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}###讀取腳本並進行評估，產生數據讀取及可視覺化 
##R6源文件
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/ContestModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMAModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/GamModel.R')
source_github('https://raw.githubusercontent.com/reichlab/forecast-framework-demos/master/models/SARIMATD1Model.R')

#原始資料數據
dat <- ilinet(region = "National") ##互聯網的區域是""
dat <- dat %>%
  select('region','year','week','weighted_ili', 'week_start') ##選擇想呈現的資料(weighted_ili是該特定年和周報告的wILI度量)
print(head(dat,7))##展現幾筆資料

#原始數據的時間序列
dat$date_sick <- as.Date(strptime(dat$week_start,"%m/%d/%Y")) ## 轉換為日期
##制定圖表呈現資訊
plot <- ggplot() +
  geom_line(mapping = aes(x = week_start, y = weighted_ili),size=0.7,data = dat) +
  xlab("") + ylab("Weighted ILI") +
  coord_cartesian(ylim = c(0, 10)) +
  ggtitle("National Weighted Inluenza-Like Illness")
##呈現圖表
print(plot)
##創建新矩陣
data_matrix <- matrix(1:9,3,3) ###我有1-9的數字，要形成三*三的矩陣
print(data_matrix)
##以關聯矩陣之方法創造新的對象
data_object <- IncidenceMatrix$new(data_matrix)
##用矩陣方式看新數據對象
data_object$mat
##與數據列關聯的元數據列表
data_object$colData <- list(1:3) #初始化第一列
data_object$colData <- list(c("A","B","C"))
data_object$colData
##與數據行關聯的元數據列表
data_object$rowData <- list(1:3) #初始化第一行
data_object$rowData <- list(c("A","B","C"))
data_object$rowData
##增加列數
data_object$addColumns(2)
data_object$colData
##增加行數
data_object$addRows(3)
data_object$rowData



#通過模擬使用該forecast::auto.arima()函數擬合的模型的多個時間序列軌跡來創建預測
##定義要生成的模擬的數量nsim、定義數據或period參數的季節性週期性，即可將它們傳遞到sarimaTDModel()類生成器中以創建一個新sarimaTDmodel類
nsim <- 10 ### 模擬的次數
sarimaTDmodel <- SARIMATD1Model$new(period = 52, nsim = nsim)###週期52，模擬次數前面定義

#導入fluview數據，範圍是National
dat <- ilinet(region = "National")
#設定範圍，篩選需要的數據範圍
dat <- dat %>% 
  filter( week != 53 &
          year > 2009 & 
          year < 2018 & 
          !(year == 2017 & week > 18))
#選擇資料
dat <- dat %>%
  select('region','year','week','weighted_ili', 'week_start')

#創建預處理函數
preprocess_inc <- function(dat){
  dat$time.in.year = dat$week
  dat$t = dat$year + dat$week/52
  inc = ObservationList$new(dat)
  inc$formArray('region','t',val='weighted_ili',
                dimData = list(NULL,list('week','year','time.in.year','t')),
                metaData = list(t.step = 1/52,max.year.time = 52))
  return(inc)
}

#分成訓練和測試數據
training_data <- dat %>% 
  filter( ! ((year == 2016 ) | (year == 2017 ) ))
testing_data <- dat %>% 
  filter( ! ((year == 2016 & week >= 19) | (year == 2017 & week <= 18)) )
truth_data <- dat %>% 
  filter( ((year == 2016 & week >= 19) | (year == 2017 & week <= 18)) )

training_inc <- preprocess_inc(training_data)
testing_inc <- preprocess_inc(testing_data)
truth_inc <- preprocess_inc(truth_data)
print(training_inc$mat[,0:10])

#擬合模型
sarimaTDmodel$fit(training_inc)

#定義要預測之週期數
steps <- 52
#使用內置$forecast()功能創建預測
forecast_X <- sarimaTDmodel$forecast(testing_inc,steps = steps)

#查看數值(中位數)
forecast_X$median()$mat

#將預測矩陣轉換為R中的數據框以便於操作
preds_df <- data.frame(as.table(t(forecast_X$data$mat)))

#將預測轉換為數據框以使用dplyr，導入包含2013年數據的測試數據集，將預測日期添加到原始預測
preds_df[["date_sick"]] <-as.Date(truth_data$week_start, format = "%d-%m-%Y")


