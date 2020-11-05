# R 基本資料處理練習- Data Manipulation(資料框建立、資料類型轉換及資料集合併、欄位變更等)
# 範例內容參考來源：One Piece Wiki

# 載入所需的套件 - tidyr
library(tidyr)

### 建立資料集
# 以c()函式建立草帽海賊團成員的各類基本資料的初始"向量變數物件"
name <- c("蒙其·D·魯夫", "羅羅亞·索隆", "娜美", "騙人布", "賓什莫克·香吉士",
          "多尼多尼·喬巴", "妮可·羅賓", "佛朗基", "布魯克")
#姓別
gender <- c("Male", "Male", "Female", "Male", "Male", "Male", "Female",
            "Male", "Male")
#角色
occupation <- c("船長", "劍士", "航海士", "狙擊手", "廚師", "醫生", "考古學家",
                "船匠", "音樂家")
#懸賞金額
bounty <- c(500000000, 320000000, 66000000, 200000000, 177000000, 100,
            130000000, 94000000, 83000000)
#生日
birthday <- c("05-05", "11-11", "07-03", "04-01", "03-02", "12-24", 
              "02-06", "03-09", "04-03")
#身高
height <- c(174, 181, 170, 176, 180, 90, 188, 240, 277)

# 以上列初始向量變項建立草帽海賊團角色設定的資料集資料框(dataframe)物件-straw_hat_df
straw_hat_df <- data.frame(name, gender, occupation, bounty, birthday, height)
View(straw_hat_df) #檢視資料集內容

# 檢視資料集內容 - 使用 dim() 、 head() 、 tail() 、 str() 與 summary()函數
dim(straw_hat_df)
head(straw_hat_df)
tail(straw_hat_df)
str(straw_hat_df)
summary(straw_hat_df)

# 資料排序操作 - 使用order()函數
# 基於 height 進行遞增排序
straw_hat_df[order(straw_hat_df$height, decreasing = FALSE), ]

# 基於 bounty 進行遞減排序
straw_hat_df[order(straw_hat_df$bounty, decreasing = TRUE), ]

## 資料集基本操作
# 建立並新增欄位(column) - 利用 cbind() 函數將age變項加入資料集中成為新的欄位
age <- c(19, 21, 20, 19, 21, 17, 30, 36, 90)
straw_hat_df <- cbind(straw_hat_df, age)
View(straw_hat_df) #檢視資料集內容

# 草帽海賊團的廚師-賓什莫克·香吉士要為船員們準備餐點
# 建立並新增"最喜愛料理"的變項向量
favorite_food <- c("Meat", "Food matches wine", "Orange", "Fish",
                   "Food matches black tea", "Sweets", "Food matches coffee",
                   "Food matches coke", "Milk")

# 將向量加入資料框中成為新的欄位
straw_hat_df$favorite_food <- favorite_food
# 將資料框輸出在 R Console
straw_hat_df


# 刪除"favorite_food"的欄位
straw_hat_df$favorite_food <- NULL

# 將資料框輸出在 R Console
straw_hat_df


# 新增一筆資料(列數)-薇薇公主
princess_vivi <- c("娜菲魯塔利·薇薇", "Female", "阿拉巴斯坦王國公主", NA, "02-02", NA, 18)

# 將薇薇公主加入草帽海賊團資料框
straw_hat_df <- rbind(straw_hat_df, princess_vivi)

# 把 straw_hat_df 輸出在 R Console
straw_hat_df

# 刪除薇薇公主的資料列
straw_hat_df <- straw_hat_df[-10,]

straw_hat_df

# 用資料篩選函式 subset 函式一次刪除職業與身高兩個欄位
straw_hat_df_test <- subset(straw_hat_df, select = c(-occupation, -height))
View(straw_hat_df_test)

# 另一種寫法
# straw_hat_df_test <- subset(straw_hat_df, select = c(-3, -7)) 
# View(straw_hat_df_test)

## 變更欄位名稱
# 將賞金欄位 straw_hat_df_test$bounty 改命名為 straw_hat_df$reward
names(straw_hat_df_test)[3] <- "reward"
# 將 straw_hat_df_test 的欄位名稱輸出在 R Console
names(straw_hat_df_test)

## 資料篩選
# 篩選賞金大於 1000 萬貝里並且年齡小於 30 歲，欄位只需要包含姓名、賞金與年齡
subset(straw_hat_df, bounty > 1000 & age < 30, select = c(name, bounty, age))

## 資料型別檢視與變更
# 檢視個別欄位資料型別
class(straw_hat_df$name)
class(straw_hat_df$occupation)
class(straw_hat_df$birthday)

# 將資料框中的"name", "occupation"欄位資料型別變更為character
straw_hat_df$name <- as.character(straw_hat_df$name)
straw_hat_df$occupation <- as.character(straw_hat_df$occupation)
straw_hat_df$birthday <- as.character(straw_hat_df$birthday)

# 檢視資料型別是否已變更
class(straw_hat_df$name)
class(straw_hat_df$occupation)
class(straw_hat_df$birthday)


### 資料的轉換: 類別型變數的重新分類
## 類別型變數的重新分類(1) 分成2類別:
#雖然草帽海賊團每個船員都有獨立作戰的能力，但交戰仍然區分為兩種類型：輔助型與戰鬥型。
#加一個欄位紀錄船員們的戰鬥類型 
straw_hat_df$battle_role <- ifelse(straw_hat_df$occupation %in% c("航海士", "狙擊手", 
                            "醫生", "考古學家"), yes = "Support", no = "Fighter")
straw_hat_df

straw_hat_df$battle_role <- NULL # 先移除衍生欄位

## 類別型變數的重新分類(2) 分成3類別:  Range, Support, Fighter
straw_hat_df$battle_role[straw_hat_df$occupation == c("狙擊手")] <- "Range"
straw_hat_df$battle_role[straw_hat_df$occupation %in% c("醫生", "考古學家", "航海士")] <- "Support"
straw_hat_df$battle_role[straw_hat_df$occupation %in% c("船長", "劍士", "廚師", "船匠", "音樂家")] <- "Fighter"

# 將資料框輸出在 R Console
straw_hat_df

#### 資料的轉換: 數值型變數的分類 (又稱資料分箱)
#將船員依照賞金級距切分為低、中與高三個等級，以既有的數值型變數所衍生(分箱)轉換
straw_hat_df$bounty <- as.numeric(straw_hat_df$bounty)
straw_hat_df$bounty_level <- cut(straw_hat_df$bounty, breaks = 
                        c(0, 8.30e+07, 1.8e+08, Inf), labels = c("Low", "Medium", "High"))

# 將資料框輸出在 R Console
straw_hat_df

### 資料的轉換: 離散資料轉換成類別資料-使用資料分箱
#以subset()函數對age 25歲做為判斷條件進行資料分箱
# 新增一筆資料(列數)-薇薇公主
princess_vivi <- c("娜菲魯塔利·薇薇", "Female", "阿拉巴斯坦王國公主", 
                   NA, "02-02", NA, 18, NA, NA)

# 將薇薇公主加入草帽海賊團資料框,並存入新資料框straw_hat_df_w_vivi_age
straw_hat_df_w_vivi_age<- rbind(straw_hat_df, princess_vivi)
straw_hat_df_w_vivi_age$age <- as.double(straw_hat_df_w_vivi_age$age)
# 資料分箱操作
bins <-c(0, 25, Inf)
group_names <- c("小於25歲", "25歲以上")
straw_hat_df_w_vivi_age$age_cat <- cut(straw_hat_df_w_vivi_age$age, breaks = bins, labels = group_names)
View(straw_hat_df_w_vivi_age)



## 衍生數值型變項欄位
# 新增一個以百萬元貝里作為單位的 straw_hat_df$bounty_million
straw_hat_df$bounty_million <- straw_hat_df$bounty/1000000

# 將資料框輸出在 R Console
straw_hat_df

## 較難的衍生變數: 船員的生日資料及日期格式的轉換
# 1.取得系統時間資料
# 產生 sys_date
sys_date <- Sys.Date()
sys_date

# 產生 sys_date_year
sys_date_year <- format(sys_date, '%Y')
sys_date_year
class(sys_date_year)

# 產生 sys_date_year_num
sys_date_year_num <- as.numeric(sys_date_year)

# 將 sys_date, sys_date_year 與 sys_date_year_num 輸出在 R Console
sys_date

sys_date_year

sys_date_year_num


## 較難的衍生變數: 船員的生日資料及日期格式的轉換
# 2.取得船員的生日轉換及變更資料型態
# 宣告 age 向量
age <- as.numeric(straw_hat_df$age)

# 宣告 birthday 向量
birthday <- straw_hat_df$birthday

# 用 sys_date_year_num 減去 age 並指派給 birth_year
birth_year <- sys_date_year_num - age

# 利用 as.character 將 birth_year 轉換成字元並指派給 birth_year_char
birth_year_char <- as.character(birth_year)

# 將 birth_year 與 birth_year_char 輸出在 R Console
class(birth_year)

class(birth_year_char)


## 較難的衍生變數: 船員的生日資料及日期格式的轉換
# 3.產生衍生變數: 字串的結合paste( )
# 結合 birth_year_char 與 birthday
birth_date_char <- paste(birth_year_char, birthday, sep = "-")

# 將 birth_date_char 轉成日期 birth_date
birth_date <- as.Date(birth_date_char)

# 將 birth_date 新增至資料框
straw_hat_df$birth_date <- birth_date

View(straw_hat_df)


### 資料集欄位聚合運算
# 摘要統計(1): summary()
# 對 straw_hat_df 使用 summary()
summary(straw_hat_df)

# 對 straw_hat_df$height 使用 summary()
summary(straw_hat_df$height)

# 對 straw_hat_df$bounty 使用 sum()
straw_hat_df$bounty <- as.numeric(straw_hat_df$bounty)
sum(straw_hat_df$bounty)

# 對 straw_hat_df$bounty 使用 sd()
sd(straw_hat_df$bounty)

# 摘要統計(2)
# 用 head() 函數看一下 straw_hat_df
head(straw_hat_df)

# 載入 plyr 套件
library(plyr)
straw_hat_df$height <- as.numeric(straw_hat_df$height)

# 依據 gender 計算平均身高
ddply(straw_hat_df, .variables = "gender", .fun = summarise, avg_height = mean(height))

# 依據 battle_role 計算加總賞金
ddply(straw_hat_df, .variables = "battle_role", .fun = summarise, ttl_bounty = sum(bounty))

# 依據 gender 與 battle_role 計算平均身高與加總賞金
ddply(straw_hat_df, .variables = c("gender", "battle_role"), .fun = summarise, avg_height = mean(height), ttl_bounty = sum(bounty))


### 資料集欄位資料轉置：寬資料框變為長資料框
## 建立一個新的資料框 straw_hat_wide_df 僅包含姓名、年齡與身高
straw_hat_wide_df <- straw_hat_df[, c("name", "age", "height")]

# 資料集轉置
straw_hat_long_df <- gather(straw_hat_wide_df, key = cate, value = int, height, age)

# 將資料框輸出在 R Console
straw_hat_long_df

# 資料轉置：長資料框變為寬資料框
# 轉置
straw_hat_wide_df <- spread(straw_hat_long_df, key = cate, value = int)

# 將 straw_hat_wide_df 輸出在 R Console看看
straw_hat_wide_df


### 聯結資料框-資料合併: merge()操作，預設為內部聯結(inner join)
name <- c("蒙其·D·魯夫", "多尼多尼·喬巴", "妮可·羅賓", "布魯克")
devil_fruit <- c("橡膠果實", "人人果實", "花花果實", "黃泉果實")
devil_fruit_type <- c("Paramecia", "Zoan", "Paramecia", "Paramecia")
straw_hat_devil_fruit <- data.frame(name, devil_fruit, devil_fruit_type)
straw_hat_df_devil_fruit <- merge(straw_hat_df, straw_hat_devil_fruit)
View(straw_hat_df_devil_fruit) # 只合併有吃惡魔果實的列(4列)

# 左外部聯結
straw_hat_df2_devil_fruit <- merge(straw_hat_df, straw_hat_devil_fruit, all.x = TRUE)

# 將結果輸出在 R Console
straw_hat_df2_devil_fruit


# 右外部聯結
straw_hat_df3_devil_fruit <- merge(straw_hat_df, straw_hat_devil_fruit, all.y = TRUE)

# 將結果輸出在 R Console
straw_hat_df3_devil_fruit


# 全外部聯結
straw_hat_df4_devil_fruit <- merge(straw_hat_df, straw_hat_devil_fruit, all.x = TRUE, all.y = TRUE)

# 將結果輸出在 R Console
straw_hat_df4_devil_fruit



### 資料寫入檔案(硬碟)
#經過資料角力（Data wrangling）後，將整理乾淨的資料輸出成 csv 格式的資料檔
write.csv(straw_hat_df, file = "straw_hat_final.csv", row.names = FALSE)


