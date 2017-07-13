# Multiple Regression 練習
library(plotly)
library(ggplot2)
data("mtcars")
head(mtcars)
cor(mtcars)[1,] # 觀察相關係數
# 整理資料
mycars <- mtcars
#增加總排氣量
mycars$tdisp <- mtcars$cyl * mtcars$disp
# 調整欄位值
mycars$am <- ifelse(mtcars$am == 0, "automatic", "manual");
plot_ly(data = mycars,
        x= mycars$wt,
        y= mycars$mpg,
        type = "scatter",
        mode = "markers")

# 建模
mlm <- lm(mpg~ wt+ hp+ am+ tdisp, data = mycars)
summary(mlm)


# CL station test
clcsv <- read.csv('c://R_Application/Machine Learning/data/CL.csv')

newcl <- data.frame(Rn = as.numeric(clcsv$Rn_conc.), temp = as.numeric(clcsv$Temp), hum = as.numeric(clcsv$Humi))
newclsubet <- subset(newcl, newcl$temp > 0 & newcl$temp < 40)
cor(newclsubet)[2,]

# 隨機抽取10000筆
sampe_g <- newclsubet[sample(10000),]
cor(sampe_g)[1,]
# 建模
CLlm <- lm(Rn~ temp+ hum, data = sampe_g)
summary(CLlm)
# 圖示
RnTemp <- plot_ly(data = sampe_g,
                  x = sampe_g$Rn,
                  y = sampe_g$temp,
                  type = "scatter",
                  mode = "markers")
RnTemp

# JS station

js <- read.csv("c://R_Application/Machine Learning/data/js_train.csv")
timeNew <-as.POSIXct(js$time, "%d/%m/%Y %H:%M:%S", tz = "GMT")
js$time <- timeNew
jsNew <- subset(js, js$Radon != "NA")
Rn_mean <- mean(jsNew$Radon)
Rn_sd <- sd(jsNew$Radon)
p <- plot_ly(x = ~jsNew$Radon, type = "histogram") 
p
# time series

timeSeriesRn <- plot_ly(data = jsNew,
                      x = jsNew$time,
                      y = jsNew$Radon,
                      type = "scatter",
                      mode = "lines",
                      name = "Rn",
                      line = list(color = '#00cc66')) %>%
  add_lines(x = jsNew$time, y = smoothRn, line = list(color = 'red'))
timeSeriesTemp <- plot_ly(data = jsNew,
                      x = jsNew$time,
                      y = jsNew$temperature,
                      type = "scatter",
                      mode = "lines",
                      name = "Temp",
                      line = list(color = '#ff3300'))
timeSeriesHum <- plot_ly(data = jsNew,
                      x = jsNew$time,
                      y = jsNew$humitid,
                      type = "scatter",
                      mode = "lines",
                      name = "Hum",
                      line = list(color = '#884dff'))
timeSeries <- subplot(timeSeriesRn, timeSeriesTemp, timeSeriesHum, nrows = 3, shareX = TRUE)
timeSeries
# ====Rn to hum=====
HumtoRn <- plot_ly(data = js,
                   x = js$Radon,
                   y = js$humitid,
                   type = "scatter",
                   mode = "markers")
HumtoRn

# subset Hum<17
newHumJS <- subset(js, js$humitid<=17 & js$humitid>=12)
less20HumtoRn <- plot_ly(data = newHum,
                   x = newHum$Radon,
                   y = newHum$humitid,
                   type = "scatter",
                   mode = "markers")
less20HumtoRn

# ====Rn to temp in 17>=Hum>=12====
TemptoRn <- plot_ly(data = newHumJS,
                   x = newHumJS$Radon,
                   y = newHumJS$temperature,
                   type = "scatter",
                   mode = "markers")
TemptoRn

# new lm data frame

lmdfJS <- data.frame(Rn = newHumJS$Radon, hum = newHumJS$humitid, temp = newHumJS$temperature)

# sample(10000)
set.seed(1234)
RandomlmdfJS <- lmdfJS[sample(5000),]

n <- nrow(RandomlmdfJS)
#取出樣本數的idx
t_idx <- sample(seq_len(n), size = round(0.7 * n))

#訓練資料與測試資料比例: 70%建模，30%驗證
traindata <- RandomlmdfJS[t_idx,]
testdata <- RandomlmdfJS[ - t_idx,]
cor(traindata)[1,] # 觀察相關係數
mlm <- lm(Rn~ hum+ temp, data = traindata)
summary(mlm)


# 連結分析
library(arules)
data("Groceries")

testLA <- data.frame(d = movie_raw_data$director_name, a1 = movie_raw_data$actor_1_name, a2 = movie_raw_data$actor_2_name)
LA <- Groceries@itemInfo
Groceries@data
rule <- apriori(t2, 
                parameter=list(supp=0.001, conf=0.4, minlen=3),
                appearance = list(default="lhs",
                                  rhs=c("d=James Cameron"))
                
                )
rule0 <- apriori(Groceries, 
                 parameter = list(support = 0.001,
                                  confidence = 0.5))
inspect(rule[1:7])

write.csv(t2, "c://R_Application/Machine Learning/data/Movie.csv")
