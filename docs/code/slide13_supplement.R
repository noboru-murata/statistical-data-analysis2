### 基本的な時系列モデルによる予測
### 厚生労働省のCOVID-19の感染者数データを用いた例

### viewer での表示を利用するために ggplot 系の関数を利用

## パッケージの読み込み
library(forecast)
library(tidyverse)
library(scales) # 年月日表示
library(plotly) 
library(zoo)    # 時系列表示
library(ggfortify)

## 自前の描画関数を用意
myPlot <- function(est,test){
    p <- 
        ggplot(data = fortify(forecast(est,
                                       h=min(length(test),90))) %>%
                   dplyr::mutate(Index=as.Date(Index)) %>%
                   left_join(fortify(test), by = "Index"), 
               mapping = aes(x = Index,
                             y = exp(Data)),
               na.rm = TRUE) +
        geom_line(colour = "skyblue",
                  na.rm = TRUE) +
        geom_line(mapping = aes(y = test),
                  colour = "red",
                  na.rm = TRUE) +
        geom_line(mapping = aes(y = exp(`Point Forecast`)),
                  colour = "royalblue",
                  na.rm = TRUE) +
        geom_ribbon(mapping = aes(ymin = exp(`Lo 80`),
                                  ymax = exp(`Hi 80`)),
                    fill = "royalblue", alpha = 0.3,
                    na.rm = TRUE) +
        scale_x_date(labels = date_format("%y-%m-%d"), 
                     breaks = date_breaks("1 week")) + 
        theme(axis.text.x = element_text(angle = 90,
                                         vjust = 0.5, hjust=1)) +
        labs(title = "Prediction by ARIMA model",
             x = "date",
             y = "number of patients")
    print(p)
    ggplotly()
}

## データの取得と整理 
raw_data <-
    read.csv("https://covid19.mhlw.go.jp/public/opendata/newly_confirmed_cases_daily.csv") %>%
    dplyr::rename(date=1, patients=2) %>% 
    dplyr::mutate(date=as.Date(date))
## 時系列データ(zooクラス)への変更
patients <- with(raw_data,
                 zoo(x=patients, order.by=date))
## raw_data は県別に集計されているので，例えば東京都を調べたい場合は
## patients <- with(raw_data,
##                  zoo(x=Tokyo, order.by=date))
## として，期間やモデルの次数を適宜修正して以下を実行すれば良い

## データの視覚化
p <-
    ggplot(data = fortify(patients, melt = TRUE),
	   mapping = aes(x = Index,
			 y = Value)) +
    scale_x_date(labels = date_format("%y-%m-%d"), # 年月日表示
		 breaks = date_breaks("1 week")) + # 週毎
    theme(axis.text.x = element_text(angle = 90, 
				     vjust = 0.5, hjust=1)) +
    labs(title = "COVID-19 patients in Japan",
	 x = "date",
	 y = "number of patients")
print(p + geom_col(fill="skyblue")) # グラフ出力
ggplotly() # plotly表示 (viewer)

## 第3波
train <- window(patients,
		start="2020-09-15",
		end="2020-11-30")
test <- window(patients,
	       start="2020-12-01",
               end="2021-01-31")
## drift付きのARIMAモデルを推定
(est <- forecast::auto.arima(log(train)))
myPlot(est,test)

## 第4波
train <- window(patients,
		start="2021-02-15",
		end="2021-04-10")
test <- window(patients,
	       start="2021-04-11",
               end="2021-06-01")
## 第3波で推定された次数のARIMAモデルを利用
(est <- forecast::Arima(log(train),c(2,1,2),include.drift=TRUE))
myPlot(est,test)

## 第5波
train <- window(patients,
		start="2021-06-15",
		end="2021-07-20")
test <- window(patients,
	       start="2021-07-21",
               end="2021-09-30")
## 第3波で推定された次数のARIMAモデルを利用
(est <- forecast::Arima(log(train),c(2,1,2),include.drift=TRUE))
myPlot(est,test)

## 第5波 (その2) 
train <- window(patients,
		start="2021-06-15",
		end="2021-07-31")
test <- window(patients,
	       start="2021-08-01",
               end="2021-08-30")
## 第3波で推定された次数のARIMAモデルを利用
(est <- forecast::Arima(log(train),c(2,1,2),include.drift=TRUE))
myPlot(est,test)
## 推定に利用した期間で結果が大きく異なる
## 現実の問題では弱定常の仮定が満されているとは限らない

## 第7波
train <- window(patients,
		start="2022-06-13",
		end="2022-07-15")
test <- window(patients,
	       start="2022-07-15",
               end="2022-08-15")
## 傾向の変化を見るためモデルを再度推定
## drift付きのARIMAモデルが推定される
(est <- forecast::auto.arima(log(train)))
myPlot(est,test)
## 第3波で推定された次数のARIMAモデルを利用
(est <- forecast::Arima(log(train),c(2,1,2),include.drift=TRUE))
myPlot(est,test)

## 第8波 (傾向の変化を見るためモデルを再度推定)
train <- window(patients,
		start="2022-10-10",
		end="2022-11-10")
test <- window(patients,
	       start="2022-11-11",
               end="2022-12-31")
## 傾向の変化を見るためモデルを再度推定
## driftなしのARIMAモデルが推定される
(est <- forecast::auto.arima(log(train)))
myPlot(est,test)
## 第3波で推定された次数のARIMAモデルを利用
(est <- forecast::Arima(log(train),c(2,1,2),include.drift=TRUE))
myPlot(est,test)
## 第7波で推定された次数のARIMAモデルを利用
(est <- forecast::Arima(log(train),c(0,1,1),include.drift=TRUE))
myPlot(est,test)
