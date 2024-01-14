### 第13講 サンプルコード
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(tidyverse)
library(forecast)
library(patchwork)
library(scales)

#' @excercise 自己相関・自己共分散の計算・描画

ggAcf(arima.sim(model = list(ar = c(0.8, -0.64),
                             ma = c(-0.5)),
                n = 200))

#' ---------------------------------------------------------------------------
#' @practice 自己相関

#' 前回作成した自作の関数 my_arma() を利用しても良いが
#' 今回は関数 arima.sim() を用いた方法を紹介する
Tmax <- 500 # 時系列の長さ t=1,..,Tmax
K <- 4 # 表示する時系列の数 (4つを並べて比較する)
ts_ar <- ts(replicate(K, arima.sim(list(ar = c(0.67, 0.26)),
                                   n = Tmax,
                                   innov = rnorm(Tmax))))
ts_ma <- ts(replicate(K, arima.sim(list(ma = c(0.44, -0.28)),
                                   n = Tmax,
                                   innov = rnorm(Tmax))))
ts_arma <- ts(replicate(K, arima.sim(list(ar = c(0.8, -0.64),
                                          ma = c(-0.5)),
                                     n = Tmax,
                                     innov = rnorm(Tmax))))

#' AR(2)モデルの自己相関(patchworkパッケージを用いてグラフを2x2に並べる)
patch <- list()
for(i in 1:K) {
  patch[[i]] <-
    ggAcf(ts_ar[,i],
          colour = hue_pal()(K)[i]) +
    ylim(-0.3,1) + # y軸の範囲を指定して4つのグラフの表示を揃える
    labs(title = paste("AR Series:", i))
}
do.call("wrap_plots", patch)

#' MA(2)モデルの自己相関
for(i in 1:K) {
  patch[[i]] <-
    ggAcf(ts_ma[,i],
          colour = hue_pal()(K)[i]) +
    ylim(-0.3,0.4) +
    labs(title = paste("MA series:", i))
}
do.call("wrap_plots", patch)

#' ARMA(2,1)モデルの自己相関
for(i in 1:K) {
  patch[[i]] <-
    ggAcf(ts_arma[,i],
          colour = hue_pal()(K)[i]) +
    ylim(-0.6,0.4) +
    labs(title = paste("ARMA Series:", i))
}
do.call("wrap_plots", patch)

#' ---------------------------------------------------------------------------

#' ARMA(2,1)モデルの偏自己相関
for(i in 1:K) {
  patch[[i]] <-
    ggPacf(ts_arma[,i], 
           colour = hue_pal()(K)[i]) +
    ylim(-0.6,0.4) +
    labs(title = paste("ARMA series:", i))
}
do.call("wrap_plots", patch)

#' @excercise ARモデルの推定

x <- arima.sim(model = list(ar = c(0.7,-0.6, 0.5)), n = 1000)
ar(x) 
ar(x, method = "mle")

#' @excercise ARIMAモデルの推定

x <- arima.sim(model = list(ar = c(0.7,-0.6,0.5)), n = 1000)
auto.arima(x)  
y <- arima.sim(list(order = c(2,1,1), ar = c(0.8,-0.64), ma = c(-0.5)), n = 1000)
auto.arima(y)

### 
### 練習問題 ARMAモデルの推定
### 

## ARMA過程を生成する関数
myARMA <- function(a, b, epsilon){
    p <- length(a)
    q <- length(b)
    r <- max(p,q)
    Tmax <- length(epsilon) # 時系列の長さを取得
    x <- double(Tmax)
    x[1:r] <- epsilon[1:r]
    for(t in (r+1):Tmax) {
        x[t] <- a %*% x[t-1:p] + b %*% epsilon[t-1:q] + epsilon[t]
    }
    return(x)
}

## AR(2)過程の生成 (以下は前回の例を利用，自由に変更せよ)
x.ar <- ts(myARMA(a=c(0.67, 0.26),
                  b=0,
                  epsilon=rnorm(1000))) # 時系列の長さも自由に変更せよ
## 関数 ar による自動推定
est.ar <- ar(x.ar) 
print(est.ar) # ある程度長い系列であれば良い推定が得られる

## ARMA(2,1)過程の生成
x.arma <- ts(myARMA(a=c(0.8, -0.64),
                    b=c(-0.5),
                    epsilon=rnorm(1000))) 
## 関数 arima による手動推定
est.arma0 <- arima(x.arma, order=c(2, 0, 1)) # 正しいモデル
est.arma1 <- arima(x.arma, order=c(3, 0, 1))
est.arma2 <- arima(x.arma, order=c(2, 0, 2))
print(est.arma0)
print(est.arma1)
print(est.arma2)
## 尤度(likelihood)は大きい方が観測データへのあてはまりは良い
## AICは小さい方が良い予測が良いことが期待される

## 関数 auto.arima による自動推定
## パッケージの読み込み
library(forecast) # 既に読み込んでいれば不要 
est.arma <- auto.arima(x.arma, d=0, D=0)
print(est.arma)
## 必ずしも正しいモデルが推定される訳ではないことに注意
## 特に短い時系列では推定が難しい場合が多い
## 生成する系列の長さを変えて実験してみよう

## 自己相関係数による評価
plot(x.arma) # 元の時系列
acf(x.arma)  # 元の時系列の自己相関
plot(resid(est.arma)) # 推定されたモデルの残差
acf(resid(est.arma))  # 推定されたモデルの残差の自己相関
## 残差は無相関になっていることが確認できる

library(zoo) # forecast を利用すると自動的に読み込まれる
zoo(x = NULL, order.by = index(x), frequency = NULL, ...)
## x: ベクトル，行列
## order.by: 成分の目盛
## frequency: 季節成分の周期
x.zoo <- zoo(x, # データに日付の情報を付加する例 (Dateクラスで指定)
		 order.by = seq(from=as.Date("2021-01-01"), 
				to=as.Date("2021-12-31"), by=1))
start(x.zoo) # index(x.zoo)[1] 最初の日付
end(x.zoo) # index(x.zoo)[length(x.zoo)] 最後の日付

window(x, start = NULL, end = NULL)
## x: ベクトル，行列
## start: 開始時点
## end: 終了時点
window(x, # データに日付の情報が入っている場合 (zooの例)
	   start="2021-12-01", # Dateクラスの標準の書き方
	   end="2021/12/31") # Dateクラスはこちらでも解釈可能

tw_data <- read.csv("data/tokyo_weather.csv")

#' ---------------------------------------------------------------------------
#' @practice 東京の気温データの時系列モデル

## パッケージの読み込み (既に読み込んでいれば不要 )
# library(zoo) 
library(forecast)
tw_data <- read.csv("data/tokyo_weather.csv")
tw_zoo <- with(tw_data,
               zoo(temp,
                   order.by = as.Date(paste(year,month,day,sep="-"))))

## データの視覚化を行う
plot(tw_zoo, col="red",
     xlab="month", ylab="degree", main="Temperature in Tokyo")
plot(window(tw_zoo, # 一部を切り出して視覚化する
            start=as.Date("2021-06-01"),
            end=as.Date("2021-07-31")),
     col="red",
     xlab="date", ylab="degree", main="Temperature (June-July)")
acf(tw_zoo)       # 減衰が遅いので差分をとった方が良さそう
plot(diff(tw_zoo)) # 階差系列の視覚化
acf(diff(tw_zoo))  # 階差系列の自己相関

## 階差系列にARMAモデルをあてはめる (d=1)
tw_fit <- auto.arima(tw_zoo, d=1, D=0)
summary(tw_fit) # 推定されたモデルの仕様を表示
acf(resid(tw_fit)) # そこそこあてはまりは良さそう

#' ---------------------------------------------------------------------------

## ランダムウォークの予測
autoplot(forecast(myARMA(a=c(1),b=c(0),rnorm(180)),h=20),col=2)

## ARMA 過程の予測
autoplot(forecast(myARMA(a=c(0.8, -0.64),b=c(-0.5),rnorm(180)),h=20),col=2)

### 基本的な時系列モデルによる予測
### 厚生労働省のCOVID-19の感染者数データを用いた例

## パッケージの読み込み
library(forecast)
library(tidyverse)
library(scales) # 年月日表示
library(plotly) 
library(zoo)    # 時系列表示
library(ggfortify)

## データの取得と整理 
patients <-
    read.csv("https://covid19.mhlw.go.jp/public/opendata/newly_confirmed_cases_daily.csv") %>%
    dplyr::rename(date=1, patients=2) %>% 
    dplyr::mutate(date=as.Date(date))
## 時系列データ(zooクラス)への変更
patients <- with(patients,
                   zoo(x=patients, order.by=date))

## データの視覚化
p <-
    ggplot(data = fortify(patients, melt = TRUE),
           mapping = aes(x = Index,
                         y = Value)) +
    scale_x_date(labels = date_format("%y-%m-%d"), # 年月日表示
                 breaks = date_breaks("1 month")) + # 週毎
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, hjust=1)) +
    labs(title = "COVID-19 patients in Japan",
         x = "date",
         y = "number of patients")
## 棒グラフ
print(p + geom_col(fill="skyblue")) # グラフ出力

## 第3波 (2020/9/15-2021/1/31)
p <-
  ggplot(data = fortify(window(patients,
                               start="2020-09-15",
                               end="2021-01-31"),
                        melt = TRUE),
         mapping = aes(x = Index,
                       y = Value)) +
  scale_x_date(labels = date_format("%y-%m-%d"), # 年月日表示
               breaks = date_breaks("1 week")) + # 週毎
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1)) +
  labs(title = "COVID-19 patients in Japan",
       x = "date",
       y = "number of patients")
## 棒グラフ
print(p + geom_col(fill="skyblue")) # グラフ出力

## 9月以降の第3波を対象とする
train <- window(patients,
                start="2020-09-15",
                end="2020-11-30")
test <- window(patients,
               start="2020-12-01")

## 階差系列の性質
autoplot(diff(train)) +
  labs(x = "date",
       y = "D(patients)")

autoplot(acf(diff(train), plot = FALSE), # 自己相関
         conf.int.fill = "royalblue",
         conf.int.alpha =0.2,
         conf.int.value = 0.7,
         conf.int.type = "ma") +
  labs(title = "D(patients)") +
  ylim(c(-0.5,1.0))

autoplot(pacf(diff(train), plot = FALSE), # 偏自己相関
         conf.int.fill = "royalblue",
         conf.int.alpha =0.2,
         conf.int.value = 0.7) +
  labs(title = "D(patients)",
       y = "PACF") +
  ylim(c(-0.5,1.0))

## 対数変換を確認する
ltrain <- log(train)
autoplot(diff(ltrain)) +
    labs(x = "date",
         y = "D(log(patients))")

autoplot(acf(diff(ltrain), plot = FALSE), # 自己相関
         conf.int.fill = "royalblue",
         conf.int.alpha =0.2,
         conf.int.value = 0.7,
         conf.int.type = "ma") +
  labs(title = "D(log(patients))") +
  ylim(c(-0.5,1.0))

autoplot(pacf(diff(ltrain), plot = FALSE), # 偏自己相関
         conf.int.fill = "royalblue",
         conf.int.alpha =0.2,
         conf.int.value = 0.7) +
  labs(title = "D(log(patients))",
       y = "PACF") +
  ylim(c(-0.5,1.0))

## 7日の周期性を確認する
autoplot(diff(diff(ltrain), lag=7)) +
    labs(x = "date",
         y = "D7*D(log(patients))")

autoplot(acf(diff(diff(ltrain), lag=7), plot = FALSE), # 自己相関
         conf.int.fill = "royalblue",
         conf.int.alpha =0.2,
         conf.int.value = 0.7,
         conf.int.type = "ma") +
  labs(title = "D7*D(log(patients))") +
  ylim(c(-0.5,1.0))

autoplot(pacf(diff(diff(ltrain), lag=7), plot = FALSE), # 偏自己相関
         conf.int.fill = "royalblue",
         conf.int.alpha =0.2,
         conf.int.value = 0.7) +
  labs(title = "D7*D(log(patients))",
       y = "PACF") +
  ylim(c(-0.5,1.0))

## drift付きのARIMAモデルの次数を自動推定
est.arima <- forecast::auto.arima(ltrain)
## 推定されたモデルを表示
print(est.arima)
## SARIMAモデルを当て嵌める場合は周期を指定する．
## frequency(ltrain) <- 7 # 7日周期の成分を仮定
## (est.arima7 <- auto.arima(ltrain))
## このデータではモデルの推定はうまくいかない

## モデルによる当て嵌めの視覚化
p <- 
  ggplot(data = fortify(est.arima) %>%
           dplyr::mutate(Index=as.Date(Index)),
         mapping = aes(x = Index,
                       y = Data)) +
  geom_line(colour = "skyblue") +
  geom_line(mapping = aes(y = Fitted),
            colour = "orange") +
  scale_x_date(labels = date_format("%y-%m-%d"), 
               breaks = date_breaks("1 week")) + 
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1)) +
  labs(title = "Fitted by ARIMA model",
       x = "date",
       y = "log(patients)")
print(p)

## 診断プロット
tsdiag(est.arima)
## 残差に相関が残っているので，優れたモデルという訳ではない

## 12月以降(最大60日)を予測してみる
p <- 
  ggplot(data = fortify(forecast(est.arima,
                                 h=min(length(test),60))) %>%
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
  ## geom_ribbon(mapping = aes(ymin = exp(`Lo 95`),
  ##   			ymax = exp(`Hi 95`)),
  ##   	  fill = "royalblue", alpha = 0.1,
  ##   	  na.rm = TRUE) +
  scale_x_date(labels = date_format("%y-%m-%d"), 
               breaks = date_breaks("1 week")) + 
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1)) +
  labs(title = "Prediction by ARIMA model",
       x = "date",
       y = "number of patients")
print(p)

## 第8波 (2022/10/10-現在)
p <-
  ggplot(data = fortify(window(patients,
                               start="2022-10-10"),
                        melt = TRUE),
         mapping = aes(x = Index,
                       y = Value)) +
  scale_x_date(labels = date_format("%y-%m-%d"), # 年月日表示
               breaks = date_breaks("1 week")) + # 週毎
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1)) +
  labs(title = "COVID-19 patients in Japan",
       x = "date",
       y = "number of patients")
## 棒グラフ
print(p + geom_col(fill="skyblue")) # グラフ出力

## 9月以降の第3波を対象とする
  train <- window(patients,
                  start="2022-11-01",
                  end="2022-11-30")
#                  start="2022-10-10",
#                  end="2022-11-10")
  test <- window(patients,
                 start="2022-12-01")
#                 start="2022-11-11")
  ltrain <- log(train)

  ## 第3波で推定された次数のARIMAモデルを利用
  est.arima <- forecast::Arima(log(train),c(2,1,2),include.drift=TRUE)
  ## 推定されたモデルを表示
  print(est.arima)
  ## 自動選択だと良いモデルが選択されない
  ## est.arima <- forecast::auto.arima(ltrain)
  ## SARIMAモデルを当て嵌める場合は周期を指定する．
  ## frequency(ltrain) <- 7 # 7日周期の成分を仮定
  ##  (est.arima7 <- auto.arima(ltrain))
  ## このデータではモデルの推定はうまくいかない

## 診断プロット
tsdiag(est.arima)
## 残差に相関が残っているので，優れたモデルという訳ではない

## モデルによる当て嵌めの視覚化
p <- 
  ggplot(data = fortify(est.arima) %>%
           dplyr::mutate(Index=as.Date(Index)),
         mapping = aes(x = Index,
                       y = Data)) +
  geom_line(colour = "skyblue") +
  geom_line(mapping = aes(y = Fitted),
            colour = "orange") +
  scale_x_date(labels = date_format("%y-%m-%d"), 
               breaks = date_breaks("1 week")) + 
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1)) +
  labs(title = "Fitted by ARIMA model",
       x = "date",
       y = "log(patients)")
print(p)

## 12月以降(最大60日)を予測してみる
p <- 
  ggplot(data = fortify(forecast(est.arima,
                                 h=min(length(test),60))) %>%
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
  ## geom_ribbon(mapping = aes(ymin = exp(`Lo 95`),
  ##   			ymax = exp(`Hi 95`)),
  ##   	  fill = "royalblue", alpha = 0.1,
  ##   	  na.rm = TRUE) +
  scale_x_date(labels = date_format("%y-%m-%d"), 
               breaks = date_breaks("1 week")) + 
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1)) +
  labs(title = "Prediction by ARIMA model",
       x = "date",
       y = "number of patients")
print(p)

predict(object, newdata, n.ahead = 1, se.fit = TRUE, ...)
## object: ar また arima による推定結果
## newdata: 予測対象のデータ (arの場合のみ)
## n.ahead: n期先の予測
## se.fit: 標準誤差を付加するか否か
x.fit <- arima(x, order=c(0,1,1),
		   seasona=list(order=c(0,1,1), period=12))
x.prd <- predict(x.fit, n.ahead=10)
x.prd$pred # 予測値 (標準誤差は $se)

forecast(object, h)
## object: ar また arima による推定結果
## h: h期先の予測 (指定しないと2周期または10期先を予測)
x.fit <- auto.arima(x, d=1, D=1)
x.prd <- forecast(x.fit, h=10)
x.prd$mean # 予測値 (信頼区間は $upper/$lower)
plot(x.prd) # 全体を視覚化

StructTS(x, type = "level", fixed = NULL, ...)
## x: 時系列データ
## type: "level" 平均の変動をランダムウォークでモデル化
##       "trend" 平均と傾きをランダムウォークでモデル化
##       "BSM" 季節成分を含むモデル (frequencyが必要)
## fixed: ホワイトノイズの分散の指定
x.sts <- StructTS(x, type = "trend", fixed = c(0.1,NA,NA))
## 平均のホワイトノイズの分散を0.1，傾きとランダム成分の分散は推定
forecast(x.sts, h=10) # predictを使うことも可

### 
### 練習問題 東京の気温の予測
### 

## パッケージの読み込み (既に読み込んでいれば不要)
library(zoo) 
library(forecast) 

## データの読み込み (既に行っていれば不要)
tw_data <- read.csv("data/tokyo_weather.csv")
tw_zoo <- with(tw_data,
               zoo(temp,
                   order.by = as.Date(paste(year,month,day,sep="-"))))
## データの整理
tw_train <- window(tw_zoo, # 6月までのデータ (訓練データ)
                   end="2021-06-30")  
tw_test  <- window(tw_zoo, # 7月のデータ (試験データ)
                   start="2021-07-01", end="2021-07-31") 

## auto.arima による推定
(tw_auto <- auto.arima(tw_train, d=1, D=0)) 
(tw_fcst <- forecast(tw_auto, h=length(tw_test)))

## 視覚化
plot(tw_fcst) # X軸が無粋 (1970-01-01からの日数)

## X軸の書き直し
plot(tw_fcst, xaxt="n",
     xlim=c(as.Date("2021-06-01"), as.Date("2021-07-31")))
axis(side=1, # x軸を指定
     at=index(tw_zoo), # 文字を書く座標軸上の位置
     labels=index(tw_zoo), # ラベル
     las=2, # 垂直に表示
     cex.axis=0.7) # 文字の大きさを調整
lines(tw_test, col="red") # 真値を重ね描き

## 別の書き方
plot(window(tw_zoo, start="2021-06-01", end="2021-07-31"),
     col="darkgray",
     xlab="date", ylab="temperature")
with(tw_fcst, lines(mean, col="red", lwd=3))    # 予測値
with(tw_fcst, lines(upper[,1], col="orange", lwd=3)) # +80%信頼区間
with(tw_fcst, lines(upper[,2], col="orchid", lwd=3)) # +95%信頼区間
with(tw_fcst, lines(lower[,1], col="orange", lwd=3)) # -80%信頼区間
with(tw_fcst, lines(lower[,2], col="orchid", lwd=3)) # -95%信頼区間

## StructTS による推定
(tw_sts <- StructTS(tw_train, type="trend", fixed=c(0.1,NA,NA)))
(tw_fsts <- forecast(tw_sts, h=length(tw_test)))

## 分解結果の表示 
plot(merge(tw_train, fitted(tw_sts)), col="blue")

## 視覚化
plot(tw_fsts, xaxt="n",
     xlim=c(as.Date("2021-06-01"), as.Date("2021-07-31")))
axis(side=1, # x軸を指定
     at=index(tw_zoo), # 文字を書く座標軸上の位置
     labels=index(tw_zoo), # ラベル
     las=2, # 垂直に表示
     cex.axis=0.7) # 文字の大きさを調整
lines(tw_test, col="red") # 真値を重ね描き

### 
### 練習問題 AirPassengersデータの分析
### 

## AirPassengersデータの読み込み
library(forecast)
data(AirPassengers)
tsp(AirPassengers) # データの時間に関する情報を表示 (月ごとのデータ)
plot(AirPassengers, col="blue") # データの表示
plot(log(AirPassengers), col="blue") # 対数変換データの表示
## 対数変換により分散変動が安定化していることがわかる

## 以下では対数変換したデータを扱う
ap_train <- window(log(AirPassengers), end=c(1957,12))  # 訓練データ
ap_test  <- window(log(AirPassengers), start=c(1958,1)) # 試験データ

## まずトレンド(明らかな上昇傾向)について考察
## 階差を取ることにより定常化できるか検討
plot(diff(ap_train), col="blue") 
acf(diff(ap_train))  # 自己相関
pacf(diff(ap_train)) # 偏自己相関
## lag=1(1年)に強い(偏)自己相関(季節成分)がある

## 季節成分について考察
## 12ヶ月で階差を取って同様に検討
plot(diff(diff(ap_train), lag=12), col="blue")
acf(diff(diff(ap_train), lag=12), lag.max=24)  # 自己相関
pacf(diff(diff(ap_train), lag=12), lag.max=24) # 偏自己相関

## lag=1/12,3/12,1 に若干偏自己相関が残っている
## lag=2 (2年) まで見ると自己相関も偏自己相関も誤差内

## SARIMAモデルの作成
##  階差系列については ARMA(1-3)，
##  季節成分 については ARMA(1-2)
## あたりを考える必要がありそう

## 関数arimaを用いる場合
## 季節成分によるARMA項の指定はseasonalオプションを用いる
## 例えば seasonal=list(order=c(0,1,2),period=12) で
## 差分(1階)= e(t) + b(12)*e(t-12) + b(24)*e(t-24) のMA(2)モデルを指定
## orderとseasonal/orderでそれぞれ1ヶ月階差と12ヶ月階差を取ることに注意
## seasonalのperiodは既定値では時系列のfrequencyを用いるので通常は指定不要
## 例
(ap_arima <- arima(ap_train,
                   order=c(0,1,2), # 階差1のMA(2)
                   seasonal=list(order=c(0,1,1)))) # 12ヶ月階差1のMA(1)
tsdiag(ap_arima) # 時系列モデルの診断図

## 自動的にモデル選択を行う
(ap_auto <- auto.arima(ap_train, d=1, D=1))
tsdiag(ap_auto) # 時系列モデルの診断図
## AIC最小のモデルは以下となる
## arima(x, order=c(0,1,1), seasonal=list(order=c(0,1,1)))

## 予測値と標準偏差の計算
plot(forecast(ap_auto, h=length(ap_test))) # 対数変換していることに注意
lines(ap_test, col="red") # 真の値

## 関数 predict を利用して元のデータ空間に戻す
ap_pred <- predict(ap_auto, n.ahead=length(ap_test))

## 対数データにおける予測+/-標準偏差の表示
library(tseries)
seqplot.ts(x=ap_train, y=ap_test,
           colx="gray", coly="red", 
           ylab="passengers/month (log)")
with(ap_pred,
     lines(pred, col="blue", lwd=2))
with(ap_pred, lines(pred+1.96*se, col="darkblue")) # 95%の信頼区間
with(ap_pred, lines(pred-1.96*se, col="darkblue")) # 1.28なら80%

## もとのデータの空間に戻してみる (指数変換 <-> 対数変換)
seqplot.ts(x=exp(ap_train), y=exp(ap_test),
           colx="gray", coly="red",
           ylab="passengers/month")
with(ap_pred,
     lines(exp(pred), col="blue", lwd=2))
with(ap_pred, lines(exp(pred+1.96*se), col="darkblue")) 
with(ap_pred, lines(exp(pred-1.96*se), col="darkblue"))

## 時系列の分解
## basic structure model による分析
## トレンド(level+slope) + 季節(12ヶ月周期) + ランダム
## 自動的にモデル選択を行う
(ap_sts <- StructTS(ap_train, type="BSM"))
plot(cbind(obs=ap_train, fit=fitted(ap_sts))) # 分解結果の視覚化
tsdiag(ap_sts) # 時系列モデルの診断図
## slopeの変動が季節成分の影響を受けて大きいので，推定に制限を付ける
(ap_sts <- StructTS(ap_train, type="BSM",
                    fixed=c(NA,0,NA,NA))) # slopeの推定を滑らかに
plot(cbind(obs=ap_train, fit=fitted(ap_sts))) 
tsdiag(ap_sts) 
## fixed=c(0,0,NA,NA) とすればlevelの推定も滑らかになる

## 予測値と標準偏差の計算
plot(forecast(ap_sts, h=length(ap_test))) # 対数変換していることに注意
lines(ap_test, col="red") # 真の値

## 関数 predict を利用して元のデータ空間に戻す
ap_psts <- predict(ap_sts, n.ahead=length(ap_test))
seqplot.ts(x=exp(ap_train), y=exp(ap_test),
           colx="gray", coly="red",
           ylab="passengers/month")
with(ap_psts,
     lines(exp(pred), col="blue", lwd=2))
with(ap_psts, lines(exp(pred+1.96*se), col="darkblue")) 
with(ap_psts, lines(exp(pred-1.96*se), col="darkblue"))

### 
### 練習問題 厚生労働省のCOVID-19の感染者数データ
### 

## データの取得と整理 
cp_data <- subset(
    x = read.csv("https://covid19.mhlw.go.jp/public/opendata/newly_confirmed_cases_daily.csv"),
    select = 1:2)
names(cp_data) <- c("date","patients")
cp_data$date <- as.Date(cp_data$date)
head(cp_data)

## 時系列データ(zooクラス)への変更
cp_zoo <- with(cp_data,zoo(x=patients, order.by=date))
plot(cp_zoo, col="blue")

## 対象を限定する
cp_sub <- window(cp_zoo, start="2022-06-01")

plot(diff(cp_sub), col="blue") 
acf(diff(cp_sub))  # 自己相関
pacf(diff(cp_sub)) # 偏自己相関
## 7日周期の影響があることがわかる
plot(diff(diff(cp_sub), lag=7), col="blue")
acf(diff(diff(cp_sub), lag=7), lag.max=21)
pacf(diff(diff(cp_sub), lag=7), lag.max=21)

## 対数変換を確認する
cp_log <- log(cp_sub)
plot(diff(cp_log), col="blue") 
acf(diff(cp_log))  # 自己相関
pacf(diff(cp_log)) # 偏自己相関
plot(diff(diff(cp_log), lag=7), col="blue")
acf(diff(diff(cp_log), lag=7), lag.max=21)
pacf(diff(diff(cp_log), lag=7), lag.max=21)

## cp_log にもとづいて予測を行う

## auto.arima による方法
## 周期を指定して分析してみる
frequency(cp_log) <- 7 # 7日周期の成分を仮定
(cp_auto <- auto.arima(cp_log))
## モデルの推定としてはうまくいかない，おそらく周期性が曖昧なため
frequency(cp_log) <- 1 # 周期なしとして推定
(cp_auto <- auto.arima(cp_log))
## ARIMA(3,1,4)としてモデル化
tsdiag(cp_auto)
## 残差に相関が残っているので，優れたモデルという訳ではない
plot(cp_log, col="blue", ylab="log(patients)")
lines(fitted(cp_auto), col="orange")
## 50日先まで予測してみる
cp_date <- seq(from=start(cp_zoo), to=as.Date("2023-03-31"), by=1)
plot(forecast(cp_auto, h=50), xaxt="n")
axis(side=1, 
     at=cp_date, labels=cp_date, las=2, 
     cex.axis=0.7) 
## 対数変換
cp_fauto <- forecast(cp_auto, h=50)
seqplot.ts(x=as.ts(cp_sub), y=exp(with(cp_fauto, mean)),
           colx="gray", coly="red",
           ylab="patients")
with(cp_fauto, lines(exp(lower[,1]), col="darkblue")) # 80%信頼区間
with(cp_fauto, lines(exp(upper[,1]), col="darkblue")) 

## StructTS による方法
(cp_sts <- StructTS(cp_log))
plot(fitted(cp_sts))
plot(forecast(cp_sts, h=50), xaxt="n")
lines(cp_log)
axis(side=1, 
     at=cp_date, labels=cp_date, las=2, 
     cex.axis=0.7)
## 対数変換
## StructTSの返値は少し整理が必要なので注意 (別の書き方の例)
cp_fsts <- forecast(cp_sts, h=50)
plot(c(exp(cp_log), # zooクラスを連結する
       zoo(exp(with(cp_fsts, upper[,1])), 
           order.by=as.Date(with(cp_fsts, index(mean))))),
     col="white", ylab="patients", xlab="", xaxt="n")
axis(side=1, 
     at=cp_date, labels=cp_date, las=2, 
     cex.axis=0.7)
lines(exp(cp_log), col="gray")
with(cp_fsts, lines(exp(mean), col="blue"))
with(cp_fsts, lines(ts(exp(lower[,1]),
                       start=with(cp_fsts, start(mean))),
                    col="darkblue")) # 80%信頼区間
with(cp_fsts, lines(ts(exp(upper[,1]),
                       start=with(cp_fsts, start(mean))),
                    col="darkblue")) # 80%信頼区間
