### 
### 第13講 サンプルコード
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

set.seed(1234) # 乱数のシード
Tmax <- 500 # 時系列の長さ t=1,..,Tmax
K <- 4 # 表示する時系列の数 (4つを並べて比較する)
library(RColorBrewer)
my_col <- brewer.pal(K,"Dark2")
df.ar <- ts(replicate(K, myARMA(a=c(0.67, 0.26), b=c(0),
				epsilon=rnorm(Tmax))))
df.ma <- ts(replicate(K, myARMA(a=c(0), b=c(0.44, 0.08),
				epsilon=rnorm(Tmax))))
df.arma <- ts(replicate(K, myARMA(a=c(0.8, -0.64), b=c(-0.5),
				  epsilon=rnorm(Tmax))))

plot(x=df.ar, plot.type="single",
     ylab="value", col=my_col,
     main=expression(X[t] == 0.67*X[t-1] + 0.26*X[t-2] + epsilon[t]))

### AR(2)モデルの自己相関
orgpar <- par(mfrow=c(2,2)) # グラフを2x2(行方向の順)に並べる
for(i in 1:K) {
  acf(df.ar[,i], col=my_col[i], main=paste("AR series",i))
}
par(orgpar)

plot(x=df.ma, plot.type="single",
     ylab="value", col=my_col,
     main=expression(X[t] == 0.44*epsilon[t-1] + 0.08*epsilon[t-2] + epsilon[t]))

### MA(2)モデルの自己相関
orgpar <- par(mfrow=c(2,2))
for(i in 1:K) {
    acf(df.ma[,i], col=my_col[i], main=paste("MA series",i))
}
par(orgpar)

plot(x=df.arma, plot.type="single",
     ylab="value", col=my_col,
     main=expression(X[t] == 0.8*X[t-1] - 0.64*X[t-2] - 0.5*epsilon[t-1] + epsilon[t]))

### ARMA(2,1)モデルの自己相関
orgpar <- par(mfrow=c(2,2))
for(i in 1:K) {
    acf(df.arma[,i], col=my_col[i], main=paste("ARMA series",i))
}
par(orgpar)

### AR(2)モデルの偏自己相関
orgpar <- par(mfrow=c(2,2)) # グラフを2x2(行方向の順)に並べる
for(i in 1:K) {
    pacf(df.ar[,i], col=my_col[i], main=paste("AR series",i))
}
par(orgpar)

### MA(2)モデルの偏自己相関
orgpar <- par(mfrow=c(2,2))
for(i in 1:K) {
    pacf(df.ma[,i], col=my_col[i], main=paste("MA series",i))
}
par(orgpar)

### ARMA(2,1)モデルの偏自己相関
orgpar <- par(mfrow=c(2,2))
for(i in 1:K) {
    pacf(df.arma[,i], col=my_col[i], main=paste("ARMA series",i))
}
par(orgpar)

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

### 
### 練習問題 東京の気温データの時系列モデル
### 

## パッケージの読み込み (既に読み込んでいれば不要 )
library(zoo) 
library(forecast)
TW.data <- read.csv("data/tokyo_weather.csv")
TW.zoo <- with(TW.data,
               zoo(temp,
                   order.by = as.Date(paste(year,month,day,sep="-"))))

## データの視覚化を行う
plot(TW.zoo, col="red",
     xlab="month", ylab="degree", main="Temperature in Tokyo")
plot(window(TW.zoo, # 一部を切り出して視覚化する
            start=as.Date("2020-06-01"),
            end=as.Date("2020-07-31")),
     col="red",
     xlab="date", ylab="degree", main="Temperature (June-July)")
acf(TW.zoo)       # 減衰が遅いので差分をとった方が良さそう
plot(diff(TW.zoo)) # 階差系列の視覚化
acf(diff(TW.zoo))  # 階差系列の自己相関

## 階差系列にARMAモデルをあてはめる (d=1)
TW.fit <- auto.arima(TW.zoo, d=1, D=0)
summary(TW.fit) # 推定されたモデルの仕様を表示
acf(resid(TW.fit)) # そこそこあてはまりは良さそう

### 
### 練習問題 東京の気温の予測
### 

## パッケージの読み込み (既に読み込んでいれば不要)
library(zoo) 
library(forecast) 

## データの読み込み (既に行っていれば不要)
TW.data <- read.csv("data/tokyo_weather.csv")
TW.zoo <- with(TW.data,
               zoo(temp,
                   order.by = as.Date(paste(year,month,day,sep="-"))))
## データの整理
TW.train <- window(TW.zoo, # 6月までのデータ (訓練データ)
                   end="2020-06-30")  
TW.test  <- window(TW.zoo, # 7月のデータ (試験データ)
                   start="2020-07-01", end="2020-07-31") 

## auto.arima による推定
(TW.auto <- auto.arima(TW.train, d=1, D=0)) 
(TW.fcst <- forecast(TW.auto, h=length(TW.test)))

## 視覚化
plot(TW.fcst) # X軸が無粋 (1970-01-01からの日数)

## X軸の書き直し
plot(TW.fcst, xaxt="n",
     xlim=c(as.Date("2020-06-01"), as.Date("2020-07-31")))
axis(side=1, # x軸を指定
     at=index(TW.zoo), # 文字を書く座標軸上の位置
     labels=index(TW.zoo), # ラベル
     las=2, # 垂直に表示
     cex.axis=0.7) # 文字の大きさを調整
lines(TW.test, col="red") # 真値を重ね描き

## 別の書き方
plot(window(TW.zoo, start="2020-06-01", end="2020-07-31"),
     col="darkgray",
     xlab="date", ylab="temperature")
with(TW.fcst, lines(mean, col="red", lwd=3))    # 予測値
with(TW.fcst, lines(upper[,1], col="orange", lwd=3)) # +80%信頼区間
with(TW.fcst, lines(upper[,2], col="orchid", lwd=3)) # +95%信頼区間
with(TW.fcst, lines(lower[,1], col="orange", lwd=3)) # -80%信頼区間
with(TW.fcst, lines(lower[,2], col="orchid", lwd=3)) # -95%信頼区間

## StructTS による推定
(TW.sts <- StructTS(TW.train, type="trend", fixed=c(0.1,NA,NA)))
(TW.fsts <- forecast(TW.sts, h=length(TW.test)))

## 分解結果の表示 
plot(merge(TW.train, fitted(TW.sts)), col="blue")

## 視覚化
plot(TW.fsts, xaxt="n",
     xlim=c(as.Date("2020-06-01"), as.Date("2020-07-31")))
axis(side=1, # x軸を指定
     at=index(TW.zoo), # 文字を書く座標軸上の位置
     labels=index(TW.zoo), # ラベル
     las=2, # 垂直に表示
     cex.axis=0.7) # 文字の大きさを調整
lines(TW.test, col="red") # 真値を重ね描き

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
AP.train <- window(log(AirPassengers), end=c(1957,12))  # 訓練データ
AP.test  <- window(log(AirPassengers), start=c(1958,1)) # 試験データ

## まずトレンド(明らかな上昇傾向)について考察
## 階差を取ることにより定常化できるか検討
plot(diff(AP.train), col="blue") 
acf(diff(AP.train))  # 自己相関
pacf(diff(AP.train)) # 偏自己相関
## lag=1(1年)に強い(偏)自己相関(季節成分)がある

## 季節成分について考察
## 12ヶ月で階差を取って同様に検討
plot(diff(diff(AP.train), lag=12), col="blue")
acf(diff(diff(AP.train), lag=12), lag.max=24)  # 自己相関
pacf(diff(diff(AP.train), lag=12), lag.max=24) # 偏自己相関

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
(AP.arima <- arima(AP.train,
                   order=c(0,1,2), # 階差1のMA(2)
                   seasonal=list(order=c(0,1,1)))) # 12ヶ月階差1のMA(1)
tsdiag(AP.arima) # 時系列モデルの診断図

## 自動的にモデル選択を行う
(AP.auto <- auto.arima(AP.train, d=1, D=1))
tsdiag(AP.auto) # 時系列モデルの診断図
## AIC最小のモデルは以下となる
## arima(x, order=c(0,1,1), seasonal=list(order=c(0,1,1)))

## 予測値と標準偏差の計算
plot(forecast(AP.auto, h=length(AP.test))) # 対数変換していることに注意
lines(AP.test, col="red") # 真の値

## 関数 predict を利用して元のデータ空間に戻す
AP.pred <- predict(AP.auto, n.ahead=length(AP.test))

## 対数データにおける予測+/-標準偏差の表示
library(tseries)
seqplot.ts(x=AP.train, y=AP.test,
           colx="gray", coly="red", 
           ylab="passengers/month (log)")
with(AP.pred,
     lines(pred, col="blue", lwd=2))
with(AP.pred, lines(pred+1.96*se, col="darkblue")) # 95%の信頼区間
with(AP.pred, lines(pred-1.96*se, col="darkblue")) # 1.28なら80%

## もとのデータの空間に戻してみる (指数変換 <-> 対数変換)
seqplot.ts(x=exp(AP.train), y=exp(AP.test),
           colx="gray", coly="red",
           ylab="passengers/month")
with(AP.pred,
     lines(exp(pred), col="blue", lwd=2))
with(AP.pred, lines(exp(pred+1.96*se), col="darkblue")) 
with(AP.pred, lines(exp(pred-1.96*se), col="darkblue"))

## 時系列の分解
## basic structure model による分析
## トレンド(level+slope) + 季節(12ヶ月周期) + ランダム
## 自動的にモデル選択を行う
(AP.sts <- StructTS(AP.train, type="BSM"))
plot(cbind(obs=AP.train, fit=fitted(AP.sts))) # 分解結果の視覚化
tsdiag(AP.sts) # 時系列モデルの診断図
## slopeの変動が季節成分の影響を受けて大きいので，推定に制限を付ける
(AP.sts <- StructTS(AP.train, type="BSM",
                    fixed=c(NA,0,NA,NA))) # slopeの推定を滑らかに
plot(cbind(obs=AP.train, fit=fitted(AP.sts))) 
tsdiag(AP.sts) 
## fixed=c(0,0,NA,NA) とすればlevelの推定も滑らかになる

## 予測値と標準偏差の計算
plot(forecast(AP.sts, h=length(AP.test))) # 対数変換していることに注意
lines(AP.test, col="red") # 真の値

## 関数 predict を利用して元のデータ空間に戻す
AP.psts <- predict(AP.sts, n.ahead=length(AP.test))
seqplot.ts(x=exp(AP.train), y=exp(AP.test),
           colx="gray", coly="red",
           ylab="passengers/month")
with(AP.psts,
     lines(exp(pred), col="blue", lwd=2))
with(AP.psts, lines(exp(pred+1.96*se), col="darkblue")) 
with(AP.psts, lines(exp(pred-1.96*se), col="darkblue"))

### 
### 練習問題 厚生労働省のCOVID-19の感染者数データ
### 

## データの取得と整理 
myData <- subset(
    x = read.csv("https://covid19.mhlw.go.jp/public/opendata/newly_confirmed_cases_daily.csv"),
    select = 1:2)
names(myData) <- c("date","patients")
myData$date <- as.Date(myData$date)
head(myData)

## 時系列データ(zooクラス)への変更
CP.zoo <- with(myData,zoo(x=patients, order.by=date))
plot(CP.zoo, col="blue")

## 対象を限定する
CP.sub <- window(CP.zoo, start="2021-04-01")

plot(diff(CP.sub), col="blue") 
acf(diff(CP.sub))  # 自己相関
pacf(diff(CP.sub)) # 偏自己相関
## 7日周期の影響があることがわかる
plot(diff(diff(CP.sub), lag=7), col="blue")
acf(diff(diff(CP.sub), lag=7), lag.max=21)
pacf(diff(diff(CP.sub), lag=7), lag.max=21)

## 対数変換を確認する
CP.log <- log(CP.sub)
plot(diff(CP.log), col="blue") 
acf(diff(CP.log))  # 自己相関
pacf(diff(CP.log)) # 偏自己相関
plot(diff(diff(CP.log), lag=7), col="blue")
acf(diff(diff(CP.log), lag=7), lag.max=21)
pacf(diff(diff(CP.log), lag=7), lag.max=21)

## CP.log にもとづいて予測を行う

## auto.arima による方法
## 周期を指定して分析してみる
frequency(CP.log) <- 7 # 7日周期の成分を仮定
(CP.auto <- auto.arima(CP.log))
## モデルの推定としてはうまくいかない，おそらく周期性が曖昧なため
frequency(CP.log) <- 1 # 周期なしとして推定
(CP.auto <- auto.arima(CP.log))
## drift付きのARIMA(3,1,1)としてモデル化
tsdiag(CP.auto)
## 残差に相関が残っているので，優れたモデルという訳ではない
plot(CP.log, col="blue", ylab="log(patients)")
lines(fitted(CP.auto), col="orange")
## 50日先まで予測してみる
CP.date <- seq(from=start(CP.zoo), to=as.Date("2022-03-31"), by=1)
plot(forecast(CP.auto, h=50), xaxt="n")
axis(side=1, 
     at=CP.date, labels=CP.date, las=2, 
     cex.axis=0.7) 
## 対数変換
CP.fauto <- forecast(CP.auto, h=50)
seqplot.ts(x=as.ts(CP.sub), y=exp(with(CP.fauto, mean)),
           colx="gray", coly="red",
           ylab="patients")
with(CP.fauto, lines(exp(lower[,1]), col="darkblue")) # 80%信頼区間
with(CP.fauto, lines(exp(upper[,1]), col="darkblue")) 

## StructTS による方法
(CP.sts <- StructTS(CP.log))
plot(fitted(CP.sts))
plot(forecast(CP.sts, h=50), xaxt="n")
lines(CP.log)
axis(side=1, 
     at=CP.date, labels=CP.date, las=2, 
     cex.axis=0.7)
## 対数変換
## StructTSの返値は少し整理が必要なので注意 (別の書き方の例)
CP.fsts <- forecast(CP.sts, h=50)
plot(c(exp(CP.log), # zooクラスを連結する
       zoo(exp(with(CP.fsts, upper[,1])), 
           order.by=as.Date(with(CP.fsts, index(mean))))),
     col="white", ylab="patients", xlab="", xaxt="n")
axis(side=1, 
     at=CP.date, labels=CP.date, las=2, 
     cex.axis=0.7)
lines(exp(CP.log), col="gray")
with(CP.fsts, lines(exp(mean), col="blue"))
with(CP.fsts, lines(ts(exp(lower[,1]),
                       start=with(CP.fsts, start(mean))),
                    col="darkblue")) # 80%信頼区間
with(CP.fsts, lines(ts(exp(upper[,1]),
                       start=with(CP.fsts, start(mean))),
                    col="darkblue")) # 80%信頼区間
