### 時系列分析の事例
library(forecast)

## AirPassengersデータの読み込み
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

### 厚生労働省のCOVID-19の感染者数データ
## データの取得と整理 
myData <- read.csv("https://www.mhlw.go.jp/content/pcr_positive_daily.csv")
names(myData) <- c("date","patients")
myData$date <- as.Date(myData$date)
head(myData)

## 時系列データ(zooクラス)への変更
CP.zoo <- with(myData,zoo(x=patients, order.by=date))
plot(CP.zoo, col="blue")

## 対象を限定する
CP.sub <- (window(CP.zoo, start="2020-09-01"))

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
CP.date <- seq(from=start(CP.zoo), to=as.Date("2021-03-31"), by=1)
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
