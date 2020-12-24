### 第13回 練習問題解答例

### 練習12.3
### 自己相関

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

Tmax <- 200 # 時系列の長さ t=1,..,Tmax
K <- 4 # 表示する時系列の数 (4つを並べて比較する)
library(RColorBrewer)
myCol <- brewer.pal(K,"Dark2") 
df.ar <- ts(replicate(K, myARMA(a=c(0.67, 0.26), b=c(0),
                                epsilon=rnorm(Tmax))))
df.ma <- ts(replicate(K, myARMA(a=c(0), b=c(0.44, 0.08),
                                epsilon=rnorm(Tmax))))
df.arma <- ts(replicate(K, myARMA(a=c(0.8, -0.64), b=c(-0.5),
                                  epsilon=rnorm(Tmax))))

### AR(2)モデルの自己相関
orgpar <- par(mfrow=c(2,2)) # グラフを2x2(行方向の順)に並べる
for(i in 1:K) {
  acf(df.ar[,i], col=myCol[i], main=paste("AR series",i))
}
par(orgpar) # もとのparの内容に戻す

### MA(2)モデルの自己相関
orgpar <- par(mfrow=c(2,2))
for(i in 1:K) {
  acf(df.ma[,i], col=myCol[i], main=paste("MA series",i))
}
par(orgpar) # もとのparの内容に戻す

### ARMA(2,1)モデルの自己相関
orgpar <- par(mfrow=c(2,2))
for(i in 1:K) {
  acf(df.arma[,i], col=myCol[i], main=paste("ARMA series",i))
}
par(orgpar) # もとのparの内容に戻す

### 練習1.1
### ARMAモデルの推定

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

### 練習1.2
### 東京の気温データの時系列モデル

## パッケージの読み込み
library(forecast) # 既に読み込んでいれば不要 
TW.data <- read.csv("data/tokyo_weather_reg.csv")
TW.zoo <- with(TW.data,
	       zoo(temp,
		   order.by = as.Date(date)))

## データの視覚化を行う
plot(TW.zoo, col="red",
     xlab="month", ylab="degree", main="Temperature in Tokyo")
plot(window(TW.zoo, # 一部を切り出して視覚化する
	    start=as.Date("2019-06-01"),
	    end=as.Date("2019-07-31")),
     col="red",
     xlab="date", ylab="degree", main="Temperature (June-July)")
acf(TW.zoo)       # 減衰が遅いので差分をとった方が良さそう
plot(diff(TW.zoo)) # 階差系列の視覚化
acf(diff(TW.zoo))  # 階差系列の自己相関

## 階差系列にARMAモデルをあてはめる (d=1)
TW.fit <- auto.arima(TW.zoo, d=1, D=0)
summary(TW.fit) # 推定されたモデルの仕様を表示
acf(resid(TW.fit)) # そこそこあてはまりは良さそう

### 練習2
### 東京の気温の予測

## パッケージの読み込み (既に読み込んでいれば不要)
library(forecast) 

## データの読み込み (既に行っていれば不要)
TW.data <- read.csv("data/tokyo_weather_reg.csv")
TW.zoo <- with(TW.data,
	       zoo(temp,
		   order.by = as.Date(date)))
## データの整理
TW.train <- window(TW.zoo, # 6月までのデータ (訓練データ)
		   end="2019-06-30")  
TW.test  <- window(TW.zoo, # 7月のデータ (試験データ)
		   start="2019-07-01", end="2019-07-31") 

## auto.arima による推定
(TW.auto <- auto.arima(TW.train, d=1, D=0)) 
(TW.fcst <- forecast(TW.auto, h=length(TW.test)))

## 視覚化
plot(TW.fcst) # X軸が無粋 (1970-01-01からの日数)

## X軸の書き直し
plot(TW.fcst, xaxt="n",
     xlim=c(as.Date("2019-06-01"), as.Date("2019-07-31")))
axis(side=1, # x軸を指定
     at=index(TW.zoo), # 文字を書く座標軸上の位置
     labels=index(TW.zoo), # ラベル
     las=2, # 垂直に表示
     cex.axis=0.7) # 文字の大きさを調整
lines(TW.test, col="red") # 真値を重ね描き

## 別の書き方
plot(window(TW.zoo, start="2019-06-01", end="2019-07-31"),
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
     xlim=c(as.Date("2019-06-01"), as.Date("2019-07-31")))
axis(side=1, # x軸を指定
     at=index(TW.zoo), # 文字を書く座標軸上の位置
     labels=index(TW.zoo), # ラベル
     las=2, # 垂直に表示
     cex.axis=0.7) # 文字の大きさを調整
lines(TW.test, col="red") # 真値を重ね描き
