### 第05回 練習問題解答例

### 練習4
### F-統計量の性質

### 人工データによる確認
set.seed(2525) # 乱数のシード (適宜変更せよ)

## 試行の設定 (重回帰，以下適宜変更せよ)
xobs1 <- c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4) # 説明変数1
xobs2 <- c(3, 19, 1, 4, 18, 7, 2, 10, 6, 12) # 説明変数2
beta0 <- -1 # 切片 
beta1 <-  0 # x1の係数 
beta2 <-  0 # x2の係数 < 係数のどちらも0なので帰無仮説が成り立つ
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)
myTrial <- function(){ 
    epsilon <- rnorm(length(xobs1),sd=sigma) # 誤差項
    yobs <- beta0 + beta1*xobs1 + beta2*xobs2 + epsilon # 目的変数
    dat <- data.frame(x1=xobs1,x2=xobs2,y=yobs) # データフレームの作成
    est <- lm(y ~ x1 + x2, data=dat) # 回帰係数の推定
    return(summary(est)$fstat[1]) # F-統計量を返す
}

## 数値実験 (帰無仮説が成り立つ場合)
mc <- 5000 # 実験回数
myData <- data.frame( # 1次元の場合はそのままデータフレームが作成できる
    fstat=replicate(mc, myTrial()))

## モデルのF-統計量の分布
n <- length(xobs1) # データ数
p <- 2 # 説明変数の次元
hist(myData[[1]], # 実験により得られたF-統計量の分布
     breaks=30, freq=FALSE, # 密度で表示
     border="blue", col="lightblue",
     xlab="F statistic", main="null hypothesis is true")
curve(df(x,df1=p,df2=n-p-1), # 自由度 p, n-p-1 のF-分布
      col="orchid", lwd=2, add=TRUE)

## 数値実験 (帰無仮説が成り立たない場合)
beta1 <-  2 # x1の係数 < 帰無仮説が成り立たない
myData <- data.frame(
    fstat=replicate(mc, myTrial()))

## モデルのF-統計量の分布は帰無分布に従わない
hist(myData[[1]], # 実験により得られたF-統計量の分布
     breaks=30, freq=FALSE, # 密度で表示
     border="blue", col="lightblue",
     xlab="F statistic", main="null hypothesis is false")
curve(df(x,df1=p,df2=n-p-1), # 自由度 p, n-p-1 のF-分布
      col="orchid", lwd=2, add=TRUE)

### 広告費と売上データによる分析の例
## データの読み込み
Adv.data <- read.csv("data/Advertising.csv",
                     row.names=1) # 1列目を行名として読み込む
## 説明変数1つのモデルを検討する
summary(lm(sales ~ TV, data=Adv.data)) 
summary(lm(sales ~ radio, data=Adv.data)) 
summary(lm(sales ~ newspaper, data=Adv.data))
## radio, newspaper は決定係数は小さく説明力は無いが，
## F-stat はそれなりに小さいのでモデルの有効性は無いとは言えない

### 東京の気候データによる分析の例
## データの整理 (8月のデータの抽出)
TW.subset <- subset(read.csv("data/tokyo_weather_reg.csv"), 
                    subset= months(as.Date(date),
                                   abbreviate=TRUE)==" 8")
## press, solar, rain によるモデルを検討する
summary(lm(temp ~ press, data=TW.subset))
summary(lm(temp ~ press + solar, data=TW.subset))
summary(lm(temp ~ press + solar + rain, data=TW.subset))
## press のみではモデルの有効性があるとは言えないが
## solar と組み合わせることにより有効性が確認できる
## rain を加えても press の係数に変化は見られないが
## solar の係数が変化し決定係数が大きくなることから
## solar と rain が相補的にモデルの精度を上げていることが示唆される

### 9,10月のデータでモデルを構築し，8,11月のデータを予測
TW.data <- transform(read.csv("data/tokyo_weather_reg.csv"),
                     month=as.numeric(months(as.Date(date), # 月(数値)を付加
                                             abbreviate=TRUE)))
TW.model <- temp ~ solar + press # モデルの定義 
TW.train <- subset(TW.data, # モデル推定用データ
                   subset= month %in% c(9,10))
TW.test  <- subset(TW.data, # 予測用データ
                   subset= month %in% c(8,11))
TW.est <- lm(TW.model, data=TW.train) # モデルの推定
summary(TW.est) # モデルの評価
## 予測値の計算
TW.fit  <- predict(TW.est) # データのあてはめ値
TW.pred <- predict(TW.est, # 新規データの予測値
                   newdata=TW.test)

## 予測結果を図示
myColor <- rep("black",12) 
myColor[8:11] <- c("red","orange","violet","blue") # 色の定義
with(TW.train,
     plot(temp ~ TW.fit, pch=1, col=myColor[month],
          xlab="fitted", ylab="observed"))
with(TW.test,
     points(temp ~ TW.pred, pch=4, col=myColor[month]))
abline(0,1,col="gray") # 予測が完全に正しい場合のガイド線
legend("bottomright",inset=.05, pch=15, # 凡例の作成
       legend=c("Aug","Sep","Oct","Nov"), col=myColor[8:11])

### 練習1
### 回帰式を用いた予測

### 東京の気候データによる分析
## 信頼区間と予測区間の計算
require(plotrix) # 区間付きのグラフを利用するため
TW.data <- transform(read.csv("data/tokyo_weather_reg.csv"),
		     date=as.Date(date), # 日付をx軸で使えるように変換
		     month=as.numeric(months(as.Date(date), 
					     abbreviate=TRUE)))
TW.model <- temp ~ solar + press + cloud # モデルの定義 
TW.train <- subset(TW.data, subset= month %in% 8) # 推定用データ
TW.test  <- subset(TW.data, subset= month %in% 9) # 予測用データ
TW.est <- lm(TW.model, data=TW.train) # モデルの推定
## 信頼区間
TW.fit <- data.frame(TW.train,
                     predict(TW.est, # 回帰式によるあてはめ値を付加
                             interval="confidence")) 
TW.cint <- data.frame(TW.test,
                      predict(TW.est, newdata=TW.test,
                              interval="confidence"))
## 予測区間
TW.pint <- data.frame(TW.test,
                      predict(TW.est, newdata=TW.test,
                              interval="prediction"))

## 8月のデータで8月をあてはめた信頼区間
with(TW.fit, { # 2つのプロットをまとめて実行
    plotCI(date, fit, ui=upr, li=lwr, # それぞれの列名に注意
           col="blue", scol="lightblue",
           xlab="August", ylab="temperature")
    points(date, temp, col="red", pch=16)
})

## 8月のデータで9月をあてはめた信頼区間
with(TW.cint, {
    plotCI(date, fit, ui=upr, li=lwr, ylim=c(20,32), 
           col="blue", scol="lightblue", 
           xlab="September", ylab="temperature")
    points(date, temp, col="red", pch=16)
})

## 8月のデータで9月をあてはめた予測区間
with(TW.pint, {
    plotCI(date, fit, ui=upr, li=lwr, ylim=c(20,32),
           col="blue", scol="lightblue",
           xlab="September", ylab="temperature")
    points(date, temp, col="red", pch=16)
})

### 人工データによる検討例
###   以下はあくまで例です
###   自由に数値実験を設計して下さい

## 試行の設定
## モデル: y = -1 + 2*x1
## 人工データの生成
set.seed(1313) # 乱数のシード
n <- 50 # データ数の設定
xobs1 <- runif(n) # 説明変数1
xobs2 <- runif(n) # 説明変数2
beta0 <- -1 # 切片 
beta1 <-  2 # xの係数
beta2 <-  0 # xの係数
sigma <-  1/2 # 誤差の標準偏差
epsilon <- rnorm(length(xobs1),sd=sigma) # 誤差項
yobs <- beta0 + beta1*xobs1 + beta2*xobs2 + epsilon # 目的変数
myTrain <- data.frame(x1=xobs1,
		     x2=xobs2,
		     y=yobs) # データフレームの作成
est1 <- lm(y ~ x1, data=myTrain) # x1による回帰分析の実行(正しいモデル)
summary(est1)
est2 <- lm(y ~ x1 + x2, data=myTrain) # x1とx2による回帰分析の実行(冗長なモデル)
summary(est2)
est3 <- lm(y ~ x2, data=myTrain) # x2による回帰分析の実行(誤ったモデル)
summary(est3)

## 新規データに対する予測
myTest <- data.frame(x1=runif(n),
		      x2=runif(n,-10,10)) # 説明変数の新規データ
ynew <- beta0 + beta1*myTest$x1 # 新規データに対する目的変数の真値 (誤差なし)
yhat1 <- predict(est1, newdata=myTest) # est1による予測値
yhat2 <- predict(est2, newdata=myTest) # est2による予測値
yhat3 <- predict(est3, newdata=myTest) # est3による予測値

## 散布図による可視化
plot(ynew ~ yhat1, 
     col="red", pch=20,
     xlab="fitted value", ylab="observed value") # 黒
points(ynew ~ yhat2, pch=20, col="green")  # 赤
points(ynew ~ yhat3, pch=20, col="blue") # 青
abline(0,1, col="gray") # 理想的な結果
legend("bottomright",inset=.05, # 凡例の作成
       col=c("red","green","blue"), pch=c(20,20,20), 
       legend=c("model1","model2","model3"))

## 相関係数による数値的な評価 (R-squared と等価)
cor(ynew, yhat1)^2 
cor(ynew, yhat2)^2
cor(ynew, yhat3)^2

### 練習2
### 交互作用と非線形を含む回帰

### 東京の気候データによる分析
## データの整形
TW.subset <- subset(TW.data, subset= month %in% 9:11)

## 日射量，気圧，湿度の線形回帰モデル
summary(lm(temp ~ solar + press + humid, data=TW.subset))
## 湿度の対数を考えた線形回帰モデル
summary(lm(temp ~ solar + press + log(humid), data=TW.subset))
## 最初のモデルにそれぞれの交互作用を加えたモデル (書き方はいろいろある)
summary(lm(temp ~ (solar + press + humid)^2, data=TW.subset))
## 更に3つの変数の積を加えたモデル
summary(lm(temp ~ solar * press * humid, data=TW.subset))

## 用いた変数の散布図
plot(~ temp + solar + press + humid, data=TW.subset)
## 最後のモデルの視覚的な評価 (診断プロット)
plot(lm(temp ~ solar * press * humid, data=TW.subset))

## factor属性の与え方
X <- c("A","S","A","B","D")
Y <- c(85,100,80,70,30)
dat1 <- data.frame(X,Y)
dat2 <- transform(dat1, 
                  X2=factor(X))
str(dat2) # 作成したデータフレームの素性を見る
dat3 <- transform(dat2, 
                  X3=factor(X, levels=c("S","A","B","C","D")))
str(dat3) # dat2とはfactorの順序が異なる
dat4 <- transform(dat2,
                  Y2=factor(Y > 60)) 
str(dat4) # 条件の真偽で2値に類別される

### 練習3
### カテゴリカル変数

### 東京の気候データによる分析
## 雨と気温の関係を分析
TW.data <- transform(TW.data,
                     rain=factor(rain > 0)) 
summary(lm(temp ~ rain, data=TW.data))
## 通年では雨と気温の関係は積極的に支持されない

## 月毎の気温の差を考慮して月を表す変数をダミー化する
TW.data <- transform(TW.data,
                     month=factor(month))
summary(lm(temp ~ rain + month, data=TW.data))
## 月毎に比較すると雨の日の方が気温が低いことが支持される
