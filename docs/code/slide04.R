### 第04回 練習問題解答例

### 前回の宿題
### 決定係数によるモデルの比較

### 東京の気候データによる分析
## データの整理 (8月のデータの抽出)
TW.subset <- subset(read.csv("data/tokyo_weather_reg.csv"), 
                    subset= months(as.Date(date),
                                   abbreviate=TRUE)==" 8")
## 回帰モデルの設定
TW.model1 <- temp ~ solar
TW.model2 <- temp ~ solar + press
TW.model3 <- temp ~ solar + press + cloud
## 回帰モデルの推定
TW.est1 <- lm(TW.model1, data=TW.subset, y=TRUE)
TW.est2 <- lm(TW.model2, data=TW.subset, y=TRUE)
TW.est3 <- lm(TW.model3, data=TW.subset, y=TRUE)
## 分析結果の表示
summary(TW.est1) # 分析結果の重要な指標を表示 
summary(TW.est1)$adj.r.squared # 自由度調整済み決定係数を抽出
summary(TW.est2)$adj.r.squared # (model1より上昇)
summary(TW.est3)$adj.r.squared # (model2より上昇)
## 予測値と実測値の比較 (3のモデルで比較するための図示の例)
with(TW.est1, # lm(...,y=TRUE) を利用して y を抽出
     plot(y,fitted.values,col="orange",pch=17, # 三角
          xlab="temperature",
          ylab="fitted values", ylim=range(y))) # y軸を調整  
abline(0,1,col="red",lwd=2) # 予測値と実測値の理想的な関係
with(TW.est2,
     points(y,fitted.values,col="green",pch=15)) # 四角
with(TW.est3,
     points(y,fitted.values,col="blue",pch=16))  # 丸
legend("bottomright",inset=.05, # 凡例の作成
       col=c("orange","green","blue"), pch=c(17,15,16), 
       legend=c("model1","model2","model3"))

### 人工データによる推定量の性質の確認
set.seed(987) # 乱数のシード
xobs <- c(1, 3, 5, 7) # 説明変数の観測値
epsilon <- rnorm(length(xobs),sd=0.5) # 誤差項の生成
yobs <- 2 - 3*xobs + epsilon # 目的変数の観測値
myData <- data.frame(x=xobs,y=yobs) # データフレームの作成
est <- lm(y ~ x, data=myData) # 回帰係数の推定
coef(est) # 回帰係数の取得
summary(est) # 分析結果の概要の表示

### 練習1
### 推定量の性質

### 人工データによる確認
set.seed(2468) # 乱数のシード (適宜変更せよ)

## 試行の設定
xobs <- c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4) # 説明変数の観測値
beta0 <- -1 # 切片
beta1 <-  2 # xの係数
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)
myTrial <- function(){ 
    epsilon <- rnorm(length(xobs),sd=sigma) # 誤差項の生成
    yobs <- beta0 + beta1*xobs + epsilon # 目的変数の観測値
    dat <- data.frame(x=xobs,y=yobs) # データフレームの作成
    est <- lm(y ~ x, data=dat) # 回帰係数の推定
    return(coef(est)) # 推定された係数だけ返す
}

## 数値実験 (少数で確認してみる)
mc <- 5 # 実験回数
replicate(mc, myTrial())

## 数値実験
mc <- 5000 # 実験回数
myData <- as.data.frame(t( # 得られる結果を転置してデータフレームにしておく
    replicate(mc, myTrial()))) # mc回試行を行う
names(myData) <- c("beta0.est","beta1.est") # 列名を変更

## 回帰係数の分布(2次元)
plot(beta1.est ~ beta0.est, data=myData,
     col="blue", pch=20) # 推定値の散布図
abline(v=beta0, col="orchid")  # beta0の真値 (垂直線)
abline(h=beta1, col="orchid")  # beta1の真値 (水平線)

## 各回帰係数の分布
X <- cbind(1,xobs) # デザイン行列
bcov <- sigma^2*solve(crossprod(X)) # 推定量の共分散行列
bmean <- c(beta0,beta1)
## beta0 (k=1), beta1 (k=2)
for(k in 1:2){ # 同じ処理であればfor文などの利用を推奨
    hist(myData[[k]], # 実験により得られた分布
         breaks=30, freq=FALSE, # 密度で表示
         border="blue", col="lightblue",
         xlab=names(myData)[k], main="histogram of estimates")
    abline(v=bmean[k], col="orchid", lwd=2) # 真の値
    curve(dnorm(x,mean=bmean[k],sd=sqrt(bcov[k,k])),
          col="orchid", lwd=2, add=TRUE) # 理論分布
}

### 練習2
### 標準誤差の性質

### 人工データによる標準誤差と真の誤差の比較
set.seed(1313) # 乱数のシード (適宜変更せよ)

## 試行の設定 (重回帰，以下適宜変更せよ)
xobs1 <- c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4) # 説明変数1
xobs2 <- c(3, 19, 1, 4, 18, 7, 2, 10, 6, 12) # 説明変数2
beta0 <- -1 # 切片 
beta1 <-  2 # xの係数
beta2 <- -3 # xの係数
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)
myTrial <- function(){ 
    epsilon <- rnorm(length(xobs1),sd=sigma) # 誤差項
    yobs <- beta0 + beta1*xobs1 + beta2*xobs2 + epsilon # 目的変数
    dat <- data.frame(x1=xobs1,x2=xobs2,y=yobs) # データフレームの作成
    est <- lm(y ~ x1 + x2, data=dat) # 回帰係数の推定
    return(summary(est)$coef[,"Std. Error"]) # 標準誤差を返す
}

## 数値実験
mc <- 5000 # 実験回数
myData <- as.data.frame(t( # データフレームの作成
    replicate(mc, myTrial()))) # mc回の試行
names(myData) <- c("beta0.se","beta1.se","beta2.se") 

## 各回帰係数の標準誤差の分布
X <- cbind(1,xobs1,xobs2) # デザイン行列
bcov <- sigma^2*solve(crossprod(X)) # 推定量の共分散行列
bmean <- c(beta0,beta1,beta2)
## beta0 (k=1), beta1 (k=2), beta2 (k=3)
for(k in 1:3){
    hist(myData[[k]], # 実験により得られた分布
         breaks=30, freq=FALSE, # 密度で表示
         border="blue", col="lightblue",
         xlab=names(myData)[k], main="std. errors")
    abline(v=sqrt(bcov[k,k]), col="orchid", lwd=2) # 真の値
}

### 広告費と売上データによる分析
## データの読み込み
Adv.data <- read.csv("data/Advertising.csv",
                     row.names=1) # 1列目を行名として読み込む
## モデルの推定
Adv.est1 <- lm(sales ~ TV, data=Adv.data)
Adv.est2 <- lm(sales ~ radio, data=Adv.data)
Adv.est3 <- lm(sales ~ TV + radio, data=Adv.data)
## 推定値とその標準誤差
summary(Adv.est1)$coef[,1:2] 
summary(Adv.est2)$coef[,1:2] 
summary(Adv.est3)$coef[,1:2] 

### 東京の気候データによる分析
## 宿題のモデルの推定値とその標準誤差は以下のとおりである
summary(TW.est1)$coef[,c("Estimate","Std. Error")]
summary(TW.est2)$coef[,1:2] # 名前ではなく列番号で指定する場合
summary(TW.est3)$coef[,1:2] # cloud の標準誤差が大きく精度が悪いことが示唆される

### 練習3
### t-統計量の性質

### 人工データによる確認
set.seed(2525) # 乱数のシード (適宜変更せよ)

## 試行の設定 (重回帰，以下適宜変更せよ)
xobs1 <- c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4) # 説明変数1
xobs2 <- c(3, 19, 1, 4, 18, 7, 2, 10, 6, 12) # 説明変数2
beta0 <- -1 # 切片 
beta1 <-  2 # x1の係数 < 帰無仮説に従わない
beta2 <-  0 # x2の係数 < 帰無仮説に従う 
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)
myTrial <- function(){ 
    epsilon <- rnorm(length(xobs1),sd=sigma) # 誤差項
    yobs <- beta0 + beta1*xobs1 + beta2*xobs2 + epsilon # 目的変数
    dat <- data.frame(x1=xobs1,x2=xobs2,y=yobs) # データフレームの作成
    est <- lm(y ~ x1 + x2, data=dat) # 回帰係数の推定
    return(summary(est)$coef[,"t value"]) # t-統計量を返す
}

## 数値実験
mc <- 5000 # 実験回数
myData <- as.data.frame(t( # データフレームの作成
    replicate(mc, myTrial()))) # mc回の試行
names(myData) <- c("beta0.tval","beta1.tval","beta2.tval") 

## 各回帰係数のt-統計量の分布
n <- length(xobs1) # データ数 n
p <- 2 # 説明変数の次元
## beta0 (k=1), beta1 (k=2), beta2 (k=3)
for(k in 1:3){
    hist(myData[[k]], # 実験により得られた分布
         breaks=30, freq=FALSE, # 密度で表示
         border="blue", col="lightblue",
         xlab=names(myData)[k], main="t values")
    curve(dt(x,df=n-p-1), # 自由度 n-p-1 のt分布
          col="orchid", lwd=2, add=TRUE)
}

### 広告費と売上データによる分析
## 全てを用いたモデルと newspaper を除いたモデルを比較する
summary(lm(sales ~ ., data=Adv.data)) # "." は全て
summary(lm(sales ~ . -newspaper, data=Adv.data)) # "-" は除外
## newspaperの係数のt-統計量から有意性は低いと考えられる
## 自由度調整済決定係数も除いた方が高くなることが確認できる

### 東京の気候データによる分析
## solarとpressを用いたモデルを比較する
summary(lm(temp ~ press, data=TW.subset))
summary(lm(temp ~ solar + press, data=TW.subset))
summary(lm(temp ~ solar, data=TW.subset))
## press単体では係数の推定精度も決定係数も低いが
## solarと組み合わせることにより精度が上がり説明力も高くなる
## また組み合わせた方が自由度調整済決定係数はsolar単体より大きくなる

### 練習
### 演習内容の説明
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
## 説明変数1つのモデルを検討する
summary(lm(sales ~ TV, data=Adv.data)) 
summary(lm(sales ~ radio, data=Adv.data)) 
summary(lm(sales ~ newspaper, data=Adv.data))
## radio, newspaper は決定係数は小さく説明力は無いが，
## F-stat はそれなりに小さいのでモデルの有効性は無いとは言えない

### 東京の気候データによる分析の例
## press, solar, rain によるモデルを検討する
summary(lm(temp ~ press, data=TW.subset))
summary(lm(temp ~ press + solar, data=TW.subset))
summary(lm(temp ~ press + solar + rain, data=TW.subset))
## press のみではモデルの有効性があるとは言えないが
## solar と組み合わせることにより有効性が確認できる
## rain を加えても press の係数に変化は見られないが
## solar の係数が変化し決定係数が大きくなることから
## solar と rain が相補的にモデルの精度を上げていることが示唆される
