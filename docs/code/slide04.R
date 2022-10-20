### 
### 第4講 サンプルコード
###

## データの読み込み
tw_data <- read.csv("data/tokyo_weather.csv")
tw_subset <- subset(tw_data, # 8月のデータの抽出
                    subset= month==8)
head(tw_subset[,-1], 14) # 2週間分を表示

### モデル式
tw_model1 <- temp ~ press
tw_model2 <- temp ~ solar
tw_model3 <- temp ~ press + solar
tw_model4 <- temp ~ press + solar + humid
tw_model5 <- temp ~ press + solar + cloud

## 推定
tw_est1 <- lm(tw_model1, data=tw_subset, y=TRUE)
tw_est2 <- lm(tw_model2, data=tw_subset, y=TRUE)
tw_est3 <- lm(tw_model3, data=tw_subset, y=TRUE)
tw_est4 <- lm(tw_model4, data=tw_subset, y=TRUE)
tw_est5 <- lm(tw_model5, data=tw_subset, y=TRUE)

## 関連データの散布図
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
tw_df <- subset(tw_subset,
                select=c(temp,press,solar,humid,cloud))
plot(tw_df, labels=c("気温","気圧","日射","湿度","雲量"),
     col="blue")   # 散布図

## モデル1の推定結果
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(temp ~ press, data=tw_subset,
     xlab="気圧", ylab="気温",
     col="brown", pch=20)
abline(tw_est1, col="blue", lwd=3)

## モデル1の推定結果
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(temp ~ solar, data=tw_subset,
     xlab="日射", ylab="気温",
     col="brown", pch=20)
abline(tw_est2, col="blue", lwd=3)

## モデル3の推定結果
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
library(scatterplot3d) # パッケージの読み込み
s3d <- scatterplot3d( 
    tw_df[c("press","solar","temp")], # x,y,z の順
    type="p", # plotの種類: "p"点，"l"線，"h"足付き
    pch=16,# 点の種類 (?points 参照)
    angle=45, # xy平面の見る方向 (適宜調整)
    zlim=c(20,35),
    color="brown",
    xlab="気圧", ylab="日射", zlab="気温",
    ##    highlight.3d=TRUE # 高さ(z)ごとに色を変える
    )
s3d$plane3d(tw_est3, col="blue", # 回帰式の定める平面の追加
            draw_polygon=TRUE, # 平面の塗り潰しの設定
            polygon_args=list(col=rgb(0,0,1,0.1)))

## 観測値とあてはめ値の比較
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
with(tw_est1,
     plot(y,fitted.values,col="orange",pch=17, # 三角
          xlab="気温",
          ylab="あてはめ値",ylim=range(y)))   
abline(0,1,col="red",lwd=2)
with(tw_est2,
     points(y,fitted.values,col="green",pch=15)) # 四角
with(tw_est3,
     points(y,fitted.values,col="blue",pch=21))  # 丸
with(tw_est4,
     points(y,fitted.values,col="brown",pch=23))  # 菱形
with(tw_est5,
     points(y,fitted.values,col="cyan",pch=25))  # 三角
legend("bottomright",inset=.05, # 凡例の作成
       col=c("orange","green","blue","brown","cyan"), pch=c(17,15,21,23,25), 
       legend=c("モデル1","モデル2","モデル3","モデル4","モデル5"))

## モデル1
print(
    paste("R2:",
          signif(summary(tw_est1)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(tw_est1)$adj.r.squared,digits=3)))

## モデル2
print(
    paste("R2:",
          signif(summary(tw_est2)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(tw_est2)$adj.r.squared,digits=3)))

## モデル3
print(
    paste("R2:",
          signif(summary(tw_est3)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(tw_est3)$adj.r.squared,digits=3)))

## モデル4
print(
    paste("R2:",
          signif(summary(tw_est4)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(tw_est4)$adj.r.squared,digits=3)))

## モデル5
print(
    paste("R2:",
          signif(summary(tw_est5)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(tw_est5)$adj.r.squared,digits=3)))

### 人工データによる推定量の性質の確認
set.seed(987) # 乱数のシード
x_obs <- c(1, 3, 5, 7) # 説明変数の観測値
epsilon <- rnorm(length(x_obs),sd=0.5) # 誤差項の生成
y_obs <- 2 - 3*x_obs + epsilon # 目的変数の観測値
my_data <- data.frame(x=x_obs,y=y_obs) # データフレームの作成
beta_est <- lm(y ~ x, data=my_data) # 回帰係数の推定
coef(beta_est) # 回帰係数の取得
summary(beta_est) # 分析結果の概要の表示

### 
### 練習問題 推定量の性質
### 

### 人工データによる確認
set.seed(2468) # 乱数のシード (適宜変更せよ)

## 試行の設定
x_obs <- c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4) # 説明変数の観測値
beta0 <- -1 # 切片
beta1 <-  2 # xの係数
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)
my_trial <- function(){ 
    epsilon <- rnorm(length(x_obs),sd=sigma) # 誤差項の生成
    y_obs <- beta0 + beta1*x_obs + epsilon # 目的変数の観測値
    dat <- data.frame(x=x_obs,y=y_obs) # データフレームの作成
    est <- lm(y ~ x, data=dat) # 回帰係数の推定
    return(coef(est)) # 推定された係数だけ返す
}

## 数値実験 (少数で確認してみる)
mc <- 5 # 実験回数
replicate(mc, my_trial())

## 数値実験
mc <- 5000 # 実験回数
my_data <- as.data.frame(t( # 得られる結果を転置してデータフレームにしておく
    replicate(mc, my_trial()))) # mc回試行を行う
names(my_data) <- c("beta0_est","beta1_est") # 列名を変更

## 回帰係数の分布(2次元)
plot(beta1_est ~ beta0_est, data=my_data,
     col="blue", pch=20) # 推定値の散布図
abline(v=beta0, col="orchid")  # beta0の真値 (垂直線)
abline(h=beta1, col="orchid")  # beta1の真値 (水平線)

## 各回帰係数の分布
X <- cbind(1,x_obs) # デザイン行列
beta_cov <- sigma^2*solve(crossprod(X)) # 推定量の共分散行列
beta_mean <- c(beta0,beta1)
## beta0 (k=1), beta1 (k=2)
for(k in 1:2){ # 同じ処理であればfor文などの利用を推奨
    hist(my_data[[k]], # 実験により得られた分布
         breaks=30, freq=FALSE, # 密度で表示
         border="blue", col="lightblue",
         xlab=names(my_data)[k], main="histogram of estimates")
    abline(v=beta_mean[k], col="orchid", lwd=2) # 真の値
    curve(dnorm(x,mean=beta_mean[k],sd=sqrt(beta_cov[k,k])),
          col="orchid", lwd=2, add=TRUE) # 理論分布
}

### 
### 練習問題 標準誤差の性質
###

### 人工データによる標準誤差と真の誤差の比較
set.seed(1313) # 乱数のシード (適宜変更せよ)

## 試行の設定 (重回帰，以下適宜変更せよ)
x_obs1 <- c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4) # 説明変数1
x_obs2 <- c(3, 19, 1, 4, 18, 7, 2, 10, 6, 12) # 説明変数2
beta0 <- -1 # 切片 
beta1 <-  2 # xの係数
beta2 <- -3 # xの係数
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)
my_trial <- function(){ 
    epsilon <- rnorm(length(x_obs1),sd=sigma) # 誤差項
    y_obs <- beta0 + beta1*x_obs1 + beta2*x_obs2 + epsilon # 目的変数
    dat <- data.frame(x1=x_obs1,x2=x_obs2,y=y_obs) # データフレームの作成
    est <- lm(y ~ x1 + x2, data=dat) # 回帰係数の推定
    return(summary(est)$coef[,"Std. Error"]) # 標準誤差を返す
}

## 数値実験
mc <- 5000 # 実験回数
my_data <- as.data.frame(t( # データフレームの作成
    replicate(mc, my_trial()))) # mc回の試行
names(my_data) <- c("beta0.se","beta1.se","beta2.se") 

## 各回帰係数の標準誤差の分布
X <- cbind(1,x_obs1,x_obs2) # デザイン行列
beta_cov <- sigma^2*solve(crossprod(X)) # 推定量の共分散行列
beta_mean <- c(beta0,beta1,beta2)
## beta0 (k=1), beta1 (k=2), beta2 (k=3)
for(k in 1:3){
    hist(my_data[[k]], # 実験により得られた分布
         breaks=30, freq=FALSE, # 密度で表示
         border="blue", col="lightblue",
         xlab=names(my_data)[k], main="std. errors")
    abline(v=sqrt(beta_cov[k,k]), col="orchid", lwd=2) # 真の値
}

### 広告費と売上データによる分析

## データの読み込み
adv_data <- read.csv('https://www.statlearning.com/s/Advertising.csv',
                     row.names=1) # 1列目を行名として読み込む

## モデルの推定
adv_est1 <- lm(sales ~ TV, data=adv_data)
adv_est2 <- lm(sales ~ radio, data=adv_data)
adv_est3 <- lm(sales ~ TV + radio, data=adv_data)

## 推定値とその標準誤差
summary(adv_est1)$coef[,1:2] 
summary(adv_est2)$coef[,1:2] 
summary(adv_est3)$coef[,1:2] 

### 東京の気候データによる分析

## データの整理 (8月のデータの抽出)
tw_subset <- subset(read.csv("data/tokyo_weather.csv"),
                      subset= month==8)

## 回帰モデルの設定
tw_model1 <- temp ~ solar
tw_model2 <- temp ~ solar + press
tw_model3 <- temp ~ solar + press + cloud

## 回帰モデルの推定
tw_est1 <- lm(tw_model1, data=tw_subset, y=TRUE)
tw_est2 <- lm(tw_model2, data=tw_subset, y=TRUE)
tw_est3 <- lm(tw_model3, data=tw_subset, y=TRUE)

## モデルの推定値とその標準誤差は以下のとおり
summary(tw_est1)$coef[,c("Estimate","Std. Error")]
summary(tw_est2)$coef[,1:2] # 名前ではなく列番号で指定する場合
summary(tw_est3)$coef[,1:2] # cloud の標準誤差が大きく精度が悪いことが示唆される

### 
### 練習問題 F統計量の性質
### 

### 人工データによる確認
set.seed(2525) # 乱数のシード (適宜変更せよ)

## 試行の設定 (重回帰，以下適宜変更せよ)
x_obs1 <- c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4) # 説明変数1
x_obs2 <- c(3, 19, 1, 4, 18, 7, 2, 10, 6, 12) # 説明変数2
beta0 <- -1 # 切片 
beta1 <-  0 # x1の係数 
beta2 <-  0 # x2の係数 < 係数のどちらも0なので帰無仮説が成り立つ
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)
my_trial <- function(){ 
    epsilon <- rnorm(length(x_obs1),sd=sigma) # 誤差項
    y_obs <- beta0 + beta1*x_obs1 + beta2*x_obs2 + epsilon # 目的変数
    dat <- data.frame(x1=x_obs1,x2=x_obs2,y=y_obs) # データフレームの作成
    est <- lm(y ~ x1 + x2, data=dat) # 回帰係数の推定
    return(summary(est)$fstat[1]) # F統計量を返す
}

## 数値実験 (帰無仮説が成り立つ場合)
mc <- 5000 # 実験回数
my_data <- data.frame( # 1次元の場合はそのままデータフレームが作成できる
    fstat=replicate(mc, my_trial()))

## モデルのF統計量の分布
n <- length(x_obs1) # データ数
p <- 2 # 説明変数の次元
hist(my_data[[1]], # 実験により得られたF統計量の分布
     breaks=30, freq=FALSE, # 密度で表示
     border="blue", col="lightblue",
     xlab="F statistic", main="null hypothesis is true")
curve(df(x,df1=p,df2=n-p-1), # 自由度 p, n-p-1 のF-分布
      col="orchid", lwd=2, add=TRUE)

## 数値実験 (帰無仮説が成り立たない場合)
beta1 <-  2 # x1の係数 < 帰無仮説が成り立たない
my_data <- data.frame(
    fstat=replicate(mc, my_trial()))

## モデルのF統計量の分布は帰無分布に従わない
hist(my_data[[1]], # 実験により得られたF統計量の分布
     breaks=30, freq=FALSE, # 密度で表示
     border="blue", col="lightblue",
     xlab="F statistic", main="null hypothesis is false")
curve(df(x,df1=p,df2=n-p-1), # 自由度 p, n-p-1 のF-分布
      col="orchid", lwd=2, add=TRUE)

### 広告費と売上データによる分析の例

## 説明変数1つのモデルを検討する
summary(lm(sales ~ TV, data=adv_data)) 
summary(lm(sales ~ radio, data=adv_data)) 
summary(lm(sales ~ newspaper, data=adv_data))

## radio, newspaper は決定係数は小さく説明力は無いが，
## F-stat はそれなりに小さいのでモデルの有効性は無いとは言えない

### 東京の気候データによる分析の例

## press, solar, rain によるモデルを検討する
summary(lm(temp ~ press, data=tw_subset))
summary(lm(temp ~ press + solar, data=tw_subset))
summary(lm(temp ~ press + solar + rain, data=tw_subset))

## press のみではモデルの有効性があるとは言えないが
## solar と組み合わせることにより有効性が確認できる
## rain を加えても press の係数に変化は見られないが
## solar の係数が変化し決定係数が大きくなること(自由度調整済みは若干下がるが)から
## solar と rain が相補的にモデルの精度を上げている可能性が示唆される

### 関数 lm() による推定結果の診断プロットの使い方
est <- lm(temp ~ press + solar + rain, data=tw_subset)
plot(est) # 指示に従って <Return> キーを押すと順次表示される
## help(plot.lm) を参照
