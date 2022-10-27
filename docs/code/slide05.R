### 
### 第5講 サンプルコード
###

## データの読み込み
tw_data <- read.csv("data/tokyo_weather.csv")
tw_subset <- subset(tw_data, # 8月のデータの抽出
                    subset= month==8)
head(tw_subset[,-1], 14) # 2週間分を表示

### モデル式
tw_model0 <- temp ~ press + solar + humid + cloud
tw_model1 <- temp ~ press
tw_model2 <- temp ~ solar
tw_model3 <- temp ~ press + solar
tw_model4 <- temp ~ press + solar + humid
tw_model5 <- temp ~ press + solar + cloud

## 推定
tw_est0 <- lm(tw_model0, data=tw_subset, y=TRUE)
tw_est1 <- lm(tw_model1, data=tw_subset, y=TRUE)
tw_est2 <- lm(tw_model2, data=tw_subset, y=TRUE)
tw_est3 <- lm(tw_model3, data=tw_subset, y=TRUE)
tw_est4 <- lm(tw_model4, data=tw_subset, y=TRUE)
tw_est5 <- lm(tw_model5, data=tw_subset, y=TRUE)

## 説明変数と目的変数の散布図
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(model.frame(tw_est0),
     labels=c("気温","気圧","日射","湿度","雲量"),
     col="blue", pch=20)

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
          signif(summary(tw_est1)$adj.r.squared,digits=3)
          ))

## モデル1
print(
    paste("R2:",
          signif(summary(tw_est2)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(tw_est2)$adj.r.squared,digits=3)
          ))

## モデル3
print(
    paste("R2:",
          signif(summary(tw_est3)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(tw_est3)$adj.r.squared,digits=3)
          ))

## モデル4
print(
    paste("R2:",
          signif(summary(tw_est4)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(tw_est4)$adj.r.squared,digits=3)
          ))

## モデル4
print(
    paste("R2:",
          signif(summary(tw_est5)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(tw_est5)$adj.r.squared,digits=3)
          ))

## モデル1
tw_sum <- summary(tw_est1)
tw_r2 <- tw_sum$r.squared
tw_ar2 <- tw_sum$adj.r.squared
tw_fstat <- tw_sum$fstat
print(
    paste("R2:",
          signif(tw_r2,digits=3),
          "; adj. R2:",
          signif(tw_ar2,digits=3),
          "; F-stat:",
          signif(tw_fstat[1],digits=3),
          "; p-val:",
          signif(1-pf(tw_fstat[1],tw_fstat[2],tw_fstat[3]),digits=3)
          ))

## モデル2
tw_sum <- summary(tw_est2)
tw_r2 <- tw_sum$r.squared
tw_ar2 <- tw_sum$adj.r.squared
tw_fstat <- tw_sum$fstat
print(
    paste("R2:",
          signif(tw_r2,digits=3),
          "; adj. R2:",
          signif(tw_ar2,digits=3),
          "; F-stat:",
          signif(tw_fstat[1],digits=3),
          "; p-val:",
          signif(1-pf(tw_fstat[1],tw_fstat[2],tw_fstat[3]),digits=3)
          ))

## モデル3
tw_sum <- summary(tw_est3)
tw_r2 <- tw_sum$r.squared
tw_ar2 <- tw_sum$adj.r.squared
tw_fstat <- tw_sum$fstat
print(
    paste("R2:",
          signif(tw_r2,digits=3),
          "; adj. R2:",
          signif(tw_ar2,digits=3),
          "; F-stat:",
          signif(tw_fstat[1],digits=3),
          "; p-val:",
          signif(1-pf(tw_fstat[1],tw_fstat[2],tw_fstat[3]),digits=3)
          ))

## モデル4
tw_sum <- summary(tw_est4)
tw_r2 <- tw_sum$r.squared
tw_ar2 <- tw_sum$adj.r.squared
tw_fstat <- tw_sum$fstat
print(
    paste("R2:",
          signif(tw_r2,digits=3),
          "; adj. R2:",
          signif(tw_ar2,digits=3),
          "; F-stat:",
          signif(tw_fstat[1],digits=3),
          "; p-val:",
          signif(1-pf(tw_fstat[1],tw_fstat[2],tw_fstat[3]),digits=3)
          ))

## モデル5
tw_sum <- summary(tw_est5)
tw_r2 <- tw_sum$r.squared
tw_ar2 <- tw_sum$adj.r.squared
tw_fstat <- tw_sum$fstat
print(
    paste("R2:",
          signif(tw_r2,digits=3),
          "; adj. R2:",
          signif(tw_ar2,digits=3),
          "; F-stat:",
          signif(tw_fstat[1],digits=3),
          "; p-val:",
          signif(1-pf(tw_fstat[1],tw_fstat[2],tw_fstat[3]),digits=3)
          ))

## モデル1
signif(summary(tw_est1)$coef,digits=3)

## モデル2
signif(summary(tw_est2)$coef,digits=3)

## モデル3
signif(summary(tw_est3)$coef,digits=3)

## モデル4
signif(summary(tw_est4)$coef,digits=3)

## モデル5
signif(summary(tw_est5)$coef,digits=3)

## 診断プロット
library(tidyverse)
library(ggfortify)
autoplot(tw_est4)

### 9,10月のデータでモデルを構築し，8,11月のデータを予測
tw_data <- read.csv("data/tokyo_weather.csv")
tw_train <- subset(tw_data, # モデル推定用データ
                   subset= month %in% c(9,10)) # %in% は集合に含むか
tw_test  <- subset(tw_data, # 予測用データ
                   subset= month %in% c(8,11))

tw_model <- temp ~ solar + press # モデルの定義 
tw_est <- lm(tw_model, data=tw_train) # モデルの推定
summary(tw_est) # モデルの評価
tw_fit  <- predict(tw_est) # データのあてはめ値
tw_pred <- predict(tw_est, # 新規データの予測値
                   newdata=tw_test)

## 予測結果を図示
myColor <- rep("black",12) 
myColor[8:11] <- c("red","orange","violet","blue") # 色の定義
with(tw_train,
     plot(temp ~ tw_fit, pch=1, col=myColor[month],
          xlab="fitted", ylab="observed"))
with(tw_test,
     points(temp ~ tw_pred, pch=4, col=myColor[month]))
abline(0,1,col="gray") # 予測が完全に正しい場合のガイド線
legend("bottomright",inset=.05, pch=15, # 凡例の作成
       legend=c("Aug","Sep","Oct","Nov"), col=myColor[8:11])

### 
### 練習問題 回帰式を用いた予測
### 

### 東京の気候データによる分析

## 信頼区間と予測区間の計算
library(plotrix) # 区間付きのグラフを利用するため
tw_data <- read.csv("data/tokyo_weather.csv")
tw_train <- subset(tw_data, subset= month %in% 8) # 推定用データ
tw_test  <- subset(tw_data, subset= month %in% 9) # 予測用データ
tw_model <- temp ~ solar + press + cloud # モデルの定義 
tw_est <- lm(tw_model, data=tw_train) # モデルの推定

## 信頼区間
tw_fit <- data.frame(tw_train,
                     predict(tw_est, # 回帰式によるあてはめ値を付加
                             interval="confidence")) 
tw_cint <- data.frame(tw_test,
                      predict(tw_est, newdata=tw_test,
                              interval="confidence"))

## 予測区間
tw_pint <- data.frame(tw_test,
                      predict(tw_est, newdata=tw_test,
                              interval="prediction"))

## 8月のデータで推定したモデルで8月をあてはめた信頼区間
with(tw_fit, { # 2つのプロットをまとめて実行
    plotCI(day, fit, ui=upr, li=lwr, # それぞれの列名に注意
           col="blue", scol="steelblue", lwd=2,
           xlab="August", ylab="temperature")
    points(day, temp, col="red", pch=16)
})

## 8月のモデルで9月をあてはめた信頼区間
with(tw_cint, {
    plotCI(day, fit, ui=upr, li=lwr, ylim=c(20,32), 
           col="blue", scol="steelblue", lwd=2,
           xlab="September", ylab="temperature")
    points(day, temp, col="red", pch=16)
})

## 8月のモデルで9月をあてはめた予測区間
with(tw_pint, {
    plotCI(day, fit, ui=upr, li=lwr, ylim=c(20,32),
           col="blue", scol="lightblue", lwd=2,
           xlab="September", ylab="temperature")
    points(day, temp, col="red", pch=16)
})

### 人工データによる検討例
###   (以下はあくまで例なので自由に数値実験を設計して下さい)

## 試行の設定
## モデル: y = -1 + 2*x1
## 人工データの生成
set.seed(1515) # 乱数のシード
n <- 50 # データ数の設定
x1.obs <- runif(n) # 説明変数1
x2.obs <- runif(n) # 説明変数2
beta0 <- -1 # 切片 
beta1 <-  2 # xの係数
beta2 <-  0 # xの係数
sigma <-  1/2 # 誤差の標準偏差
epsilon <- rnorm(length(x1.obs),sd=sigma) # 誤差項
y.obs <- beta0 + beta1*x1.obs + beta2*x2.obs + epsilon # 目的変数
x.obs <- data.frame(x1=x1.obs,
                    x2=x2.obs,
                    y=y.obs) # データフレームの作成
est1 <- lm(y ~ x1, data=x.obs) # x1による回帰分析の実行(正しいモデル)
summary(est1)
est2 <- lm(y ~ x1 + x2, data=x.obs) # x1とx2による回帰分析の実行(冗長なモデル)
summary(est2)
est3 <- lm(y ~ x2, data=x.obs) # x2による回帰分析の実行(誤ったモデル)
summary(est3)

## 新規データに対する予測
x.new <- data.frame(x1=runif(n),
                    x2=runif(n,-10,10)) # 説明変数の新規データ
y.new <- beta0 + beta1*x.new$x1 # 新規データに対する目的変数の真値 (誤差なし)
y.hat1 <- predict(est1, newdata=x.new) # est1による予測値
y.hat2 <- predict(est2, newdata=x.new) # est2による予測値
y.hat3 <- predict(est3, newdata=x.new) # est3による予測値

## 散布図による可視化
plot(y.new ~ y.hat1, 
     col="red", pch=20,
     xlab="fitted value", ylab="observed value") # 黒
points(y.new ~ y.hat2, pch=20, col="green")  # 赤
points(y.new ~ y.hat3, pch=20, col="blue") # 青
abline(0,1, col="gray") # 理想的な結果
legend("bottomright",inset=.05, # 凡例の作成
       col=c("red","green","blue"), pch=c(20,20,20), 
       legend=c("model1","model2","model3"))

## 相関係数による数値的な評価 (R-squared と等価)
cor(y.new, y.hat1)^2 
cor(y.new, y.hat2)^2
cor(y.new, y.hat3)^2

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

### 
### 練習問題 交互作用と非線形を含むモデルとカテゴリカル変数の扱い
### 

### 9月から11月のデータによる分析 (交互作用と非線形性)

## データの整形
tw_subset <- subset(tw_data, subset= month %in% 9:11)

## 日射量，気圧，湿度の線形回帰モデル
summary(lm(temp ~ solar + press + humid, data=tw_subset))
## 湿度の対数を考えた線形回帰モデル
summary(lm(temp ~ solar + press + log(humid), data=tw_subset))
## 最初のモデルにそれぞれの交互作用を加えたモデル (書き方はいろいろある)
summary(lm(temp ~ (solar + press + humid)^2, data=tw_subset))
## 更に3つの変数の積を加えたモデル
summary(lm(temp ~ solar * press * humid, data=tw_subset))

## 用いた変数の散布図
plot(~ temp + solar + press + humid, data=tw_subset)
## 最後のモデルの視覚的な評価 (診断プロット)
plot(lm(temp ~ solar * press * humid, data=tw_subset))

### 雨と気温の関係の分析 (カテゴリカル変数)

## 雨の有無をダミー化(因子化)する
tw_data <- transform(tw_data,
                     rain=factor(rain > 0)) 
summary(lm(temp ~ rain, data=tw_data))
## 通年では雨と気温の関係は積極的に支持されない

## 月毎の気温の差を考慮して月を表す変数(整数値)をダミー化する
tw_data <- transform(tw_data, 
                     month=factor(month))
summary(lm(temp ~ rain + month, data=tw_data))
## 月毎に比較すると雨の日の方が気温が低いことが支持される

## モデルの探索
adv_data <- read.csv('https://www.statlearning.com/s/Advertising.csv',
                     row.names=1) 
summary(lm(sales ~ radio, data=adv_data))
summary(lm(sales ~ TV + radio, data=adv_data))
summary(lm(sales ~ TV + radio + newspaper, data=adv_data))
summary(init <- lm(sales ~ TV * radio * newspaper, data=adv_data))
opt <- step(init) # step関数による探索 (最大のモデルから削減増加を行う)
summary(opt)
