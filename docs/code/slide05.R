### 
### 第5講 サンプルコード
###

## データの読み込み
TW.data <- read.csv("data/tokyo_weather.csv")
TW.subset <- subset(TW.data, # 8月のデータの抽出
                    subset= month==8)
head(TW.subset[,-1], 14) # 2週間分を表示

### モデル式
TW.model0 <- temp ~ press + solar + humid + cloud
TW.model1 <- temp ~ press
TW.model2 <- temp ~ press + solar
TW.model3 <- temp ~ press + solar + humid
TW.model4 <- temp ~ press + solar + cloud

## 推定
TW.est0 <- lm(TW.model0, data=TW.subset, y=TRUE)
TW.est1 <- lm(TW.model1, data=TW.subset, y=TRUE)
TW.est2 <- lm(TW.model2, data=TW.subset, y=TRUE)
TW.est3 <- lm(TW.model3, data=TW.subset, y=TRUE)
TW.est4 <- lm(TW.model4, data=TW.subset, y=TRUE)

## 説明変数と目的変数の散布図
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(model.frame(TW.est0),
     labels=c("気温","気圧","日射","湿度","雲量"),
     col="blue", pch=20)

## 観測値とあてはめ値の比較
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
with(TW.est1,
     plot(y,fitted.values,col="orange",pch=17, # 三角
          xlab="気温",
          ylab="あてはめ値",ylim=range(y)))   
abline(0,1,col="red",lwd=2)
with(TW.est2,
     points(y,fitted.values,col="green",pch=15)) # 四角
with(TW.est3,
     points(y,fitted.values,col="blue",pch=21))  # 丸
with(TW.est4,
     points(y,fitted.values,col="brown",pch=23))  # 菱形
legend("bottomright",inset=.05, # 凡例の作成
       col=c("orange","green","blue","brown"), pch=c(17,15,21,23), 
       legend=c("モデル1","モデル2","モデル3","モデル4"))

## モデル1
print(
    paste("R2:",
          signif(summary(TW.est1)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(TW.est1)$adj.r.squared,digits=3)
          ))

## モデル2
print(
    paste("R2:",
          signif(summary(TW.est2)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(TW.est2)$adj.r.squared,digits=3)
          ))

## モデル3
print(
    paste("R2:",
          signif(summary(TW.est3)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(TW.est3)$adj.r.squared,digits=3)
          ))

## モデル4
print(
    paste("R2:",
          signif(summary(TW.est4)$r.squared,digits=3),
          "; adj. R2:",
          signif(summary(TW.est4)$adj.r.squared,digits=3)
          ))

## モデル1
TW.sum <- summary(TW.est1)
TW.r2 <- TW.sum$r.squared
TW.ar2 <- TW.sum$adj.r.squared
TW.fstat <- TW.sum$fstat
print(
    paste("R2:",
          signif(TW.r2,digits=3),
          "; adj. R2:",
          signif(TW.ar2,digits=3),
          "; F-stat:",
          signif(TW.fstat[1],digits=3),
          "; p-val:",
          signif(1-pf(TW.fstat[1],TW.fstat[2],TW.fstat[3]),digits=3)
          ))

## モデル2
TW.sum <- summary(TW.est2)
TW.r2 <- TW.sum$r.squared
TW.ar2 <- TW.sum$adj.r.squared
TW.fstat <- TW.sum$fstat
print(
    paste("R2:",
          signif(TW.r2,digits=3),
          "; adj. R2:",
          signif(TW.ar2,digits=3),
          "; F-stat:",
          signif(TW.fstat[1],digits=3),
          "; p-val:",
          signif(1-pf(TW.fstat[1],TW.fstat[2],TW.fstat[3]),digits=3)
          ))

## モデル3
TW.sum <- summary(TW.est3)
TW.r2 <- TW.sum$r.squared
TW.ar2 <- TW.sum$adj.r.squared
TW.fstat <- TW.sum$fstat
print(
    paste("R2:",
          signif(TW.r2,digits=3),
          "; adj. R2:",
          signif(TW.ar2,digits=3),
          "; F-stat:",
          signif(TW.fstat[1],digits=3),
          "; p-val:",
          signif(1-pf(TW.fstat[1],TW.fstat[2],TW.fstat[3]),digits=3)
          ))

## モデル4
TW.sum <- summary(TW.est4)
TW.r2 <- TW.sum$r.squared
TW.ar2 <- TW.sum$adj.r.squared
TW.fstat <- TW.sum$fstat
print(
    paste("R2:",
          signif(TW.r2,digits=3),
          "; adj. R2:",
          signif(TW.ar2,digits=3),
          "; F-stat:",
          signif(TW.fstat[1],digits=3),
          "; p-val:",
          signif(1-pf(TW.fstat[1],TW.fstat[2],TW.fstat[3]),digits=3)
          ))

## モデル1
signif(summary(TW.est1)$coef,digits=3)

## モデル2
signif(summary(TW.est2)$coef,digits=3)

## モデル3
signif(summary(TW.est3)$coef,digits=3)

## モデル4
signif(summary(TW.est4)$coef,digits=3)

### 9,10月のデータでモデルを構築し，8,11月のデータを予測
TW.data <- read.csv("data/tokyo_weather.csv")
TW.train <- subset(TW.data, # モデル推定用データ
                   subset= month %in% c(9,10)) # %in% は集合に含むか
TW.test  <- subset(TW.data, # 予測用データ
                   subset= month %in% c(8,11))

TW.model <- temp ~ solar + press # モデルの定義 
TW.est <- lm(TW.model, data=TW.train) # モデルの推定
summary(TW.est) # モデルの評価
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

### 
### 練習問題 回帰式を用いた予測
### 

### 東京の気候データによる分析

## 信頼区間と予測区間の計算
library(plotrix) # 区間付きのグラフを利用するため
TW.data <- read.csv("data/tokyo_weather.csv")
TW.train <- subset(TW.data, subset= month %in% 8) # 推定用データ
TW.test  <- subset(TW.data, subset= month %in% 9) # 予測用データ
TW.model <- temp ~ solar + press + cloud # モデルの定義 
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

## 8月のデータで推定したモデルで8月をあてはめた信頼区間
with(TW.fit, { # 2つのプロットをまとめて実行
    plotCI(day, fit, ui=upr, li=lwr, # それぞれの列名に注意
           col="blue", scol="steelblue", lwd=2,
           xlab="August", ylab="temperature")
    points(day, temp, col="red", pch=16)
})

## 8月のモデルで9月をあてはめた信頼区間
with(TW.cint, {
    plotCI(day, fit, ui=upr, li=lwr, ylim=c(20,32), 
           col="blue", scol="steelblue", lwd=2,
           xlab="September", ylab="temperature")
    points(day, temp, col="red", pch=16)
})

## 8月のモデルで9月をあてはめた予測区間
with(TW.pint, {
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

### 雨と気温の関係の分析 (カテゴリカル変数)

## 雨の有無をダミー化(因子化)する
TW.data <- transform(TW.data,
                     rain=factor(rain > 0)) 
summary(lm(temp ~ rain, data=TW.data))
## 通年では雨と気温の関係は積極的に支持されない

## 月毎の気温の差を考慮して月を表す変数(整数値)をダミー化する
TW.data <- transform(TW.data, 
                     month=factor(month))
summary(lm(temp ~ rain + month, data=TW.data))
## 月毎に比較すると雨の日の方が気温が低いことが支持される

## モデルの探索
Adv.data <- read.csv('https://www.statlearning.com/s/Advertising.csv',
                     row.names=1) 
summary(lm(sales ~ radio, data=Adv.data))
summary(lm(sales ~ TV + radio, data=Adv.data))
summary(lm(sales ~ TV + radio + newspaper, data=Adv.data))
summary(init <- lm(sales ~ TV * radio * newspaper, data=Adv.data))
opt <- step(init)
summary(opt)
