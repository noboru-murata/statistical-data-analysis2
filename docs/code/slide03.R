### 第3講 資料

###
### 練習問題 回帰係数の推定
###

### 東京の気候データによる回帰分析

## データの読み込み
TW.data <- read.csv("data/tokyo_weather.csv")

## モデルの推定: 8月の"気温"を目的変数，"日射量・気圧"を説明変数とする
TW.model <- temp ~ solar + press # モデル式の定義 
## class(model) # モデルは formula class
(TW.est <- lm(TW.model, # 回帰係数の推定
              data=subset(TW.data, # 8月のデータの抽出
                          subset= month==8)))
TW.df <- model.frame(TW.est) # 推定に用いたデータフレームの抽出
plot(TW.df, col="blue") # 散布図

## 散布図と回帰式の定める平面の描画(3次元プロット)
library(scatterplot3d) # パッケージの読み込み
s3d <- scatterplot3d( 
    TW.df[c("solar","press","temp")], # x,y,z の順
    type="p", # plotの種類: "p"点，"l"線，"h"足付き
    pch=16,# 点の種類 (?points 参照)
    angle=30, # xy平面の見る方向 (適宜調整)
    highlight.3d=TRUE # 高さ(z)ごとに色を変える
)
s3d$plane3d(TW.est, col="blue", # 回帰式の定める平面の追加
            draw_polygon=TRUE, # 平面の塗り潰しの設定
            polygon_args=list(col=rgb(0,0,1,0.1))) 

### 広告費と売上データによる回帰分析

## データの読み込み
Adv.data <- read.csv('https://www.statlearning.com/s/Advertising.csv',
                     row.names=1) # 1列目を行名として読み込む
plot(Adv.data, col="orange") # 散布図

## TVの宣伝費で売上を説明
(Adv.est1 <- lm(sales ~ TV, data=Adv.data))
plot(sales ~ TV, data=Adv.data, col="orange")
abline(Adv.est1, col="brown", lwd=2)

## radioの宣伝費で売上を説明
(Adv.est2 <- lm(sales ~ radio, data=Adv.data))
plot(sales ~ radio, data=Adv.data, col="orange")
abline(Adv.est2, col="brown", lwd=2)

## 両者の宣伝費で売上を説明
(Adv.est <- lm(sales ~ TV + radio, data=Adv.data))
s3d <- scatterplot3d( 
    model.frame(Adv.est)[c("TV","radio","sales")], # x,y,z の順
    type="p", # plotの種類: "p"点，"l"線，"h"足付き
    pch=16,# 点の種類 (?points 参照)
    angle=45, # xy平面の見る方向 (適宜調整)
    highlight.3d=TRUE # 高さ(z)ごとに色を変える
)
s3d$plane3d(Adv.est, col="brown", # 回帰式の定める平面の追加
            draw_polygon=TRUE, # 平面の塗り潰しの設定
            polygon_args=list(col=rgb(1,0,0,0.1)))

### 
### 練習問題 最小二乗推定量の性質
### 

### 東京の気候データ

## 回帰係数と正規方程式の解の一致
(beta <- coef(TW.est))        # 推定された回帰係数
X <- model.matrix(TW.est)     # デザイン行列
Y <- model.frame(TW.est)[[1]] # 目的変数 (データフレームの1列目に入っている)
solve(crossprod(X)) %*% crossprod(X, Y) # 正規方程式の解

## あてはめ値と残差の直交性
yhat <- fitted(TW.est) # あてはめ値
ehat <- resid(TW.est)  # 残差
yhat %*% ehat          # 直交すれば内積はO(に近い値)となる

## 回帰式が標本平均を通ること
colMeans(X) %*% beta # 説明変数の標本平均のあてはめ値
mean(Y)              # 目的変数の標本平均 

### 広告費と売上データ

## 回帰係数と正規方程式の解の一致
(beta <- coef(Adv.est))        # 推定された回帰係数
X <- model.matrix(Adv.est)     # デザイン行列
Y <- model.frame(Adv.est)[[1]] # 目的変数
solve(crossprod(X)) %*% crossprod(X, Y) # 正規方程式の解

## あてはめ値と残差の直交性
yhat <- fitted(Adv.est) # あてはめ値
ehat <- resid(Adv.est)  # 残差
yhat %*% ehat           # 直交すれば内積はO

## 回帰式が標本平均を通ること
colMeans(X) %*% beta # 説明変数の標本平均のあてはめ値
mean(Y)              # 目的変数の標本平均

### 
### 練習問題 残差の分解
### 

### 東京の気候データ

TW.model # モデルの確認
## 以下は目的変数を推定結果に含める方法
TW.est <- lm(TW.model,
             data=subset(TW.data, # 8月のデータの抽出
                         subset= month==8),
             y=TRUE) # 目的変数をYとして返すように指定
Y <- with(TW.est,y)                     # 目的変数の取得
(Sy <- sum((Y-mean(Y))^2))              # 目的変数のばらつき
(S <- sum(resid(TW.est)^2))             # 残差のばらつき
(Sr <- sum((fitted(TW.est)-mean(Y))^2)) # 回帰のばらつき
S+Sr # Sy と同じになっている

### 広告費と売上データ

summary(Adv.est)
Y <- model.frame(Adv.est)[[1]]           # 目的変数の取得
(Sy <- sum((Y-mean(Y))^2))               # 目的変数のばらつき
(S <- sum(resid(Adv.est)^2))             # 残差のばらつき
(Sr <- sum((fitted(Adv.est)-mean(Y))^2)) # 回帰のばらつき
S+Sr # Sy と同じになっている

### 
### 練習問題 決定係数によるモデルの比較
###

### 東京の気候データ

## モデルの比較
TW.subset <- subset(TW.data, # 8月のデータの抽出
                    subset= month==8)
TW.model1 <- temp ~ solar
TW.model2 <- temp ~ solar + press
TW.model3 <- temp ~ solar + press + cloud
TW.est1 <- lm(TW.model1, data=TW.subset, y=TRUE)
TW.est2 <- lm(TW.model2, data=TW.subset, y=TRUE)
TW.est3 <- lm(TW.model3, data=TW.subset, y=TRUE)
summary(TW.est1)$adj.r.squared # 自由度調整済み決定係数
summary(TW.est2)$adj.r.squared # (model1より上昇)
summary(TW.est3)$adj.r.squared # (model2より上昇)

## 予測値と実測値の比較
with(TW.est1,
     plot(y,fitted.values,col="orange",pch=17, # 三角
          xlab="temperature",
          ylab="fitted values",ylim=range(y)))   
abline(0,1,col="red",lwd=2)
with(TW.est2,
     points(y,fitted.values,col="green",pch=15)) # 四角
with(TW.est3,
     points(y,fitted.values,col="blue",pch=16))  # 丸
legend("bottomright",inset=.05, # 凡例の作成
       col=c("orange","green","blue"), pch=c(17,15,16), 
       legend=c("model1","model2","model3"))

### 広告費と売上データ

## モデルの比較
Adv.model1 <- sales ~ TV
Adv.model2 <- sales ~ radio
Adv.model3 <- sales ~ TV + radio
Adv.est1 <- lm(Adv.model1, data=Adv.data, y=TRUE)
Adv.est2 <- lm(Adv.model2, data=Adv.data, y=TRUE)
Adv.est3 <- lm(Adv.model3, data=Adv.data, y=TRUE)
summary(Adv.est1)$adj.r.squared # 自由度調整済み決定係数
summary(Adv.est2)$adj.r.squared # (model1より減少)
summary(Adv.est3)$adj.r.squared # (model1より上昇)

## 予測値と実測値の比較
with(Adv.est1,
     plot(y,fitted.values,col="orange",pch=17, # 三角
          xlab="sales",
          ylab="fitted values",ylim=range(y)))   
abline(0,1,col="red",lwd=2)
with(Adv.est2,
     points(y,fitted.values,col="green",pch=15)) # 四角
with(Adv.est3,
     points(y,fitted.values,col="blue",pch=16))  # 丸
legend("bottomright",inset=.05, # 凡例の作成
       col=c("orange","green","blue"), pch=c(17,15,16), 
       legend=c("model1","model2","model3"))
