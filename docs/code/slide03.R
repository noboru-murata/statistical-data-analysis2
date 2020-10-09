### 第03回 練習問題解答例

### 練習6 (第2回の続き)
### 擬似乱数

## 関数sampleの使い方
(x <- 1:10)   # サンプリング対象の集合を定義
set.seed(123) # 乱数のシード値(任意に決めてよい)を指定
sample(x, 5)               # xから5つの要素を重複なしでランダムに抽出
sample(x, 5, replace=TRUE) # xから5つの要素を重複ありでランダムに抽出
sample(x, length(x))       # xの要素のランダムな並べ替え
sample(1:6, 10, replace=TRUE)           # サイコロを10回振る実験の再現
sample(1:6, 10, prob=6:1, replace=TRUE) # 出る目の確率に偏りがある場合

## 関数rbinomの使い方
rbinom(10, size=4, prob=0.5) # 表(1)の出る確率が0.5にコインを4枚投げる試行を10回
rbinom(20, size=4, prob=0.2) # 個数を20, 確率を0.2に変更

## 関数runifの使い方
runif(5, min=-1, max=2) # 区間(-1,2)上の一様乱数を5個発生
runif(5)                # 指定しない場合は区間(0,1)が既定値

## 関数rnormの使い方
rnorm(10, mean=5, sd=3) # 平均5，分散3^2の正規乱数を10個発生
rnorm(10)               # 指定しない場合は mu=0, sd=1 が既定値

## 関数set.seedについて
set.seed(1) # 乱数の初期値をseed=1で指定
runif(5) 
set.seed(2) # 乱数の初期値をseed=2で指定
runif(5)    # seed=1の場合と異なる結果
set.seed(1) # 乱数の初期値をseed=1で指定
runif(5)    # 初めのseed=1の場合と同じ結果

### 中心極限定理

## 確率変数の分布の設定 (例: 区間[-1,1]の一様乱数)
myrand <- function(n) { # n個の乱数を生成
    return(runif(n,min=-1,max=1))
}
## 標本平均の計算
mymean <- function(n) { # n個のデータで計算
    return(mean(myrand(n)))
}

## Monte-Carlo実験
set.seed(123) # 実験を再現したい場合はシードを指定する
mu <- 0; sigma <- sqrt(1/3) # 理論平均と標準偏差
mc <- 5000 # 実験の繰り返し回数
for(n in c(1,2,4,8,16)){ # nを変えて実験
    xbars <- replicate(mc, mymean(n)) # mc回実験し標本平均を記録
    hist(xbars, breaks=25, freq=FALSE, # 分布を表示
         col="orchid", border="slateblue",
         xlab=expression(bar(X)), main=paste0("n=",n))
    thdist <- function(x){dnorm(x,mean=mu,sd=sigma/sqrt(n))}
    curve(thdist, add=TRUE, col="orange", lwd=2) # 理論曲線を重ねる
}

### コイン投げの賭け

## コイン投げの試行 (いろいろな書き方があるので以下は一例)
mytrial <- function(){
    while(TRUE){ # 永久に回るループ
        if(rbinom(1,size=1,prob=0.5)==1){return("A")} # Aが表で終了
        if(rbinom(1,size=1,prob=0.5)==1){return("B")} # Bが表で終了
        ## どちらも裏ならもう一度ループ
    }
}

## Monte-Carlo実験
set.seed(8888) # 実験を再現したい場合はシードを指定する
mc <- 10000 # 実験回数を設定 
mydata <- replicate(mc,mytrial()) 
## 簡単な集計
table(mydata)    # 頻度
table(mydata)/mc # 確率(推定値)

### 練習7 (第2回の続き)
### 双六ゲーム

## 双六の試行
mytrial <- function(){
    step <- 0 # 最初の位置
    num <- 0  # さいころを振る回数
    while(TRUE){ # 永久に回るループ
        step <- step + sample(1:6,1) # さいころを振る
        num <- num + 1 # 回数を記録
        if(step >= 100) { # ゴールしたか?
            return(num) # 回数を出力して関数を終了
        }
    }
}

## 試行を行ってみる
for(i in 1:10) print(mytrial())

## Monte-Carlo実験
set.seed(12345)
mc <- 10000 # 実験回数を設定 
mydata <- replicate(mc,mytrial()) 
hist(mydata) # ヒストグラムを出力
summary(mydata) # 簡単な集計

### 練習1
### 回帰係数の推定

### 東京の気候データによる回帰分析
## モデル: 8月の"気温"を目的変数，"日射量・気圧"を説明変数とする
TW.data <- read.csv("data/tokyo_weather_reg.csv")
TW.model <- temp ~ solar + press # モデル式の定義 
## class(model) # モデルは formula class
(TW.est <- lm(TW.model, # 回帰係数の推定
           data=subset(TW.data, # 8月のデータの抽出
                       subset= months(as.Date(date),
                                      abbreviate=TRUE)==" 8"))) 
TW.df <- model.frame(TW.est) # 推定に用いたデータフレームの抽出 (後述)
plot(TW.df, col="blue")   # 散布図

## 散布図と回帰式の定める平面の描画(3次元プロット)
require(scatterplot3d) # パッケージの読み込み
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
Adv.data <- read.csv("data/Advertising.csv",
                     row.names=1) # 1列目を行名として読み込む
## 以下のように download しないで URL を指定してもよい
## read.csv("http://faculty.marshall.usc.edu/gareth-james/ISL/Advertising.csv",
##          row.names=1)
plot(Adv.data, col="orange")
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

### 練習2
### 最小二乗推定量の性質

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

### 練習3
### 残差の分解

### 東京の気候データ
TW.model
TW.est <- lm(TW.model,
             data=subset(TW.data, # 8月のデータの抽出
                         subset= months(as.Date(date),
                                        abbreviate=TRUE)==" 8"),
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

### 練習4
### 決定係数によるモデルの比較

### 東京の気候データ
TW.subset <- subset(TW.data, # 8月のデータの抽出
                    subset= months(as.Date(date),
                                   abbreviate=TRUE)==" 8")
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
