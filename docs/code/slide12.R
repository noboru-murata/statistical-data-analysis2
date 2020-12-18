### 第12回 練習問題解答例

## 基本的なplotの使い方
x <- rnorm(240) # 正規分布のホワイトノイズ
plot(ts(x, start=c(2000,1), frequency=12)) # 2000年からの毎月のデータを想定
## 複数の系列を表示する場合
y <- rt(240,df=4) # t-分布のホワイトノイズ
z <- ts(data.frame(x,y),
	start=c(2000,1), frequency=12) 
plot(z, col="red") # 指定しなければ個別にグラフを描画
plot(z, plot.type="single", col=c("red","blue"))

### 練習1
### 基本的な時系列モデル

Tmax <- 200 # 時系列の長さ t=1,..,Tmax
K <- 5 # 生成する時系列の数

## 系列を異なる色で表示するための準備
## 右下ペインの package タブから RColorBrewer をインストール
## install.packages("RColorBrewer") 
myCol <- brewer.pal(K,"Dark2") # 暗めの色合いのパレットを利用
## 色合いの例は関数 display.brewer.all()
## myCol <- rainbow(K) # いわゆるレインボーカラーを使う場合
## 黄色などが見えにくい場合もあるので，スライドなどに使う場合は注意が必要

## set.seed(123) # 必要なら乱数のシードを指定する
## ホワイトノイズの生成と図示
x <- ts(rnorm(Tmax))
plot(x)
## x <- ts(rt(n, df=4)) # 正規分布ではなく，例えば自由度4のt分布の場合

## 複数の系列を表示
plot(x=ts(1:Tmax),
     ylim=c(-5,5), ylab="value", # yの範囲は適宜調整すること
     main=expression(X[t] == epsilon[t]), # 数式で表示
     type="n") # 枠だけ作図
for(i in 1:K) {
    x <- ts(rnorm(Tmax))
    lines(x, col=myCol[i])
}

## データフレーム化して表示する場合
z <- ts(replicate(K, # K回以下の関数を実行する
                  rnorm(Tmax)))
## 個別に作図
plot(x=z, # 既定値は plot.type="multiple"
     ylab="value", col="blue",
     main=expression(X[t] == epsilon[t]))

## まとめて一枚
plot(x=z, plot.type="single",
     ylab="value", col=myCol,
     main=expression(X[t] == epsilon[t]))

### トレンドのあるホワイトノイズ
x <- ts(rnorm(Tmax) -1 + 0.05*(1:Tmax))
plot(x)

## 複数の系列を表示
z <- ts(replicate(K,
                  rnorm(Tmax) -1 + 0.05*(1:Tmax)))
plot(x=z, plot.type="single",
     ylab="value", col=myCol,
     main=expression(X[t] == -1 + 0.05 * t + epsilon[t]))

### ランダムウォーク
## 定義に則ってrecursiveに計算する
x <- ts(rnorm(Tmax)) # はじめは epslion が入っている
for(t in 2:Tmax) {
    x[t] <- x[t-1] + x[t] # 順に足し合わせていく
}
plot(x)

## 同じ演算をする関数が用意されている
x <- ts(cumsum(rnorm(Tmax))) # 逐次的に加算を行う関数
plot(x)
## 書き方はいろいろあるので考えてみて下さい

## 複数の系列を表示
z <- ts(replicate(K,
                  cumsum(rnorm(Tmax))))
plot(x=z, plot.type="single",
     ylab="value", col=myCol,
     main=expression(X[t] == X[t-1] + epsilon[t]))

### 練習2
### より一般の時系列モデル

## 設定は前の練習問題と同じ
Tmax <- 200 # 時系列の長さ t=1,..,Tmax
K <- 5 # 生成する時系列の数
myCol <- brewer.pal(K,"Dark2") 
## 書き方は以下に示したもの以外にもいろいろあるので考えてみて下さい

### AR(2)モデルのシミュレーション
a <- c(0.67, 0.26) # ARの係数
epsilon <- rnorm(Tmax) # epsilonを生成
x <- double(Tmax)      # 変数を用意
x[1:2] <- epsilon[1:2] # 初期値は(epsilon1, epsilon2)
for(t in 3:Tmax) {
    x[t] <- a %*% x[t-1:2] + epsilon[t] # %*% はベクトルの内積計算
}
plot(ts(x)) # 時系列classに変換して表示

## 複数の系列を表示
myAR <- function(a, epsilon){ # 以下に一連の手続きを記述して関数化しておく
    p <- length(a) # 次数pを取得
    Tmax <- length(epsilon) # 時系列の長さを取得
    x <- double(Tmax)      # 変数を用意
    x[1:p] <- epsilon[1:p] # 初期値は(epsilon1,...)
    for(t in (p+1):Tmax) {
        x[t] <- a %*% x[t-1:p] + epsilon[t]
    }
    return(x) # 計算結果のxを返す
}
## 使い方は a と epsilon(ホワイトノイズ)を指定する
x <- myAR(a=c(0.6, 0.3, 0.1), epsilon=rnorm(100))
plot(ts(x))
## 関数の引数として Tmax を指定する方法もあるが
## 様々なホワイトノイズを試したい場合もあるのでそれを指定するように定義しておく

## データフレームを作成して表示
df.ar <- ts(replicate(K, myAR(a=a, epsilon=rnorm(Tmax))))
plot(x=df.ar, plot.type="single",
     ylab="value", col=myCol,
     main="AR(2)")

### MA(2)モデルのシミュレーション
b <- c(0.44, 0.08) # MAの係数
epsilon <- rnorm(Tmax)
x <- ts(double(Tmax))
x[1:2] <- epsilon[1:2]   
for(t in 3:Tmax) {
    x[t] <- b %*% epsilon[t-1:2] + epsilon[t]
}
plot(ts(x))

## 複数の系列を表示
myMA <- function(b, epsilon){
    q <- length(b) # 次数qを取得
    Tmax <- length(epsilon) # 時系列の長さを取得
    x <- double(Tmax)
    x[1:q] <- epsilon[1:q]   
    for(t in (q+1):Tmax) {
        x[t] <- b %*% epsilon[t-1:q] + epsilon[t]
    }
    return(x)
}
df.ma <- ts(replicate(K, myMA(b=b, epsilon=rnorm(Tmax))))
plot(x=df.ma, plot.type="single",
     ylab="value", col=myCol,
     main="MA(2)")

### ARMA(2,1)モデルのシミュレーション
a <- c(0.8, -0.64) # ARの係数
b <- -0.5          # MAの係数
epsilon <- rnorm(Tmax)
x <- double(Tmax)
x[1:2] <- epsilon[1:2]
for(t in 3:Tmax) {
    x[t] <- a %*% x[t-1:2] + b %*% epsilon[t-1] + epsilon[t]
    ## bは1次元なのでこの問題では b*epsilon でも可
}
plot(ts(x))

## 複数の系列を表示
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
df.arma <- ts(replicate(K, myARMA(a=a, b=b, epsilon=rnorm(Tmax))))
plot(x=df.arma, plot.type="single",
     ylab="value", col=myCol,
     main="ARMA(2,1)")
## 関数 filter や arima.sim などを利用することもできる

### 練習3
### 自己相関

K <- 4 # 表示する時系列の数 (4つを並べて比較する)
## myCol K=5 で作ったものをそのまま利用，別途作成してもよい

### AR(2)モデルの自己相関
par(mfrow=c(2,2)) # グラフを2x2(行方向の順)に並べる
for(i in 1:K) {
  acf(df.ar[,i], col=myCol[i], main=paste("AR series",i))
}

### MA(2)モデルの自己相関
par(mfrow=c(2,2))
for(i in 1:K) {
  acf(df.ma[,i], col=myCol[i], main=paste("MA series",i))
}

### ARMA(2,1)モデルの自己相関
par(mfrow=c(2,2))
for(i in 1:K) {
  acf(df.arma[,i], col=myCol[i], main=paste("ARMA series",i))
}
