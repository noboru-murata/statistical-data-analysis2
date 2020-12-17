### 第12回 練習問題解答例

## 基本的なplotの使い方
x <- rnorm(240) # 正規分布のホワイトノイズ
plot(ts(x, start=c(2000,1), frequency=12)) # 2000年からの毎月のデータを想定
## 複数の系列を表示する場合
y <- rt(240,df=4) # t-分布のホワイトノイズ
z <- ts(data.frame(x,y),
        start=c(2000,1), frequency=12) 
plot(z, col=c("red","blue"))
plot(z, plot.type="single", col=c("red","blue"))

### 練習1
### 基本的な時系列モデル

Tmax <- 200 # 時系列の長さ t=1,..,Tmax
K <- 5 # 生成する時系列の数
myCol <- rainbow(K) # 系列ごとに色分けして表示する

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

### トレンドのあるホワイトノイズ
plot(x=ts(1:Tmax),
     ylim=c(-5,12), ylab="value",
     main=expression(X[t] == -1 + 0.05 * t + epsilon[t]),
     type="n")
for(i in 1:K) {
  x <- ts(rnorm(Tmax) -1 + 0.05*(1:Tmax))
  lines(x, col=myCol[i])
}

### ランダムウォーク
plot(x=ts(1:Tmax),
     ylim=c(-20,20), ylab="value",
     main=expression(X[t] == X[t-1] + epsilon[t]),
     type="n")
for(i in 1:K) {
  x <- ts(cumsum(rnorm(Tmax))) # 逐次的に加算する関数
  lines(x, col=myCol[i])
}
## recursiveに計算してもよい
x <- ts(rnorm(Tmax)) # はじめは epslion が入っている
for(t in 2:Tmax) {
  x[t] <- x[t-1] + x[t] # 順に足し合わせていく
}
plot(x)
## 書き方はいろいろあるので考えてみて下さい

### 練習2
### より一般の時系列モデル

## 設定は前の練習問題と同じ
Tmax <- 200 # 時系列の長さ t=1,..,Tmax
K <- 5 # 生成する時系列の数
myCol <- rainbow(K) # 系列ごとに色分けして表示する
## 書き方は以下に示したもの以外にもいろいろあるので考えてみて下さい

### AR(2)モデルのシミュレーション
a <- c(0.67, 0.26) # ARの係数
plot(x=ts(1:Tmax),
     ylim=c(-5,5), ylab="value",
     main="AR(2)",
     type="n")
for(i in 1:K) {
  epsilon <- rnorm(Tmax) # epsilonを生成
  x <- ts(double(Tmax))  # 変数を用意
  x[1:2] <- epsilon[1:2] # 初期値は(epsilon1, epsilon2)
  for(t in 3:Tmax) x[t] <- a %*% x[t-1:2] + epsilon[t]
  lines(x, col=myCol[i])
}
## %*% はベクトルの内積を計算する

### MA(2)モデルのシミュレーション
b <- c(0.44, 0.08) # MAの係数
plot(x=ts(1:Tmax),
     ylim=c(-5,5), ylab="value",
     main="MA(2)",
     type="n")
for(i in 1:K) {
  epsilon <- rnorm(Tmax)
  x <- ts(double(Tmax))
  x[1:2] <- epsilon[1:2]   
  for(t in 3:Tmax) x[t] <- b %*% epsilon[t-1:2] + epsilon[t]
  lines(x, col=myCol[i])
}

### ARMA(2,1)モデルのシミュレーション
a <- c(0.8, -0.64) # ARの係数
b <- -0.5          # MAの係数
plot(x=ts(1:Tmax),
     ylim=c(-5,5), ylab="value",
     main="ARMA(2,1)",
     type="n")
for(i in 1:K) {
  epsilon <- rnorm(Tmax)
  x <- ts(double(Tmax))
  x[1:2] <- epsilon[1:2]
  for(t in 3:Tmax) x[t] <- a %*% x[t-1:2] + b %*% epsilon[t-1] + epsilon[t]
  lines(x, col=myCol[i])
}
## bは1次元なので b*epsilon でも可

## 関数 filter や arima.sim などを利用することもできる

### 練習3
### 自己相関

## 前の練習問題と設定を変えている
Tmax <- 300 # 時系列の長さ (長いほど推定精度はよい)
K <- 4 # 生成する時系列の数 (4つを並べて比較する)
myCol <- rainbow(K) # 系列ごとに色分けして表示する

### AR(2)モデルの自己相関
par(mfrow=c(2,2)) # グラフを2x2(行方向の順)に並べる
a <- c(0.67, 0.26) # ARの係数
for(i in 1:K) {
  epsilon <- rnorm(Tmax) 
  x <- ts(double(Tmax))  
  x[1:2] <- epsilon[1:2] 
  for(t in 3:Tmax) x[t] <- a %*% x[t-1:2] + epsilon[t]
  acf(x, col=myCol[i])
}

### MA(2)モデルの自己相関
par(mfrow=c(2,2))
b <- c(0.44, 0.08) # MAの係数
for(i in 1:K) {
  epsilon <- rnorm(Tmax)
  x <- ts(double(Tmax))
  x[1:2] <- epsilon[1:2]   
  for(t in 3:Tmax) x[t] <- b %*% epsilon[t-1:2] + epsilon[t]
  acf(x, col=myCol[i])
}

### ARMA(2,1)モデルの自己相関
par(mfrow=c(2,2))
a <- c(0.8, -0.64) # ARの係数
b <- -0.5          # MAの係数
for(i in 1:K) {
  epsilon <- rnorm(Tmax)
  x <- ts(double(Tmax))
  x[1:2] <- epsilon[1:2]
  for(t in 3:Tmax) x[t] <- a %*% x[t-1:2] + b %*% epsilon[t-1] + epsilon[t]
  acf(x, col=myCol[i])
}
