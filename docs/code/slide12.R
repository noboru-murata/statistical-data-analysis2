### 第12講 サンプルコード
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(tidyverse)
library(ggfortify)

ts(data = NA, start = 1, end = numeric(), frequency = 1)
## data: ベクトル，または行列(データフレーム)
## start: 開始時刻
## end: 終了時刻
## frequency: 単位時間あたりの観測回数
ts(data = x) # t=1,2,... を添字とする時系列
ts(data = x, start = c(2020,1), frequency =12) # 2020年1月からの月ごと
ts(data = x, start = c(2020,3), frequency =4) # 四半期ごと

## 基本的なplotの使い方
x <- rnorm(240) # 正規分布のホワイトノイズ
plot(ts(x, start=c(2000,1), frequency=12)) # 2000年からの毎月のデータを想定
## 複数の系列を表示する場合
y <- rt(240,df=4) # t-分布のホワイトノイズ
z <- ts(data.frame(x,y),
	    start=c(2000,1), frequency=12) 
plot(z, col="red") # 指定しなければ個別にグラフを描画
plot(z, plot.type="single", col=c("red","blue"))

## 基本的なplotの使い方
x <- rnorm(240) # 正規分布のホワイトノイズ
plot(ts(x, start=c(2000,1), frequency=12)) # 2000年からの毎月のデータを想定
## 複数の系列を表示する場合
y <- rt(240,df=4) # t-分布のホワイトノイズ
z <- ts(data.frame(x,y),
	    start=c(2000,1), frequency=12) 
plot(z, col="red") # 指定しなければ個別にグラフを描画
plot(z, plot.type="single", col=c("red","blue"))

#' ---------------------------------------------------------------------------
#' @practice 基本的な時系列モデル

Tmax <- 200 # 時系列の長さ t=1,..,Tmax
K <- 5 # 生成する時系列の数
#' set.seed(123) # 必要なら乱数のシード値を指定する

#' ホワイトノイズの生成と図示
x <- ts(rnorm(Tmax, sd=2)) # 分散4=標準偏差2
#' x <- ts(rt(n, df=4)) # 正規分布ではなく，例えば自由度4のt分布の場合
autoplot(x)
#' @notes
#' 色や線種を指定することもできる
#' また ggplot オブジェクトとして修飾することも可能
autoplot(x,
         ts.colour = "blue",
         ts.linetype = "dotted") +
  labs(x = "Time", y = "Observation")

#' トレンドのあるホワイトノイズ
x <- ts(rnorm(Tmax, sd=2) -1 + 0.05*(1:Tmax))
autoplot(x)

#' ランダムウォーク
#' 定義に則って再帰的に計算する
x <- ts(rnorm(Tmax, sd=2)) # はじめは epslion が入っている
for(t in 2:Tmax) {
  x[t] <- x[t-1] + x[t] # 順に足し合わせていく
}
autoplot(x)

#' 同じ演算をする関数が用意されている
x <- ts(cumsum(rnorm(Tmax, sd=2))) # 逐次的に加算を行う関数
autoplot(x)
#' 書き方はいろいろあるので考えてみて下さい

#' 複数の系列を表示する場合
#' ホワイトノイズの生成と図示
z <- ts(replicate(K, # K回以下の関数を実行する
                  rnorm(Tmax, sd = 2)))
autoplot(z) # 各系列を個別に表示する
autoplot(z, facets = FALSE) # 同じグラフに重ね描きする
autoplot(z, facets = FALSE) +
  theme(legend.position = "none") + #' 凡例を削除
  labs(title = expression(X[t] == epsilon[t]), # 数式で表示
       x = "Time", y = "Observation")

#' トレンドのあるホワイトノイズ
z <- ts(replicate(K,
                  rnorm(Tmax, sd = 2) -1 + 0.05*(1:Tmax)))
autoplot(z, facets = FALSE) +
  theme(legend.position = "none") + 
  labs(title = expression(X[t] == -1 + 0.05 * t + epsilon[t]),
       x = "Time", y = "Observation")

#' ランダムウォーク
z <- ts(replicate(K,
                  cumsum(rnorm(Tmax, sd=2))))
autoplot(z, facets = FALSE) +
  theme(legend.position = "none") + 
  labs(title = expression(X[t] == X[t-1] + epsilon[t]),
       x = "Time", y = "Observation")

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice より一般の時系列モデル

#' 設定は前の練習問題と同じ
Tmax <- 200 # 時系列の長さ t=1,..,Tmax
K <- 5 # 生成する時系列の数
#' 書き方は以下に示したもの以外にもいろいろあるので考えてみて下さい

#' AR(2)モデルのシミュレーション
a <- c(0.67, 0.26) # ARの係数
epsilon <- rnorm(Tmax) # epsilonを生成
x <- double(Tmax)      # 変数を用意
x[1:2] <- epsilon[1:2] # 初期値は(epsilon1, epsilon2)
for(t in 3:Tmax) {
  x[t] <- a %*% x[t-1:2] + epsilon[t] # %*% はベクトルの内積計算
}
autoplot(ts(x)) # 時系列classに変換して表示

#' 複数の系列を表示
my_ar <- function(a, epsilon){ # 以下に一連の手続きを記述して関数化しておく
  p <- length(a) # 次数pを取得
  Tmax <- length(epsilon) # 時系列の長さを取得
  x <- double(Tmax)      # 変数を用意
  x[1:p] <- epsilon[1:p] # 初期値は(epsilon1,...)
  for(t in (p+1):Tmax) {
    x[t] <- a %*% x[t-1:p] + epsilon[t]
  }
  return(x) # 計算結果のxを返す
}
#' 使い方は a と epsilon(ホワイトノイズ)を指定する
x <- my_ar(a = c(0.6, 0.3, 0.1), epsilon = rnorm(100))
autoplot(ts(x))
#' @notes
#' 関数の引数として Tmax を指定する方法もあるが
#' 様々な分布のホワイトノイズを試したい場合もあるので
#' 生成の元となるノイズを渡す形で定義してある

#' データフレームを作成して表示
ts(replicate(K, my_ar(a = a, epsilon = rnorm(Tmax)))) |>
  autoplot(facets = FALSE) +
  theme(legend.position = "none") + 
  labs(title = "AR(2)", x = "Time", y = "Observation")

#' MA(2)モデルのシミュレーション
b <- c(0.44, 0.08) # MAの係数
epsilon <- rnorm(Tmax)
x <- ts(double(Tmax))
x[1:2] <- epsilon[1:2]   
for(t in 3:Tmax) {
  x[t] <- b %*% epsilon[t-1:2] + epsilon[t]
}
autoplot(ts(x))

#' 複数の系列を表示
my_ma <- function(b, epsilon){
  q <- length(b) # 次数qを取得
  Tmax <- length(epsilon) # 時系列の長さを取得
  x <- double(Tmax)
  x[1:q] <- epsilon[1:q]   
  for(t in (q+1):Tmax) {
    x[t] <- b %*% epsilon[t-1:q] + epsilon[t]
  }
  return(x)
}
ts(replicate(K, my_ma(b = b, epsilon = rnorm(Tmax)))) |>
  autoplot(facets = FALSE) +
  theme(legend.position = "none") + 
  labs(title = "MA(2)", x = "Time", y = "Observation")

#' ARMA(2,1)モデルのシミュレーション
a <- c(0.8, -0.64) # ARの係数
b <- -0.5          # MAの係数
epsilon <- rnorm(Tmax)
x <- double(Tmax)
x[1:2] <- epsilon[1:2]
for(t in 3:Tmax) {
  x[t] <- a %*% x[t-1:2] + b %*% epsilon[t-1] + epsilon[t]
  #' bは1次元なのでこの問題では b*epsilon でも可
}
plot(ts(x))

#' 複数の系列を表示
my_arma <- function(a, b, epsilon){
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
ts(replicate(K, my_arma(a = a, b = b, epsilon = rnorm(Tmax)))) |>
  autoplot(facets = FALSE) +
  theme(legend.position = "none") + 
  labs(title = "ARMA(2,1)", x = "Time", y = "Observation")
#' 関数 filter や arima.sim などを利用することもできる

#' ---------------------------------------------------------------------------
