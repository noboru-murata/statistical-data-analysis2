### 第12講 サンプルコード
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
  )
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

x <- rnorm(24) # 正規分布のホワイトノイズ
ts(data = x) # t=1,2,... を添字とする単純な時系列
ts(data = x, start = c(2020,1), frequency =12) # 2020年1月からの月ごと
ts(data = x, start = c(2020,3), frequency =4) # 四半期ごと

#' 単一時系列の描画
x <- rnorm(240) # 正規分布のホワイトノイズ
ts(x, start = c(2000,1), frequency = 12) |> # 2000年1月から毎月のデータ
    as_tsibble() |> # tsibbleクラスに変換
    autoplot(value) # value (as_tsibbleによる時系列データの列名の規定値) を描画
#' 複数の系列を表示する場合
y <- rt(240, df=4) # t-分布のホワイトノイズ
z <- ts(tibble(x,y), start = c(2000,1), frequency = 12)
z |> as_tsibble() |>
    autoplot(value) # 同一のグラフで色を変えて描画
z |> as_tsibble() |>
    autoplot(value) + # 別にする場合は facet を指定すればよい
    facet_grid(key ~ .)

#' ---------------------------------------------------------------------------
#' @practice 基本的な時系列モデル

Tmax <- 200 # 時系列の長さ t=1,..,Tmax
K <- 5 # 生成する時系列の数
#' set.seed(123) # 必要なら乱数のシード値を指定する

#' ホワイトノイズの生成と図示
x <- ts(rnorm(Tmax, sd=2)) # 分散4=標準偏差2
#' x <- ts(rt(n, df=4)) # 正規分布ではなく，例えば自由度4のt分布の場合
x |> as_tsibble() |>
    autoplot(value)
#' @notes
#' 色や線種を指定することもできる
x |> as_tsibble() |>
    autoplot(value,
             colour = "blue",
             linetype = "dotted")
#' ggplot オブジェクトとして修飾することも可能
x |> as_tsibble() |>
    autoplot(value, colour = "blue") +
    labs(x = "time", y = "observation")

#' トレンドのあるホワイトノイズ
x <- ts(rnorm(Tmax, sd=2) -1 + 0.05*(1:Tmax))
x |> as_tsibble() |> autoplot(value)

#' ランダムウォーク
#' 定義に則って再帰的に計算する
x <- ts(rnorm(Tmax, sd=2)) # はじめは epslion が入っている
for(t in 2:Tmax) {
  x[t] <- x[t-1] + x[t] # 順に足し合わせていく
}
x |> as_tsibble() |> autoplot(value)

#' 同じ演算をする関数が用意されている
x <- ts(cumsum(rnorm(Tmax, sd=2))) # 逐次的に加算を行う関数
x |> as_tsibble() |> autoplot(value)
#' 書き方はいろいろあるので考えてみて下さい

#' 複数の系列を表示する場合
#' ホワイトノイズの生成と図示
z <- ts(replicate(K, # K回以下の関数を実行する
                  rnorm(Tmax, sd = 2)))
z |> as_tsibble() |>
    autoplot(value) # 同じグラフに重ね描きする
z |> as_tsibble() |>
    autoplot(value, show.legend = FALSE) # 凡例を削除 
z |> as_tsibble() |>
    autoplot(value, show.legend = FALSE) +
    facet_grid(key ~ .) # 各系列を個別に表示する
z |> as_tsibble() |>
    autoplot(value, show.legend = FALSE) +
    labs(title = expression(X[t] == epsilon[t]), # 数式で表示
         x = "Time", y = "Observations")

#' トレンドのあるホワイトノイズ
z <- ts(replicate(K,
                  rnorm(Tmax, sd = 2) -1 + 0.05*(1:Tmax)))
z |> as_tsibble() |>
    autoplot(value, show.legend = FALSE) +
    labs(title = expression(X[t] == -1 + 0.05 * t + epsilon[t]),
         x = "Time", y = "Observations")

#' ランダムウォーク
z <- ts(replicate(K,
                  cumsum(rnorm(Tmax, sd=2))))
z |> as_tsibble() |>
    autoplot(value, show.legend = FALSE) +
    labs(title = expression(X[t] == X[t-1] + epsilon[t]),
         x = "Time", y = "Observations")

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
x |> ts() |> as_tsibble() |> autoplot(value)

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
x |> ts() |> as_tsibble() |> autoplot(value)
#' @notes
#' 関数の引数として Tmax を指定する方法もあるが
#' 様々な分布のホワイトノイズを試したい場合もあるので
#' 生成の元となるノイズを渡す形で定義してある

#' データフレームを作成して表示
#' (後の演習で作成した時系列データを利用する)
ts_ar <- ts(replicate(K, my_ar(a = a, epsilon = rnorm(Tmax))))
ts_ar |> as_tsibble() |>
    autoplot(value, show.legend = FALSE) +
    labs(title = "AR(2)", x = "Time", y = "Observations")

#' MA(2)モデルのシミュレーション
b <- c(0.44, 0.08) # MAの係数
epsilon <- rnorm(Tmax)
x <- ts(double(Tmax))
x[1:2] <- epsilon[1:2]   
for(t in 3:Tmax) {
  x[t] <- b %*% epsilon[t-1:2] + epsilon[t]
}
x |> ts() |> as_tsibble() |> autoplot(value)

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
ts_ma <- ts(replicate(K, my_ma(b = b, epsilon = rnorm(Tmax))))
ts_ma |> as_tsibble() |>
    autoplot(value, show.legend = FALSE) +
    labs(title = "MA(2)", x = "Time", y = "Observations")

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
x |> ts() |> as_tsibble() |> autoplot(value)

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
ts_arma <- ts(replicate(K, my_arma(a = a, b = b, epsilon = rnorm(Tmax))))
ts_arma |> as_tsibble() |>
    autoplot(value, show.legend = FALSE) +
    labs(title = "ARMA(2,1)", x = "Time", y = "Observations")

#' @notes
#' ここでは時系列の生成過程を知ってもらうために自作の関数を作成したが，
#' 関数 stats::arima.sim() や stats::filter() などを利用することもできる
#' 上記のARMA(2,1)のシミュレーションは以下のようにして行うことができる
arima.sim(model = list(ar = c(0.8, -0.64), # ARの係数ベクトル
                       ma = c(-0.5)),      # MAの係数ベクトル
          n = Tmax, # 時系列の長さ
          innov = epsilon) # 乱数系列を渡す場合(渡さなければ標準正規乱数が使われる)
#' 詳細は '?stats::arima.sim()' を参照

#' ---------------------------------------------------------------------------

toy_acf <- arima.sim(model = list(ar = c(0.8, -0.64),
                                  ma = c(-0.5)),
                     n = 200) |>
  as_tsibble() |> ACF(value) 
toy_acf |> autoplot()

#' ---------------------------------------------------------------------------
#' @practice 自己相関

K <- 4 # 格子状に表示する時系列の数 (4つを並べて比較する)

#' AR(2)モデルの自己相関
ts_ar |> as_tsibble() |> ACF(value) # 計算結果はtsibbleクラスになる
ts_ar |> as_tsibble() |> ACF(value) |> autoplot()
ts_ar[,1:K] |> as_tsibble() |> ACF(value) |>
    autoplot() + # 格子状に並べるには facet を指定する
    facet_wrap(key ~ ., nrow = 2) +
    labs(title = "AR(2)")

#' MA(2)モデルの自己相関
ts_ma[,1:K] |> as_tsibble() |> ACF(value) |>
    autoplot() + 
    facet_wrap(key ~ ., nrow = 2) +
    labs(title = "MA(2)")

#' ARMA(2,1)モデルの自己相関
ts_arma[,1:K] |> as_tsibble() |> ACF(value) |>
    autoplot() + 
    facet_wrap(key ~ ., nrow = 2) +
    labs(title = "ARMA(2,1)")

#' ---------------------------------------------------------------------------
