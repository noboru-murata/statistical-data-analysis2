### 第3講 サンプルコード
library(tidyverse)

bw_data <- read_csv(file="data/wine.csv")  # データの読み込み
bw_fit <- lm(formula = LPRICE2 ~ . - VINT, # VINTを除く全て
             data = bw_data)

bw_fit # 推定結果の簡単な表示

#' @notes
#' 行列の転置を計算する関数t()と積を組み合わせてもよい
#'   crossprod(X,Y) = t(X) %*% Y
#' XY^T = (X^TY)^T を計算する関数 tcrossprod() もある
#'   tcrossprod(X,Y) = X %*% t(Y)
