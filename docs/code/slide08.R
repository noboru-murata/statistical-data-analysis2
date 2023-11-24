### 第8講 サンプルコード
library(conflicted)
library(tidyverse)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(ggfortify)

#' ---------------------------------------------------------------------------
#' @practice 線形判別

#' 東京の気象データによる判別分析
library(MASS) # パッケージの読み込み
#' データの整理
tw_data <- read_csv("data/tokyo_weather.csv")
tw_subset  <- tw_data |> 
  filter(month %in% c(9,10)) |> # 9,10月のデータ
  select(temp, humid, month) |> # 気温・湿度・月を選択
  mutate(month = as_factor(month)) # 月を因子化
idx <- seq(1, nrow(tw_subset), by = 2) # データの分割(1つおき)
tw_train <- tw_subset[ idx,] # 訓練データ(推定に用いる)
tw_test  <- tw_subset[-idx,] # 試験データ(評価に用いる)

#' @notes
#' 関数lubridate::month()を用いれば月を文字列(因子)とすることもできる
#' mutate(month = month(month, label = TRUE))
#' 
#' ランダムに分割するには例えば以下のようにすれば良い
#' n <- nrow(tw_subset); idx <- sample(1:n, n/2)
#'
#' 以下のようにして最初に月を因子化することもできるが
#' read_csv("data/tokyo_weather.csv") |>
#'   mutate(month = as_factor(month))
#' 関数によっていは処理で使われない因子(例えば1月)も表示してしまうので注意が必要
#'
#' また複数の項目を因子化するには例えば以下のようにすればよい
#' mutate(across(c(year, month, day), as_factor))

#' 視覚化
tw_subset |> # 気温と湿度の散布図を作成
  ggplot(aes(x = temp, y = humid)) + 
  geom_point(aes(colour = month)) + # 月ごとに点の色を変える
  labs(x = "Temperature", y = "Humidity",
       title = "September & October")

#' 訓練データで線形判別関数(等分散性を仮定)を作成
tw_model <- month ~ temp + humid
tw_lda <- lda(formula = tw_model, data = tw_train)
tw_lda_fitted <- predict(tw_lda) # 判別関数によるクラス分類結果の取得
table(true = tw_train[["month"]], # 真値
      predict = tw_lda_fitted[["class"]]) # 予測値の比較(混同行列) 
tibble(true = tw_train[["month"]]) |>
  mutate(predict = tw_lda_fitted[["class"]]) |>
  View() # 真値と予測値の対応表を作成して直接比較
as_tibble(tw_lda_fitted[["x"]]) |> # リストから判別関数値(配列)を取得
  mutate(month = tw_train[["month"]]) |> # 月(真値)を追加
  ggplot(aes(x = LD1, fill = month)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  facet_grid(month ~ .) # 月ごとに表示(y方向に並べる)

#' 試験データによる評価
tw_lda_predict <- predict(tw_lda, newdata = tw_test) 
table(true = tw_test[["month"]], # 混同行列
      predict = tw_lda_predict[["class"]])
tibble(true = tw_test[["month"]]) |> # 比較表
  mutate(predict = tw_lda_predict[["class"]]) |>
  View() 
as_tibble(tw_lda_predict[["x"]]) |> # 判別関数値の視覚化
  mutate(month = tw_test[["month"]]) |> 
  ggplot(aes(x = LD1, fill = month)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  facet_grid(month ~ .) 

#' 推定された線形判別関数の図示
range_x <- range(tw_subset[["temp"]])  # 気温の値域
range_y <- range(tw_subset[["humid"]]) # 湿度の値域
grid_x <- pretty(range_x, 100) # 気温の値域の格子点を作成
grid_y <- pretty(range_y, 100) # 湿度の値域の格子点を作成
grid_xy <- expand.grid(temp = grid_x,
                       humid = grid_y) # 2次元の格子点を作成
tw_lda_grid <- predict(tw_lda, # 格子点上の判別関数値を計算
                       newdata = grid_xy)
#' 判別関数により予測されるラベルの図示
p <- as_tibble(grid_xy) |> 
  mutate(predict = tw_lda_grid[["class"]]) |>
  ggplot(aes(x = temp, y = humid)) +
  geom_tile(aes(fill = predict), alpha = 0.3) +
  labs(x = "Temperature", y = "Humidity",
       title = "Linear Discriminant Analysis")
print(p) # 判別境界の表示
p + # データ点の重ね描き
  geom_point(data = tw_subset,
             aes(x = temp, y = humid, colour = month))
#' 判別関数値の図示 (参考)
p2 <- as_tibble(grid_xy) |> 
  mutate(LD = tw_lda_grid[["x"]][,"LD1"]) |> # LD1列のみ利用
  ggplot(aes(x = temp, y = humid)) +
  geom_raster(aes(fill = LD), alpha = 0.5) +
  scale_fill_gradientn(colours=c("red","white","blue")) +
  labs(x = "Temperature", y = "Humidity",
       title = "Scores of Discriminant Variables")
print(p2) # 判別関数値の表示
p2 + # データ点の重ね描き
  geom_point(data = tw_subset,
             aes(x = temp, y = humid, colour = month))

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 2次判別

#' 東京の気象データによる判別分析
#' 前の練習問題で作成した 'tw_train', 'tw_test' を利用

#' 訓練データで2次判別関数(等分散性を仮定しない)を作成
#' 線形判別関数と同じモデル式を用いる
tw_qda <- qda(formula = tw_model, data = tw_train)
tw_qda_fitted <- predict(tw_qda) # 判別関数によるクラス分類結果の取得
table(true = tw_train[["month"]], # 真値
      predict = tw_qda_fitted[["class"]]) # 予測値の比較(混同行列) 
tibble(true = tw_train[["month"]]) |>
  mutate(predict = tw_qda_fitted[["class"]]) |>
  View() # 真値と予測値の対応表を作成して直接比較

#' 試験データによる評価
tw_qda_predict <- predict(tw_qda, newdata = tw_test) 
  table(true = tw_test[["month"]], # 混同行列
      predict = tw_qda_predict[["class"]])
tibble(true = tw_test[["month"]]) |> # 比較表
  mutate(predict = tw_qda_predict[["class"]]) |>
  View() 

#' 推定された2次判別関数の図示
tw_qda_grid <- predict(tw_qda, # 格子点上の判別関数値を計算
                       newdata = grid_xy)
#' 判別関数により予測されるラベルの図示
p <- as_tibble(grid_xy) |> 
  mutate(predict = tw_qda_grid[["class"]]) |>
  ggplot(aes(x = temp, y = humid)) +
  geom_tile(aes(fill = predict), alpha = 0.3) +
  labs(x = "Temperature", y = "Humidity",
       title = "Quadratic Discriminant Analysis")
print(p) # 判別境界の表示
p + # データ点の重ね描き
  geom_point(data = tw_subset,
             aes(x = temp, y = humid, colour = month))

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 多値判別

#' 東京の気象データによる判別分析
#' 3値判別のためのデータの整理
tw_subset3  <- tw_data |>
  filter(month %in% c(9,10,11)) |>
  select(temp, humid, month) |>
  mutate(month = as_factor(month))

#' 線形判別関数(3値)を作成
tw_lda3 <- lda(formula = tw_model, data = tw_subset3)
tw_lda3_fitted <- predict(tw_lda3) # 判別関数によるクラス分類結果の取得
table(true = tw_subset3[["month"]], # 真値
      predict = tw_lda3_fitted[["class"]]) # 予測値の比較(混同行列)

#' 推定された線形判別関数(3値)により予測されるラベルの図示
#' 格子点は再計算する必要がある
range_x <- range(tw_subset3[["temp"]])  # 気温の値域
range_y <- range(tw_subset3[["humid"]]) # 湿度の値域
grid_x <- pretty(range_x, 100) # 気温の値域の格子点を作成
grid_y <- pretty(range_y, 100) # 湿度の値域の格子点を作成
grid_xy <- expand.grid(temp = grid_x,
                       humid = grid_y) # 2次元の格子点を作成
tw_lda3_grid <- predict(tw_lda3, # 格子点上の判別関数値を計算
                        newdata = grid_xy)
as_tibble(grid_xy) |> 
  mutate(predict = tw_lda3_grid[["class"]]) |>
  ggplot(aes(x = temp, y = humid)) +
  geom_tile(aes(fill = predict), alpha = 0.3) +
  geom_point(data = tw_subset3,
             aes(x = temp, y = humid, colour = month)) +
  labs(x = "Temperature", y = "Humidity",
       title = "Multi-label Discriminant Analysis")

#' 3値判別の場合には2つの判別関数を構成するので
#' これを用いてデータ点の散布図を作成することができる
p <- bind_cols(tw_lda3_fitted[["x"]], # 判別関数値(LD1,LD2)
               tw_subset3["month"]) |> # データフレームとして抽出
  ggplot(aes(x = LD1, y = LD2)) + 
  geom_point(aes(colour = month)) # 月ごとに色を変更
print(p)

#' 関数geom_tile()が座標軸に沿った格子点を想定しているため
#' 判別関数値の散布図上で判別境界とデータ点を表示するには工夫が必要
#' ここでは関数geom_point()で代用した簡便な例を示す
p + geom_point(data = # 判別関数値と予測ラベルのデータフレームを作成
                 bind_cols(tw_lda3_grid[["x"]],
                           predict = tw_lda3_grid[["class"]]),
               aes(colour = predict), alpha = 0.2)

#' 12ヶ月分のデータを用いる
#' 数が多いのでサンプリングする
idx <- sample(nrow(tw_data), 100)
tw_subset12 <- slice(tw_data, idx) |>
  mutate(month = as_factor(month))
tw_lda12 <- lda(month ~ temp + solar + wind + humid,
             data = tw_subset12)
tw_lda12_fitted <- predict(tw_lda12)
table(true = tw_subset12[["month"]], # 混同行列
      predict = tw_lda12_fitted[["class"]])
bind_cols(tw_lda12_fitted[["x"]], # 判別関数値の散布図を作成
          tw_subset12["month"]) |>
  GGally::ggpairs(aes(colour = month))
#' 判別関数は説明変数の数までしか作成できないので，精度はあまり高くないことがわかる

#' 雨の有無を識別する例
tw_rain <- tw_data |>
  mutate(rain = factor(rain > 0), # 雨の有無でラベル化する
         month = as_factor(month)) # 月ごとの気候の違いの補正のため
tw_rain_lda <- lda(rain ~ temp + solar + wind + month,
                   data = tw_rain,
                   subset = idx) # 一部のデータで推定，12ヶ月分の例とは別の指定の仕方
tw_rain_lda_fitted <- predict(tw_rain_lda)
as_tibble(tw_rain_lda_fitted[["x"]]) |> # 判別関数値の視覚化
  mutate(rain = tw_rain[["rain"]][idx]) |> 
  ggplot(aes(x = LD1, fill = rain)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  facet_grid(rain ~ .) 

#' 全データを予測
tw_rain_lda_predict <- predict(tw_rain_lda, newdata = tw_rain) 
table(true = tw_rain[["rain"]][idx], # 推定に用いたデータの混同行列
      predict = tw_rain_lda_predict[["class"]][idx])
table(true = tw_rain[["rain"]][-idx], # 未知データに対する予測の混同行列
      predict = tw_rain_lda_predict[["class"]][-idx])
