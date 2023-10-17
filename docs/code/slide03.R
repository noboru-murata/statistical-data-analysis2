### 第3講 サンプルコード
library(tidyverse)

#' ---------------------------------------------------------------------------
#' @practice 回帰係数の推定

#' 広告費と売上データによる回帰分析

#' データの読み込み
adv_data <- read_csv("https://www.statlearning.com/s/Advertising.csv")
#' 散布図を描くために package::GGally を利用
library(GGally) # パッケージの読み込み
ggpairs(adv_data, columns=2:5) # 散布図

#' TVの宣伝費で売上を説明
(adv_lm1 <- lm(sales ~ TV, data = adv_data))
adv_data |>
  ggplot(aes(x = TV, y = sales)) +
  geom_point(colour="orange") +
  geom_smooth(method=lm,se=FALSE)
#' adv_lm1 を利用する方法は後述

#' radioの宣伝費で売上を説明
(adv_lm2 <- lm(sales ~ radio, data = adv_data))
adv_data |>
  ggplot(aes(x = radio, y = sales)) +
  geom_point(colour="orange") +
  geom_smooth(method=lm,se=FALSE)

#' 両者の宣伝費で売上を説明
(adv_lm3 <- lm(sales ~ TV + radio, data = adv_data))
#' 3次元の散布図を描くために package::scatterplot3d を利用
#' 必要であれば install.packages("scatterplot3d") を実行
library(scatterplot3d) # パッケージの読み込み
s3d <- scatterplot3d( 
  adv_data[c("TV","radio","sales")], # x,y,z の順
  type = "p", # plotの種類: "p"点，"l"線，"h"足付き
  pch = 16,# 点の種類 (?points 参照)
  angle = 45, # xy平面の見る方向 (適宜調整)
  highlight.3d = TRUE # 高さ(z)ごとに色を変える
)
s3d$plane3d(adv_lm3, col = "blue", # 回帰式の定める平面の追加
            draw_polygon = TRUE, # 平面の塗り潰しの設定
            polygon_args = list(col = rgb(0,0,1,0.2)))

#' 東京の気候データによる回帰分析
#' データの読み込み
tw_data <- read_csv("data/tokyo_weather.csv")

#' モデルの推定: 8月の"気温"を目的変数，"日射量・気圧"を説明変数とする
tw_model <- temp ~ solar + press # モデル式の定義 
class(tw_model)                  # formula class であることを確認
(tw_lm <- lm(tw_model, # 回帰係数の推定
             data = tw_data, 
             subset =  month==8)) # 8月のデータの抽出
(tw_df <- as_tibble(model.frame(tw_lm))) # 推定に用いたデータフレームの抽出
ggpairs(tw_df) # 散布図

#' 散布図と回帰式の定める平面の描画(3次元プロット)
s3d <- scatterplot3d( 
  tw_df[c("solar","press","temp")], # x,y,z の順
  type = "p", # plotの種類: "p"点，"l"線，"h"足付き
  pch = 16,# 点の種類 (?points 参照)
  angle = 30, # xy平面の見る方向 (適宜調整)
  highlight.3d = TRUE # 高さ(z)ごとに色を変える
)
s3d$plane3d(tw_lm, col = "blue", # 回帰式の定める平面の追加
            draw_polygon = TRUE, # 平面の塗り潰しの設定
            polygon_args = list(col = rgb(0,0,1,0.1)))

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 最小二乗推定量の性質

#' 広告費と売上データ

#' 回帰係数と正規方程式の解の一致
(beta <- coef(adv_lm3))        # 推定された回帰係数
X <- model.matrix(adv_lm3)     # デザイン行列
Y <- model.frame(adv_lm3)[[1]] # 目的変数 (データフレームの1列目のベクトル)
solve(crossprod(X)) %*% crossprod(X, Y) # 正規方程式の解

#' あてはめ値と残差の直交性
yhat <- fitted(adv_lm3) # あてはめ値
ehat <- resid(adv_lm3)  # 残差
yhat %*% ehat           # 直交すれば内積はO

#' 回帰式が標本平均を通ること
colMeans(X) %*% beta # 説明変数の標本平均のあてはめ値
mean(Y)              # 目的変数の標本平均

#' @notes
#' 関数 stats::model.matrix() の返値は matrix class である
#' 関数 stats::model.frame() の返値は data.frame class であり
#' model.frame(...)[[k]] は data.frame ではなく vector になる
#' tibble(...)[[k]] は vector ではなく1列の tibble になるので注意
#' 関数 base::crossprod() の引数は厳密に vector/matrix であることを要請する
#' 例えば adv_data は全て数値なので行列に変換が可能で計算できる
crossprod(adv_data) # データフレームのままなので計算できない
crossprod(as.matrix(adv_data)) # 計算できる

#' 東京の気候データ

#' 回帰係数と正規方程式の解の一致
(beta <- coef(tw_lm))          # 推定された回帰係数
(X <- model.matrix(tw_lm))     # デザイン行列
(Y <- model.frame(tw_lm)[[1]]) # 目的変数 (データフレームの1列目のベクトル)
solve(crossprod(X)) %*% crossprod(X, Y) # 正規方程式の解

#' あてはめ値と残差の直交性
yhat <- fitted(tw_lm) # あてはめ値
ehat <- resid(tw_lm)  # 残差
yhat %*% ehat          # 直交すれば内積はO(に近い値)となる

#' 回帰式が標本平均を通ること
colMeans(X) %*% beta # 説明変数の標本平均のあてはめ値
mean(Y)              # 目的変数の標本平均

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 残差の分解

#' 広告費と売上データ
summary(adv_lm3)
Y <- model.frame(adv_lm3)[[1]]           # 目的変数の取得
(Sy <- sum((Y-mean(Y))^2))               # 目的変数のばらつき
(S <- sum(resid(adv_lm3)^2))             # 残差のばらつき
(Sr <- sum((fitted(adv_lm3)-mean(Y))^2)) # 回帰のばらつき
S+Sr # Sy と同じになっている

#' 東京の気候データ
tw_model # モデルの確認
#' 以下は目的変数を推定結果に含める方法
tw_lm <- lm(tw_model,
            data = tw_data, 
            subset = month == 8, # 8月のデータの抽出
             y = TRUE) # 目的変数をyとして返すように指定
Y <- with(tw_lm,y)                     # 目的変数の取得 (tw_lm$y でも可)
(Sy <- sum((Y-mean(Y))^2))             # 目的変数のばらつき
(S <- sum(resid(tw_lm)^2))             # 残差のばらつき
(Sr <- sum((fitted(tw_lm)-mean(Y))^2)) # 回帰のばらつき
S+Sr # Sy と同じになっている

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 決定係数によるモデルの比較

#' 広告費と売上データ

#' モデルの比較
adv_model1 <- sales ~ TV
adv_model2 <- sales ~ radio
adv_model3 <- sales ~ TV + radio
adv_lm1 <- lm(adv_model1, data=adv_data, y=TRUE)
adv_lm2 <- lm(adv_model2, data=adv_data, y=TRUE)
adv_lm3 <- lm(adv_model3, data=adv_data, y=TRUE)
summary(adv_lm1)$adj.r.squared # 自由度調整済み決定係数
summary(adv_lm2)$adj.r.squared # (model1より減少)
summary(adv_lm3)$adj.r.squared # (model1より上昇)

#' 予測値と実測値の比較
adv_data |>
  mutate(model1 = fitted(adv_lm1),    # モデルごとに予測値をデータフレームに追加
         model2 = fitted(adv_lm2),
         model3 = fitted(adv_lm3)) |>
  pivot_longer(starts_with("model"), # モデルをラベルとして予測値をまとめる
               names_to = "model", values_to = "fitted") |>
  ggplot(aes(x = sales, y = fitted)) + # 実測値をx軸，予測値をy軸で表示
  geom_abline(slope = 1, intercept = 0, colour = "red") + # 基準線
  geom_point(aes(colour = model, shape = model)) + # 予測値をモデル別に表示
  labs(y = "fitted values")

#' 東京の気候データ

#' モデルの比較
tw_subset <- tw_data |> filter(month == 8) # 8月のデータの抽出
tw_model1 <- temp ~ solar
tw_model2 <- temp ~ solar + press
tw_model3 <- temp ~ solar + press + cloud
tw_lm1 <- lm(tw_model1, data=tw_subset, y=TRUE)
tw_lm2 <- lm(tw_model2, data=tw_subset, y=TRUE)
tw_lm3 <- lm(tw_model3, data=tw_subset, y=TRUE)
summary(tw_lm1)$adj.r.squared # 自由度調整済み決定係数
summary(tw_lm2)$adj.r.squared # (model1より上昇)
summary(tw_lm3)$adj.r.squared # (model2より上昇)

#' 予測値と実測値の比較
tw_subset |>
  mutate(model1 = fitted(tw_lm1),    # モデルごとに予測値をデータフレームに追加
         model2 = fitted(tw_lm2),
         model3 = fitted(tw_lm3)) |>
  pivot_longer(starts_with("model"), # モデルをラベルとして予測値をまとめる
               names_to = "model", values_to = "fitted") |>
  ggplot(aes(x = temp, y = fitted)) + # 気温の実測値をx軸，予測値をy軸で表示
  geom_abline(slope = 1, intercept = 0, colour = "red") + # 基準線
  geom_point(aes(colour = model, shape = model)) + # 予測値をモデル別に表示
  labs(y = "fitted values")

#' ---------------------------------------------------------------------------
