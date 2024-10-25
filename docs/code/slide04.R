### 第4講 サンプルコード
library(tidyverse)
library(gt)
library(gtsummary)
if(Sys.info()["sysname"] == "Darwin") { # MacOSか調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraMaruProN-W4"))}

#' @exercise 東京の気候データを用いた例

#' データの読み込み
tw_subset <- read_csv("data/tokyo_weather.csv") |>
  filter(month == 9) |> # 9月のデータの抽出
  mutate(date = date(paste(year,month,day,sep="-")), .before = 1) |>
  select(-c(year,month,day,day_of_week)) |>
  set_names(c("日付","気温","降雨","日射","降雪","風向","風速","気圧","湿度","雲量"))

#' モデル式
tw_formula1 <- 気温 ~ 気圧
tw_formula2 <- 気温 ~ 日射
tw_formula3 <- 気温 ~ 気圧 + 日射
tw_formula4 <- 気温 ~ 気圧 + 日射 + 湿度
tw_formula5 <- 気温 ~ 気圧 + 日射 + 雲量

#' 推定
tw_lm1 <- lm(tw_formula1, data = tw_subset, y = TRUE)
tw_lm2 <- lm(tw_formula2, data = tw_subset, y = TRUE)
tw_lm3 <- lm(tw_formula3, data = tw_subset, y = TRUE)
tw_lm4 <- lm(tw_formula4, data = tw_subset, y = TRUE)
tw_lm5 <- lm(tw_formula5, data = tw_subset, y = TRUE)

tw_subset |> View() # 左上ペインに表として表示

#' 関連データの散布図
tw_subset |>
  select(気温,気圧,日射,湿度,雲量) |>
  GGally::ggpairs()

#' モデル1の推定結果
tw_subset |>
  ggplot(aes(x = 気圧, y = 気温)) +
  geom_point(colour = "brown", shape = 20) +
  geom_smooth(method = lm, se = FALSE, colour = "blue")

#' モデル2の推定結果
tw_subset |>
  ggplot(aes(x = 日射, y = 気温)) +
  geom_point(colour = "brown", shape = 20) +
  geom_smooth(method = lm, se = FALSE, colour = "blue")

#' モデル3の推定結果
if(Sys.info()[["sysname"]] == "Darwin") par(family = "HiraginoSans-W4")
s3d <- scatterplot3d::scatterplot3d( 
                        as.data.frame(tw_subset[c("気圧","日射","気温")]), # x,y,z の順
                        #' tw_subset[c("気圧","日射","気温")], # tibble のままでも動くが warning が出る
                        type = "p", # plotの種類: "p"点，"l"線，"h"足付き
                        pch = 16,   # 点の種類 (?points 参照)
                        angle = 45, # xy平面の見る方向 (適宜調整)
                        zlim = c(20,35),
                        color = "brown",
                        #' xlab="気圧", ylab="日射", zlab="気温",
                        #'    highlight.3d=TRUE # 高さ(z)ごとに色を変える
                        )
s3d$plane3d(
      tw_lm3, col = "blue", # 回帰式の定める平面の追加
      draw_polygon = TRUE, # 平面の塗り潰しの設定
      polygon_args = list(col=rgb(0,0,1,0.1)))

#' 観測値とあてはめ値の比較
tw_subset |>
  mutate(モデル1 = fitted(tw_lm1),    # モデルごとに予測値をデータフレームに追加
         モデル2 = fitted(tw_lm2),
         モデル3 = fitted(tw_lm3),
         モデル4 = fitted(tw_lm4),
         モデル5 = fitted(tw_lm5)) |>
  pivot_longer(starts_with("モデル"), # モデルをラベルとして予測値をまとめる
               names_to = "model", values_to = "fitted") |>
  ggplot(aes(x = 気温, y = fitted)) + # 気温の実測値をx軸，予測値をy軸で表示
  geom_abline(slope = 1, intercept = 0, colour = "red") + # 基準線
  geom_point(aes(colour = model, shape = model)) + # 予測値をモデル別に表示
  labs(y = "あてはめ値") +
  xlim(22,32) + ylim(22,32) + theme(legend.position = c(.88,.15))

#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x) |>
    modify_column_hide(columns = c(p.value,conf.low)) |>
    modify_column_unhide(columns = std.error) |>
    add_glance_table(include = c("r.squared","adj.r.squared")) }
tw_gt <- 
  tbl_merge(
    tbls = list(
      my_gts(tw_lm1),
      my_gts(tw_lm2),
      my_gts(tw_lm3),
      my_gts(tw_lm4),
      my_gts(tw_lm5)),
    tab_spanner = c("モデル1","モデル2","モデル3","モデル4","モデル5")) |>
  modify_table_body( ~ .x |>
                       arrange(factor(variable,
                                      levels = c("気圧","日射","湿度","雲量"))))

tw_gt # Viewer ペインに表示

#' @exercise 人工データによる推定量の性質の確認

set.seed(987) # 乱数のシード値を設定
x_obs <- tibble(x0 = 1, x1 = c(1,3,5,7)) # 説明変数の観測値
epsilon <- rnorm(nrow(x_obs), sd = 0.5) # 誤差項の生成
beta <- c(2, -3) # 回帰係数
toy_data <- x_obs |> # 目的変数の観測値を追加
  mutate(y = as.vector(as.matrix(x_obs) %*% beta) + epsilon)
toy_lm <- lm(y ~ x1, data = toy_data) # 回帰係数の推定
coef(toy_lm) # 回帰係数の取得
summary(toy_lm) # 分析結果の概要の表示

#' @notes
#' 上記の例では行列とベクトルの積を用いているが
#' 以下のように説明変数・目的変数を個別に作成してもよい
x_obs <- c(1,3,5,7) # 説明変数の観測値
epsilon <- rnorm(length(x_obs), sd = 0.5) # 誤差項の生成
y_obs <- 2 - 3 * x_obs + epsilon # 目的変数の観測値
toy_data <- tibble(x1 = x_obs, y = y_obs) # データフレームの作成

#' 関数 stats::lm() による推定結果の診断プロット
tw_lm6 <- lm(気温 ~ 気圧 + 日射 + 降雨, data = tw_subset)
#' 関数 ggfortify::autoplot() を利用する
#' 必要であれば 'install.packages("ggfortify")' を実行
library(ggfortify)
autoplot(tw_lm6)
#' 診断プロットは1から6まで用意されており 1,2,3,5 がまとめて表示される
#' 個別に表示する場合は 'autoplot(tw_lm6, which = 1)' のように指定する
#' 詳細は '?ggfortify::autoplot.lm' を参照
