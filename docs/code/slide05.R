### 第5講 サンプルコード
library(tidyverse)
library(gt)
library(gtsummary)
library(ggfortify) # 診断プロットのため
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
    modify_column_hide(columns = c(p.value)) |>
    add_glance_table(include = c(r.squared,adj.r.squared)) }
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

#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x) |>
    modify_column_hide(columns = c(p.value)) |>
    add_glance_table(include = c(statistic,p.value)) }
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

#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x, intercept = TRUE) |>                   # 標準の表
    modify_column_hide(columns = c(conf.low)) |>           # 信頼区間を非表示
    modify_column_unhide(columns = c(std.error,statistic))} # 標準誤差とt統計量を表示
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
                                      levels = c("(Intercept)","気圧","日射","湿度","雲量"))))

tw_gt # Viewer ペインに表示

#' 診断プロット (モデル4)
autoplot(tw_lm4)

#' 診断プロット (モデル5)
autoplot(tw_lm5)

#' 9,10月のデータでモデルを構築し，8,11月のデータを予測
#' データの整理
tw_data <- read_csv("data/tokyo_weather.csv")
tw_train <- tw_data |> # モデル推定用データ
  filter(month %in% c(9,10)) # %in% は集合に含むかどうかを判定
tw_test  <- tw_data |> # 予測用データ
  filter(month %in% c(8,11))
#' モデルの構築
tw_model <- temp ~ solar + press # モデルの定義 
tw_lm <- lm(tw_model, data = tw_train) # モデルの推定
tidy(tw_lm)   # 回帰係数の評価
glance(tw_lm) # モデルの評価
#' あてはめ値と予測値の計算
tw_train_fitted <- augment(tw_lm, newdata = tw_train) # あてはめ値
tw_test_fitted  <- augment(tw_lm, newdata = tw_test)  # 予測値

#' 予測結果を図示
bind_rows(tw_train_fitted, tw_test_fitted) |> # 2つのデータフレームを結合
  mutate(month = as_factor(month)) |> # 月を因子化して表示に利用
  ggplot(aes(x = .fitted, y = temp)) +
  geom_point(aes(colour = month, shape = month)) + # 月ごとに色と形を変える
  geom_abline(slope = 1, intercept = 0, # 予測が完全に正しい場合のガイド線
              colour = "gray") +
  labs(x = "fitted", y = "observed")

#' @notes
#' 関数 lubridate::month() を用いると月を文字列のラベルとすることもできる
bind_rows(tw_train_fitted, tw_test_fitted) |>
  mutate(month = month(month, label = TRUE)) |> # 文字にする場合
  ggplot(aes(x = .fitted, y = temp)) +
  geom_point(aes(colour = month)) + # 月ごとに色を変える
  geom_abline(slope = 1, intercept = 0, # 予測が完全に正しい場合のガイド線
              colour = "gray") +
  labs(x = "fitted", y = "observed")
#' この場合は順序付きの因子になるので，'shape' を利用すると警告が出る

#' factor属性の与え方
X <- c("A", "S", "A", "B", "D")
Y <- c(85, 100, 80, 70, 30)
toy_data1 <- tibble(X, Y)
toy_data2 <- toy_data1 |> # 因子化
  mutate(X2 = factor(X))  # 関数as_factor()を用いてもよい
glimpse(toy_data2) # 作成したデータフレームの素性を見る(pillar::glimpse())
toy_data3 <- toy_data2 |> # 順序付き(levels)の因子化
  mutate(X3 = factor(X, levels=c("S","A","B","C","D")))
glimpse(toy_data3) # toy_data2とはfactorの順序が異なる
toy_data4 <- toy_data2 |>
  mutate(Y2 = factor(Y > 60)) # 条件による因子化
glimpse(toy_data4) # 条件の真偽で2値に類別される

#' モデルの探索
adv_data <- read_csv('https://www.statlearning.com/s/Advertising.csv')
summary(lm(sales ~ radio, data = adv_data))
summary(lm(sales ~ TV + radio, data = adv_data))
summary(lm(sales ~ TV + radio + newspaper, data = adv_data))
summary(adv_init <- lm(sales ~ TV * radio * newspaper, data = adv_data))
adv_opt <- step(adv_init) # 最大のモデルから削減増加による探索
summary(adv_opt) # 探索された(準)最適なモデルの確認
