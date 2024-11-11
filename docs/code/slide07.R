### 第7講 サンプルコード
library(conflicted) # 名前の衝突に対応するパッケージ
library(tidyverse)
conflicts_prefer( # 衝突する可能性のあるものは tidyverse の関数を優先
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(broom)
library(gt)
library(gtsummary)
library(ggfortify)
library(ggrepel)
#' macOSのための日本語表示の設定
if(Sys.info()["sysname"] == "Darwin") { # macOSか調べる
  jp_font <- "HiraMaruProN-W4"
  theme_update(text = element_text(family = jp_font))
} else {jp_font <- NULL}

#' @exercise
#' 散布図 (標準化なし)
js_data <- read_csv("data/japan_social.csv") |>
  mutate(Area = as_factor(Area))
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  GGally::ggpairs(columns = 2:6, # 都道府県名・地方区分は除く
                  legend = c(2,1), # 2行1列のグラフから凡例を作成
                  lower = list(continuous = GGally::wrap("points", alpha = .5),
                               mapping = aes(colour = Area)))

js_data |> # 箱ひげ図．変数のばらつきに大きな違いがある
  pivot_longer(where(is.double)) |>  # 実数値をまとめる
  mutate(name = as_factor(name)) |>  # 列の順番どおりboxplotを並べる
  ggplot(aes(x = name, y = value)) + # 既定値の name と value を利用
  geom_boxplot(aes(fill = name), show.legend = FALSE) # 変数ごとに色を変える

#' データの標準化
js_data |> # 標準化したデータ(有効数字3桁)で表示する
  mutate(across(where(is.double), \(x)signif(c(scale(x)), digits = 3))) |>
  View()

#' データの視覚化
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  mutate(across(where(is.double), \(x)c(scale(x)))) |>
  GGally::ggpairs(columns = 2:6, # 都道府県名・地方区分は除く
                  legend = c(2,1), # 2行1列のグラフから凡例を作成
                  lower = list(continuous = GGally::wrap("points", alpha = .5),
                               mapping = aes(colour = Area)))

js_data |> # 箱ひげ図．変数のばらつきをそろえる
  mutate(across(where(is.double), \(x)c(scale(x)))) |>
  pivot_longer(where(is.double)) |>  # 実数値をまとめる
  mutate(name = as_factor(name)) |>  # 列の順番どおりboxplotを並べる
  ggplot(aes(x = name, y = value)) + # 既定値の name と value を利用
  geom_boxplot(aes(fill = name), show.legend = FALSE) # 変数ごとに色を変える
