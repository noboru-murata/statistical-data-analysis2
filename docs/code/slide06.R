### 第6講 サンプルコード
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

#' 主成分分析の例
#' 表の作成
ja_data <- bind_cols(
  read_csv(file = "data/prefecture.csv",
           col_select = c(2,4)) |>
  set_names("県名", "地方名"), # 列の名称を"県名"と"地方名"に変更
  read_csv(file = "data/jpamenity.csv",
           col_select = !1:2) |> slice(-1) |>
  set_names(names(read_csv(file = "data/jpamenityitem.csv")))) # 簡略化した項目名に変更

ja_data |> View() # 左上ペインに表として表示

ja_data |>
  GGally::ggpairs(columns = 3:10,
                  legend = c(2,1), # 2行1列のグラフから凡例を作成
                  lower = list(continuous = GGally::wrap("points", alpha = .5),
                               mapping = aes(colour = 地方名))) +
  theme(text = element_text(size = 8))

ja_data |>
  GGally::ggpairs(columns = 11:19,
                  legend = c(2,1), 
                  lower = list(continuous = GGally::wrap("points", alpha = .5),
                               mapping = aes(colour = 地方名))) +
  theme(text = element_text(size = 8))

ja_data |>
  GGally::ggpairs(columns = 20:27,
                  legend = c(2,1), 
                  lower = list(continuous = GGally::wrap("points", alpha = .5),
                               mapping = aes(colour = 地方名))) +
  theme(text = element_text(size = 8))

ja_fit <- ja_data |> 
  column_to_rownames(var = "県名") |>
  select(where(is.double)) |>
  prcomp(scale. = TRUE) # 主成分分析の実行
autoplot(ja_fit, # バイプロット
         label = TRUE, colour = "royalblue", # ラベルの表示
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.family = jp_font, label.size = 3,
         loadings = TRUE, loadings.colour = "orchid", # 負荷の表示
         loadings.label = TRUE, loadings.label.colour = "darkorchid", # 負荷ラベルの表示
         loadings.label.family = jp_font, loadings.label.size = 3.5)
