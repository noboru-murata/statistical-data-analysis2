### 第6講 サンプルコード
library(conflicted) # 名前の衝突に対応するパッケージ
library(tidyverse)
conflicts_prefer( # 衝突する可能性のあるものは tidyverse の関数を優先
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
  )
library(GGally)
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
#' 人口関連データの整理
ja_data <-
  bind_cols(
    ## 都道府県名と地方名の取得
    read_csv(file = "data/prefecture.csv",
             col_select = c(2,4)) |>
    set_names("都道府県名", "地方区分"),     # 列の名称を"都道府県名"と"地方区分"に変更
    ## データの取得
    read_csv(file = "data/jpamenity.csv",
             col_select = !1:2) |> # 不要な列を読み込まない
    slice(-1) |>                   # 不要な行を削除
    set_names(names(read_csv(file = "data/jpamenityitem.csv"))) # 簡略化した項目名に変更
  ) |> 
  select(1:10) |> # 人口関連のみ利用
  mutate(地方区分 = as_factor(地方区分)) # 地方区分を出現順に因子化

ja_data |> View() # 左上ペインに表として表示

ja_data |>
  ggpairs(columns = 3:10,  # 都道府県名・地方区分を除く
          legend = c(2,1), # 2行1列のグラフから凡例を作成
          upper = list(continuous = wrap("cor", size = 3.5)),
          lower = list(continuous = wrap("points", alpha = .5),
                       mapping = aes(colour = 地方区分名))) +
  theme(text = element_text(size = 8))

ja_pca <- ja_data |> 
  select(-1:2) |> # 人口関連データのみ
  prcomp(scale. = TRUE) # 主成分分析の実行
autoplot(ja_pca, # バイプロット
         asp = 1, # 縦横比を設定
         data = ja_data,
         colour = "地方区分", # 地方ごとに色付け
         label = TRUE, # ラベルの表示
         label.label = "都道府県名",
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.family = jp_font, label.size = 3,
         loadings = TRUE, loadings.colour = "orchid", # 負荷の表示
         loadings.label = TRUE, loadings.label.colour = "darkgray", # 負荷ラベルの表示
         loadings.label.family = jp_font, loadings.label.size = 4)

#' データの読み込み
js_data <-
  bind_cols(
    read_csv(file = "data/prefecture.csv",
             col_select = c(2,4)) |>
    set_names("県名","地方区分"), # 列の名称を"県名"と"地方区分"に変更
    read_csv(file = "data/japan_social.csv",
             col_select = 2:6) |> 
    set_names("森林面積割合","農業算出額","人口割合","土地生産性","商品販売額")
  )|> # 簡略化した項目名に変更
  mutate(地方区分 = as_factor(地方区分)) # 地方区分を出現順に因子化

js_data |> View() # 左上ペインに表として表示

#' データの視覚化
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  ggpairs(columns = 3:7, # 都道府県名・地方区分は除く
          legend = c(2,1), # 2行1列のグラフから凡例を作成
          upper = list(continuous = wrap("cor", size = 3.5)),
          lower = list(continuous = wrap("points", alpha = .5),
                       mapping = aes(colour = 地方区分)))

js_data |> # 箱ひげ図．変数のばらつきに大きな違いがある
  pivot_longer(!c(県名,地方区分)) |>   # 都道府県名・地方区分以外をまとめる
  mutate(name = as_factor(name)) |>  # 出現順にx軸を並べるために因子化する
  ggplot(aes(x = name, y = value)) + # 既定値の name と value を利用
  geom_boxplot(aes(fill = name), show.legend = FALSE) # 変数ごとに色を変える

js_data |> 
  pivot_longer(where(is.double)) |> # 都道府県名・地方区分以外をまとめる
  mutate(name = as_factor(name)) |>
  ggplot(aes(x = value)) + 
  geom_density(aes(fill = name), alpha = 0.4) + # 変数ごとに色を変える
  labs(fill = NULL)

#' データの標準化
js_data_std <-
  js_data |>
  mutate(across(where(is.double), \(x)signif(c(scale(x)), digits = 3)))

js_data_std |> View() # 左上ペインに表として表示

js_data |> # 箱ひげ図．変数のばらつきをそろえる
  mutate(across(where(is.double), \(x)c(scale(x)))) |>
  pivot_longer(where(is.double)) |> # 都道府県名・地方区分以外をまとめる
  mutate(name = as_factor(name)) |>
  ggplot(aes(x = name, y = value)) + # 既定値の name と value を利用
  geom_boxplot(aes(fill = name), show.legend = FALSE) # 変数ごとに色を変える

js_data |> 
  mutate(across(where(is.double), \(x)c(scale(x)))) |>
  pivot_longer(where(is.double)) |> # 都道府県名・地方区分以外をまとめる
  mutate(name = as_factor(name)) |>
  ggplot(aes(x = value)) + 
  geom_density(aes(fill = name), alpha = 0.4) + # 変数ごとに色を変える
  labs(fill = NULL)

js_pca <-
  js_data |>
  select(where(is.double)) |> # 都道府県名・地方区分を除いて主成分分析を実行
  prcomp(scale. = TRUE) # 変数のばらつきを規格化

js_pca |>
  tidy("loadings") |>
  pivot_wider(names_from = "PC", names_prefix = "PC") |>
  View() # 左上ペインに表として表示

autoplot(js_pca, # バイプロット
         asp = 1, # 縦横比を設定
         data = js_data, colour = "地方区分", # 地方ごとに色付け
         label = TRUE, label.label = "県名",   # ラベルの表示
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.family = jp_font, label.size = 3,
         loadings = TRUE, loadings.colour = "orchid", # 負荷の表示
         loadings.label = TRUE, loadings.label.colour = "darkgray", # 負荷ラベルの表示
         loadings.label.family = jp_font, loadings.label.size = 4)

js_pca <- js_data |>
     select(where(is.double)) |> # 都道府県名・地方区分を除いて主成分分析を実行
     prcomp(scale. = FALSE) # 標準化しない

js_pca |>
  tidy("loadings") |>
  pivot_wider(names_from = "PC", names_prefix = "PC") |>
  View() # 左上ペインに表として表示

autoplot(js_pca, # バイプロット
         asp = 1, # 縦横比を設定
         data = js_data, colour = "地方区分", # 地方ごとに色付け
         label = TRUE, label.label = "県名",   # ラベルの表示
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.family = jp_font, label.size = 3,
         loadings = TRUE, loadings.colour = "orchid", # 負荷の表示
         loadings.label = TRUE, loadings.label.colour = "darkgray", # 負荷ラベルの表示
         loadings.label.family = jp_font, loadings.label.size = 4)
