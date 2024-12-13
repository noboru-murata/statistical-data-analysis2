### 第10講 サンプルコード
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(tidyverse)
library(gt)
library(ggfortify)
library(GGally)
library(cluster)
library(ggdendro)

#' @exercise 実データによるクラスタ分析の例
#' 以下の例で用いる分析のための関数の詳細は今回・次回で解説
#' データの読み込み
js_data <- read_csv("data/japan_social.csv") |>
  mutate(Area = as_factor(Area))

js_data |> View() # 左上ペインに表として表示

#' データの視覚化
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  ggpairs(columns = 2:6,
          upper = list(continuous = "cor"),
          diag = list(continuous = wrap("densityDiag", alpha = 0.4),
                      mapping = aes(colour = Area)),
          lower = list(continuous = wrap("points", size = 1),
                       mapping = aes(colour = Area)))

js_data |> 
  column_to_rownames(var = "Pref") |> 
  select(where(is.double)) |> 
  pam(1) |> # クラスタリングしない
  autoplot(data = js_data,
           colour = "Area",
           label = TRUE,
           label.repel = TRUE,
           label.show.legend = FALSE) +
  theme(legend.position = c(.9,.3))

#' クラスタ分析
js_data |> 
  column_to_rownames(var = "Pref") |> 
  select(where(is.double)) |> 
  pam(6) |>
  autoplot(frame = TRUE,
           frame.type = "convex",
           label = TRUE,
           label.repel = TRUE,
           label.show.legend = FALSE) +
  theme(legend.position = c(.9,.2))

js_data <- bind_cols(
  read_csv(file="data/japan_social.csv"),
  read_csv(file="data/prefecture.csv"))
js_kanto <- js_data |> 
  slice(8:14) |>
  select(jp,2:6) |>
  set_names(c("都道府県名","森林面積割合","農業産出額","人口割合","土地生産性","商品販売額"))

js_kanto |> View() # 左上ペインに表として表示
