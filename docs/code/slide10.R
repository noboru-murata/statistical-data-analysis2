### 第10講 資料

### 第10講 資料

#| include: false
#' @exercise 実データによるクラスタ分析の例
#' 以下の例で用いる分析のための関数の詳細は今回・次回で解説
#' データの読み込み
js_data <-
  read_csv("data/japan_social2.csv",
           col_types = list(地方区分 = col_factor()))

js_data |> View() # 左上ペインに表として表示

#| fig-width: 6.4
#' データの視覚化
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  ggpairs(columns = 3:7,
          legend = c(2,1), # 2行1列のグラフから凡例を作成
          upper = list(continuous = wrap("cor", size = 3.5)),
          diag = list(continuous = wrap("densityDiag", alpha = .5),
                      mapping = aes(colour = 地方区分)),
          lower = list(continuous = wrap("points", alpha = .5),
                       mapping = aes(colour = 地方区分))) +
  theme(strip.text = element_text(size = 8),
        axis.text = element_text(size = 5))

#| fig-width: 7
js_data |> 
  column_to_rownames(var = "都道府県名") |> 
  select(where(is.double)) |> 
  pam(1) |> # クラスタリングしない
  autoplot(data = js_data,
           asp = 1,
           colour = "地方区分",
           label = TRUE,
           label.repel = TRUE,
           label.show.legend = FALSE)

#| fig-width: 7
#' クラスタ分析
js_data |> 
  column_to_rownames(var = "都道府県名") |> 
  select(where(is.double)) |> 
  pam(6) |>
  autoplot(asp = 1,
           frame = TRUE,
           frame.type = "convex",
           label = TRUE,
           label.repel = TRUE,
           label.show.legend = FALSE)

#| include: false
js_kanto <-
  js_data |>
  slice(8:14) |>
  select(-地方区分)

js_kanto |> View() # 左上ペインに表として表示
