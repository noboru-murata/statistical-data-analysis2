### 第6講 資料

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

#| fig-width: 6
ja_data |>
  ggpairs(columns = 3:10,  # 都道府県名・地方区分を除く
          legend = c(2,1), # 2行1列のグラフから凡例を作成
          upper = list(continuous = wrap("cor", size = 3.5)),
          lower = list(continuous = wrap("points", alpha = .5),
                       mapping = aes(colour = 地方区分))) +
  theme(text = element_text(size = 8))

#| fig-width: 6
ja_pca <- ja_data |> 
  select(!1:2) |> # 人口関連データのみ
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
