### 第7講 サンプルコード
library(conflicted) # 名前の衝突に対応するパッケージ
library(tidyverse)
conflicts_prefer( # 衝突する可能性のあるものは tidyverse の関数を優先
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(ggfortify)

#' @exercise
#' 散布図 (正規化なし)
js_data <- read_csv("data/japan_social.csv") |>
  mutate(Area = as_factor(Area))
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  select(where(is.double)) |> GGally::ggpairs()

js_data |> # 箱ひげ図．変数のばらつきに大きな違いがある
  pivot_longer(where(is.double)) |> # 実数値をまとめる
  ggplot(aes(x = name, y = value)) + # 既定値の name と value を利用
  geom_boxplot(aes(fill = name), show.legend = FALSE) # 変数ごとに色を変える

#' データの正規化
js_data |> # 正規化したデータ(有効数字3桁)で表示する
  mutate(across(where(is.double), \(x)signif(c(scale(x)), digits = 3))) |>
  View()

#' データの視覚化
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  mutate(across(where(is.double), \(x)c(scale(x)))) |>
  select(where(is.double)) |> GGally::ggpairs()

js_data |> # 箱ひげ図．変数のばらつきをそろえる
  mutate(across(where(is.double), \(x)c(scale(x)))) |>
  pivot_longer(where(is.double)) |> # 実数値をまとめる
  ggplot(aes(x = name, y = value)) + # 既定値の name と value を利用
  geom_boxplot(aes(fill = name), show.legend = FALSE) # 変数ごとに色を変える

#' ---------------------------------------------------------------------------
#' @practice 寄与率・累積寄与率

#' 総務省統計局の都道府県別の社会生活統計指標データ
#' - Pref: 都道府県名
#' - Forest: 森林面積割合 (%) 2014年
#' - Agri: 就業者１人当たり農業産出額(販売農家）(万円) 2014年
#' - Ratio: 全国総人口に占める人口割合 (%) 2015年
#' - Land: 土地生産性（耕地面積１ヘクタール当たり）(万円) 2014年
#' - Goods: 商業年間商品販売額［卸売業＋小売業］（事業所当たり）(百万円) 2013年
#' - Area: 地方区分
js_data <- read_csv("data/japan_social.csv") |>
  mutate(Area = as_factor(Area))
#' 視覚化の例 (データの性質を見ておくことは重要)
js_data |> select(where(is.double)) |> GGally::ggpairs() # 散布図
js_data |>
  ggplot(aes(x = Agri, y = Forest)) +
  geom_point(colour = "blue") +
  geom_text(aes(label = Pref),
            colour = "brown",
            vjust = 1, nudge_y = 0.0, check_overlap = TRUE)

#' @notes
#' 点と文字が被らないように座標をずらし，密集したところは消している
#' package::ggrepel を利用すれば自動調整してくれる
js_data |>
  ggplot(aes(x = Agri, y = Forest)) +
  geom_point(colour = "blue") +
  ggrepel::geom_text_repel(aes(label = Pref), colour = "brown")

#' 主成分分析
js_pca_raw <- prcomp(js_data[-c(1,7)]) # 正規化なし
js_pca <- prcomp(js_data[-c(1,7)], scale. = TRUE) # 正規化あり
#' 'Pref' を行の名前(ラベル)として扱う場合は以下のようにすれば良い
js_pca_raw <- js_data |>
  column_to_rownames(var = "Pref") |> # 'Pref'を行名に変換
  select(where(is.double)) |> 
  prcomp() # 正規化なし
js_pca <- js_data |> 
  column_to_rownames(var = "Pref") |> 
  select(where(is.double)) |> 
  prcomp(scale. = TRUE) # 正規化あり

#' 正規化しない場合
summary(js_pca_raw) # 第1,2主成分でほとんど説明できることが示唆される
js_pca_raw$rotation # 負荷量が偏る傾向があり，各主成分はほぼ1つの変数に対応している
broom::tidy(js_pca_raw, "d") |> # summary(js_pca_raw) を tibble 形式で取得
  ggplot(aes(x = PC, y = percent)) + # 各主成分(PC)ごとに寄与率(percent)を表示
  geom_bar(stat = "identity")
broom::tidy(js_pca_raw, "d") |> 
  ggplot(aes(x = PC, y = cumulative)) + # 各主成分(PC)ごとに累積寄与率(cumlative)を表示
  geom_bar(stat = "identity")
autoplot(js_pca_raw, scale = 0, label = TRUE) # 最も簡単な散布図の表示
#' 正規化した場合
summary(js_pca)
js_pca$rotation
broom::tidy(js_pca, "d") |> 
  ggplot(aes(x = PC, y = percent)) + # 寄与率(percent)を表示
  geom_bar(stat = "identity")
broom::tidy(js_pca, "d") |> 
  ggplot(aes(x = PC, y = cumulative)) + # 累積寄与率(cumlative)を表示
  geom_bar(stat = "identity")
autoplot(js_pca, scale = 0, label = TRUE)

#' @notes
#' 寄与率を表示するためには関数stats::screeplot()を利用してもよい
#' 詳細は '?screeplot' を参照
screeplot(js_pca)

#' MASS::UScereal
uc_data <- MASS::UScereal |> 
  rownames_to_column(var = "product") |> as_tibble() # 行名を製品名に
#' 適当な方法で視覚化をすることを推奨
#' 正規化なしの分析
uc_pca_raw <- uc_data |>
  column_to_rownames(var = "product") |>
  select(where(is.double)) |>
  prcomp() 
summary(uc_pca_raw)
uc_pca_raw$rotation
broom::tidy(uc_pca_raw, "d") |> 
  ggplot(aes(x = PC, y = percent)) + geom_bar(stat = "identity")
broom::tidy(uc_pca_raw, "d") |> 
  ggplot(aes(x = PC, y = cumulative)) + geom_bar(stat = "identity")
autoplot(uc_pca_raw, scale = 0, label = TRUE) 
#' 正規化ありの分析
uc_pca <- uc_data |>
  column_to_rownames(var = "product") |>
  select(where(is.double)) |>
  prcomp(scale. = TRUE)
summary(uc_pca)
uc_pca$rotation
broom::tidy(uc_pca, "d") |> 
  ggplot(aes(x = PC, y = percent)) + geom_bar(stat = "identity")
broom::tidy(uc_pca, "d") |> 
  ggplot(aes(x = PC, y = cumulative)) + geom_bar(stat = "identity")
autoplot(uc_pca, scale = 0, label = TRUE)

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 主成分分析の視覚化

#' 総務省統計局の都道府県別の社会生活統計指標データ
#' 簡素な biplot の指定
autoplot(js_pca, # 既定値では第1 vs 第2主成分
         label = TRUE, # ラベル(都道府県名)の表示
         loadings = TRUE, # 主成分負荷の表示
         loadings.label = TRUE) # 変数名の表示
#' 指定可能なオプションの例
autoplot(js_pca, 
         shape = 19, size = 1, colour = "gray", # データ点の修飾
         label = TRUE, label.colour = "blue", label.size = 3, # 文字の修飾
         loadings = TRUE, loadings.colour = "orange", # 主成分負荷の修飾
         loadings.label = TRUE, 
         loadings.label.colour = "brown", loadings.label.size = 4) # 変数名の修飾
#' ラベル表示を工夫した例
autoplot(js_pca, data = js_data, colour = "Area", # 地方区分ごとに色を変える
         shape = 19, size = 1, 
         loadings = TRUE, loadings.colour = "orange",
         loadings.label = TRUE, 
         loadings.label.colour = "brown", loadings.label.size = 4) +
  ggrepel::geom_text_repel(aes(label = Pref, colour = Area),
                           max.overlaps = 18) +
  theme(legend.position = "none")
#' 第1主成分方向の正の向きには大都市をもつ県が集中
#' 人口割合, 商品販売額および森林面積割合は１人当たり農業産出額とほぼ直交しており,
#' 両者に関連はあまりないといえそう
#' 第2主成分方向の正の向きには１人当たり農業産出額の上位県が集中

#' 気になるデータをいくつか見てみる
#' 農業産出額を昇順に並べる
js_data |> arrange(Agri) |> View()
#' 第2,3主成分を確認する (第2 vs 第3主成分のbiplot)
autoplot(js_pca, x = 2, y = 3,
         label = TRUE, loadings = TRUE, loadings.label = TRUE)

#' 第3主成分方向の負の向きには土地生産性の上位県が集中
#' 土地生産性を降順に並べる
js_data |> arrange(desc(Land)) |> View()
#' 北海道の土地生産性は低いことがわかる

#' UScereal
autoplot(uc_pca,
         data = uc_data, colour = "mfr", # メーカー毎に色付け
         shape = 19, size = 1, 
         loadings = TRUE, loadings.colour = "orange",
         loadings.label = TRUE, 
         loadings.label.colour = "brown", loadings.label.size = 4) +
  ggrepel::geom_text_repel(aes(label = product, colour = mfr),
                           size = 3, max.overlaps = 40) +
  theme(legend.position = "none")
autoplot(uc_pca, x = 2, y = 3, # 第2 vs 第3
         data = uc_data, colour = "mfr", 
         shape = 19, size = 1, 
         loadings = TRUE, loadings.colour = "orange",
         loadings.label = TRUE, 
         loadings.label.colour = "brown", loadings.label.size = 4) +
  ggrepel::geom_text_repel(aes(label = product, colour = mfr),
                           size = 3, max.overlaps = 40) +
  theme(legend.position = "none")

#' 第1,2主成分得点で散布図を描く (上と比較せよ)
as_tibble(predict(uc_pca)) |>
  mutate(product = uc_data[["product"]],
         mfr = uc_data[["mfr"]]) |>
  ggplot(aes(x = PC1, y = PC2, label = product, colour = mfr)) +
  geom_point() +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 40) +
  theme(legend.position = "none")
autoplot(uc_pca, scale = 0, # 'scale=0'とするとデータの座標は主成分得点となる
         data = uc_data, colour = "mfr", # メーカー毎に色付け
         shape = 19, size = 1, 
         loadings = TRUE, loadings.colour = "orange",
         loadings.label = TRUE, 
         loadings.label.colour = "brown", loadings.label.size = 4) +
  ggrepel::geom_text_repel(aes(label = product, colour = mfr),
                           size = 3, max.overlaps = 40) +
  theme(legend.position = "none")

#' ---------------------------------------------------------------------------
