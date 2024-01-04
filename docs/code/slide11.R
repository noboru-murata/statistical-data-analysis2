### 第11講 サンプルコード
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(tidyverse)
library(ggfortify)
library(cluster)
library(ggdendro)

#' ---------------------------------------------------------------------------
#' @practice 関数 kmeans による非階層的クラスタリング

#' データの読み込み (japan_social.csv)
js_data <- read_csv("data/japan_social.csv") 
js_df <- js_data |> # 距離計算用のデータフレーム
  column_to_rownames(var = "Pref") |> # 'Pref'を行名に変換
  select(-Area) # 地方名は除く
#' k-平均法の実行: 
set.seed(1234) # 必要に応じて初期値の乱数のシード値を指定する
k <- 7 # 分割数を指定
js_kmeans <- js_df |>
  scale() |> # 標準化(平均0 分散1)
  kmeans(centers = k, # クラスタ数
         nstart = 20) # 初期値を20回変更して試す
#' 各クラスター内の県名を表示
#' kmeansの返値 cluster の情報を利用する
#' kmeans.object$cluster または kmeans.object[["cluster"]] を用いる
for(i in 1:k) {
  cat("=== cluster",i,"===\n")
  which(js_kmeans[["cluster"]] == i) |> names() |> print()
}
#' 2次元でのクラスタ表示
#' 前回の主成分分析を利用したクラスタの表示とほぼ同様な記述
js_kmeans |>
  autoplot(data = scale(js_df), # kmeans の場合は元のデータの指定が必須
           ## (kmeansの返値は距離行列の情報しか持っておらず主成分の計算ができないため)
           frame = TRUE, # クラスタ毎に枠を付ける
           frame.type = "convex", # 凸包 "convex"・楕円 "norm,t" が指定できる
           label = TRUE, # ラベルを付加
           label.repel = TRUE, # 重なりを回避(ラベルが消える場合もあるので注意)
           label.size = 3, # ラベルの大きさ
           label.show.legend = FALSE) # 凡例の中のアルファベットを除く
#' @notes
#' 以下は cluster::clusplot() を用いる場合の例
clusplot(x = js_df, 
         clus = js_kmeans$cluster, # クラスタ番号
         stand = TRUE, # データの標準化を行う
         lines = 0, labels = 3, # 表示の指定
         main = NULL, sub = NULL, cex = 0.8, # タイトルなどの調整
         col.p = rainbow(k)[js_kmeans$cluster], # 虹色で色付け
         col.clus = "orange", shade = FALSE)	 # クラスタ囲みの指定

#' データの読み込み (omusubi.csv)
om_data <- bind_cols( # 日本語表記・地方の情報を追加
  read_csv(file = "data/omusubi.csv"),
  read_csv(file = "data/prefecture.csv"))
om_df <- om_data |>  # 距離計算用のデータフレーム
  select(ume:etc,jp) |>
  set_names(c("梅","鮭","昆布","鰹","明太子","鱈子","ツナ","その他","県名")) |>
  column_to_rownames(var = "県名")
#' 日本語表示のための設定
if(Sys.info()["sysname"] == "Darwin") { # MacOSか調べて日本語フォントを指定
  jp_font <- "HiraMaruProN-W4"
  theme_update(text = element_text(family = jp_font))
  par(family = jp_font)
} else {
  jp_font <- NULL # MacOSでない場合はフォントを指定しない
}
#' k-平均法の実行: 
k <- 6 # 6分割で分析
om_kmeans <- om_df |>
  sqrt() |> # Hellinger距離
  kmeans(centers = k, # クラスタ数
         nstart = 20) # 初期値を20回変更して試す
#' 各クラスター内の県名を表示
for(i in 1:k) {
  cat("=== cluster",i,"===\n")
  which(om_kmeans[["cluster"]] == i) |> names() |> print()
}
#' 2次元でのクラスタ表示
om_kmeans |>
  autoplot(data = sqrt(om_df), 
           frame = TRUE, 
           frame.type = "norm", # 楕円で囲む
           label = TRUE, 
           label.repel = TRUE, 
           label.size = 3, 
           label.family = jp_font, # 日本語フォントの指定 (不要な場合は削除)
           label.show.legend = FALSE) 
#' cluster::clusplot() による表示
clusplot(x = sqrt(om_df), 
         clus = om_kmeans$cluster, # クラスタ番号
         stand = FALSE, # データは標準化しない
         lines = 0, labels = 3, # 表示の指定
         main = NULL, sub = NULL, cex = 0.8, # タイトルなどの調整
         col.p = rainbow(k)[om_kmeans$cluster], # 虹色で色付け
         col.clus = "orange", shade = FALSE)	 # クラスタ囲みの指定

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 関数 pam による非階層的クラスタリング

#' k-medoids の実行
k <- 7
js_pam <- js_df |>  
  pam(stand = TRUE, # 正規化(平均0 絶対偏差1)
      k = k) # クラスタ数の指定
#' 各クラスター内の県名を表示
for(i in 1:k){
  cat("=== cluster",i,"===\n")
  which(js_pam[["clustering"]] == i) |> names() |> print()
}
#' 2次元でのクラスタ表示
js_pam |> 
  autoplot(frame = TRUE, 
           frame.type = "convex", 
           label = TRUE, 
           label.repel = TRUE, 
           label.size = 3, 
           label.show.legend = FALSE)
#' Manhattan距離だとどのようになるか試してみる
#' いくつかのクラスタでメンバが変わっている
js_df |>  
  pam(stand = TRUE, 
      metric = "manhattan",
      k = k) |>
  autoplot(frame = TRUE, 
           frame.type = "convex", 
           label = TRUE, 
           label.repel = TRUE, 
           label.size = 3, 
           label.show.legend = FALSE) 
#' cluster::clusplot() を用いる場合は kmeans とほぼ同様
clusplot(x = js_df,
         clus = js_pam$clustering,
         stand = TRUE,
         lines = 0, labels = 3, 
         main = NULL, sub = NULL, cex = 0.8,
         col.p = rainbow(k)[js_pam$clustering],
         col.clus = "orange", shade = FALSE)

#' k-medoids の実行
k <- 6
om_pam <- om_df |>
  sqrt() |>
  pam(k = k)
#' 各クラスター内の県名を表示
for(i in 1:k){
  cat("=== cluster",i,"===\n")
  which(om_pam[["clustering"]] == i) |> names() |> print()
}
#' 2次元でのクラスタ表示
om_pam |> 
  autoplot(frame = TRUE, 
           frame.type = "convex", 
           label = TRUE, 
           label.repel = TRUE, 
           label.size = 3, 
           label.family = jp_font, 
           label.show.legend = FALSE)
#' cluster::clusplot() による表示
#' クラスタリングの結果は om_pam$clustering に保管されている
clusplot(x = sqrt(om_df),
         clus = om_pam$clustering,
         stand = TRUE,
         lines = 0, labels = 3, 
         main = NULL, sub = NULL, cex = 0.8,
         col.p = rainbow(k)[om_pam$clustering],
         col.clus = "orange", shade = FALSE)

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 凝集係数による距離の検討

#' ユークリッド距離による階層的クラスタリング
js_agnes_euc <- agnes(js_df,
                      metric="euclidean", # データ距離
                      stand=TRUE,	    # 標準化
                      method="average")   # クラスタ距離
js_agnes_euc |> as.dendrogram() |>  
  ggdendrogram(rotate = FALSE, 
               theme_dendro = FALSE) + 
  labs(title = "Euclidean distance",
       x = "Prefecture", y = "Height") +
  theme(axis.text.y = element_text(size = 9))
#' マンハッタン距離による階層的クラスタリング
js_agnes_man <- agnes(js_df,
                      metric="manhattan",
                      stand=TRUE,
                      method="average")
js_agnes_man |> as.dendrogram() |>  
  ggdendrogram(rotate = FALSE, 
               theme_dendro = FALSE) + 
  labs(title = "Manhattan distance",
       x = "Prefecture", y = "Height") +
  theme(axis.text.y = element_text(size = 9))

#' 凝集係数の確認
js_agnes_euc[["ac"]]
js_agnes_man[["ac"]]
#' ユークリッド距離の方がわずかに良いことがわかる

#' データ毎の凝集係数の表示
tibble(x = js_agnes_euc[["height"]],
       y = length(js_agnes_euc[["height"]]):1) |>
  ggplot() +
  #' 各枝のheightと最大値で矩形を描く
  geom_rect(aes(xmin = x, xmax = max(x),
                ymin = y, ymax = y+1), 
            fill = "orange", alpha = 0.6) + # 塗り潰し色と透明度を指定
  #' y軸にラベルを表示する
  scale_y_continuous(breaks = length(js_agnes_euc[["order.lab"]]):1,
                     expand = expansion(add = 0.5),
                     labels = js_agnes_euc[["order.lab"]]) +
  labs(title = "Euclidean distance") +
  theme(axis.text.y = element_text(size = 9))
tibble(x = js_agnes_man[["height"]],
       y = length(js_agnes_man[["height"]]):1) |>
  ggplot() +
  geom_rect(aes(xmin = x, xmax = max(x),
                ymin = y, ymax = y+1), 
            fill = "orange", alpha = 0.6) + 
  scale_y_continuous(breaks = length(js_agnes_man[["order.lab"]]):1,
                     expand = expansion(add = 0.5),
                     labels = js_agnes_man[["order.lab"]]) +
  labs(title = "Manhattan distance") +
  theme(axis.text.y = element_text(size = 9))

#' @notes
#' bannerplot の情報を整理するための関数を定義してもよい
my_bannerplot <- function(object, # agnesの返値
                          fill = "orange", alpha = 0.6, ...) {
  p <- tibble(x = object[["height"]],
              y = length(object[["height"]]):1) |>
    ggplot() +
    geom_rect(aes(xmin = x, xmax = max(x),
                  ymin = y, ymax = y+1), 
              fill = fill, alpha = alpha) + 
    scale_y_continuous(breaks = length(object[["order.lab"]]):1,
                       expand = expansion(add = 0.5),
                       labels = object[["order.lab"]])
  p
}
my_bannerplot(js_agnes_euc,
             fill = "red", alpha = 0.6)

#' 一部のデータの距離が大きいと凝集係数は大きくなりがち (理由を考えてみよう)
#' 北海道，東京，宮崎，鹿児島を除いて再計算する 
#' 返値の要素を抽出する場合にはパイプ(|>)が使えない場合があるので注意
agnes(js_df |> slice(-c(1,13,45,46)),
      metric = "euclidean",
      stand = TRUE,
      method = "average")[["ac"]]
agnes(js_df |> slice(-c(1,13,45,46)),
      metric = "manhattan",
      stand = TRUE,
      method = "average")[["ac"]]
#' いずれにせよユークリッド距離の方が凝集係数は大きいことがわかる
#' 個別の係数の確認
js_df |> slice(-c(1,13,45,46)) |>
  agnes(metric = "euclidean",
        stand = TRUE,
        method = "average") |>
  my_bannerplot() + # @notes で定義した関数を利用
  labs(title = "Euclidean distance") +
  theme(axis.text.y = element_text(size = 9))
js_df |> slice(-c(1,13,45,46)) |>
  agnes(metric = "manhattan",
        stand = TRUE,
        method = "average") |>
  my_bannerplot() +
  labs(title = "Manhattan distance") +
  theme(axis.text.y = element_text(size = 9))

#' @notes
#' graphics系の関数を利用する場合は以下のようにすればよい
#' デンドログラムの表示
plot(js_agnes_euc, which.plots=2, 
     main="euclidean") 
plot(js_agnes_man, which.plots=2, 
     main="manhattan") 
#' データ毎の凝集係数の表示
plot(js_agnes_euc, which.plots=1, # banner plotの表示
     nmax.lab=50,   # 表示するラベルの上限 (標準は40)
     max.strlen=5,  # 表示するラベルの文字数の上限
     main="euclidean")
plot(js_agnes_man, which.plots=1,
     nmax.lab=50,  
     max.strlen=5,
     main="manhattan")

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice シルエット係数によるクラスタ数の検討

#' クラスタ数 4-10 で平均シルエット係数を確認
om_df_hel <- om_df |> sqrt() # Hellinger距離を計算しやすくデータフレームを用意
for(k in 4:10){
  cat(pam(om_df_hel, k = k)$silinfo$avg.width,
      " (k = ", k, ")\n", sep="")
}
#' pam(om_df_hel, k = k)[["silinfo"]][["avg.width"]] と書いても良い
#' k=7,8,9 (上位3つ) のシルエット係数を視覚化
for(k in 7:9) {
  p <- pam(om_df_hel, k = k) |>
    silhouette() |>
    autoplot() +
    labs(title = paste("k =", k))
  print(p) # ggplotはfor文内では明示的にprintする必要がある
}

#' 悪いシルエット係数が少ないという意味で k=8 が良さそう
k <- 8
om_df_hel |>
  pam(k = k) |>
  autoplot(frame = TRUE, 
           frame.type = "convex", 
           label = TRUE, 
           label.repel = TRUE, 
           label.size = 3, 
           label.family = jp_font, 
           label.show.legend = FALSE)
#' 同様な描画は以下でも可能
om_pam <- om_df_hel |>
  pam(k = k)
plot(om_pam,
     which.plot = 1, # cluster::clusplot のオプションを参考
     stand = TRUE,
     lines = 0, labels = 3, 
     main = "", sub = NULL, cex = 0.8, # タイトルと文字の大きさの調整
     col.p = rainbow(k)[om_pam$clustering], # クラスタ番号ごとに色付け
     col.clus = "orange", shade = FALSE) # クラスタを楕円で表示

#' Euclid距離による分析
#' k = 5-10 で検証
for(k in 5:10) {
  foo <- pam(om_df, k = k)
  p <- foo |>
    silhouette() |>
    autoplot() +
    labs(title = paste("k =", k, "(Silhouette coef. =",
                       round(foo$silinfo$avg.width, digits = 3), ")"))
  print(p) 
}
#' 悪いシルエット係数が少ないという意味で k=7 が良さそう
k <- 7
om_df |>
  pam(k = k) |>
  autoplot(frame = TRUE, 
           frame.type = "norm", 
           label = TRUE, 
           label.repel = TRUE, 
           label.size = 3, 
           label.family = jp_font, 
           label.show.legend = FALSE)

#' @notes
#' 階層的クラスタリングでもシルエット係数を計算することができる
om_agnes <- agnes(om_df_hel)
om_agnes |> as.dendrogram() |>
  ggdendrogram()
silhouette(cutree(om_agnes, k = k), 
           daisy(om_df_hel)) |> # 距離行列が必要
  autoplot()
#' シルエット係数のグラフはクラスタ毎に降順(大きいものが上)に並べ替えられている
#' グラフに合わせた要素名を取り出すには例えば以下のようにすれば良い
silhouette(cutree(om_agnes, k = k), daisy(om_df_hel)) |>
  as_tibble() |>
  mutate(perfecture=rownames(om_df_hel)) |>
  arrange(desc(cluster), desc(sil_width))

#' ---------------------------------------------------------------------------
