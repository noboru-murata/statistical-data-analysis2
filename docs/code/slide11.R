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
