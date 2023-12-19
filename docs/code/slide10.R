### 第10講 サンプルコード
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(tidyverse)
library(ggfortify)
library(GGally)
library(cluster)
library(ggdendro)

#' @exercise 実データによるクラスタ分析の例
#' 以下の例で用いる分析のための関数の詳細は今回・次回で解説
#' データの読み込み
js_data <- read_csv("data/japan_social.csv") |>
  mutate(Area = as_factor(Area))

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

#' ---------------------------------------------------------------------------
#' @practice 距離の計算

#' データの読み込み
js_data <- read_csv("data/japan_social.csv") |>
  mutate(Area = as_factor(Area))
js_df <- js_data |>
  column_to_rownames(var = "Pref") |> # 'Pref'を行名に変換
  select(-Area) # 距離計算に不要な地方名は除く
#' @notes
#' 'js_data' のまま扱うこともできるが，距離・類似度を計算する関数は
#' 'js_df' の形式(base::data.frame())を想定しているため用意しておく

#' ユークリッド距離とマンハッタン距離の計算
js_dist_euc <- dist(js_df, method = "euclidean")
js_dist_man <- dist(js_df, method = "manhattan")
js_daisy_euc <- daisy(js_df, metric = "euclidean")
js_daisy_man <- daisy(js_df, metric = "manhattan")
#' 両者が同じことを確認
as.matrix(js_dist_euc)[1:5,1:5]
as.matrix(js_daisy_euc)[1:5,1:5]

#' 正規化したユークリッド距離とマンハッタン距離の計算
js_dist_euc <- dist(scale(js_df), method = "euclidean")
js_dist_man <- dist(scale(js_df), method = "manhattan")
js_daisy_euc <- daisy(js_df, metric = "euclidean", stand = TRUE)
js_daisy_man <- daisy(js_df, metric = "manhattan", stand = TRUE)
#' 正規化の方法が異なることに注意
as.matrix(js_dist_man)[10:15,10:15]
as.matrix(js_daisy_man)[10:15,10:15]

#' 以下 daisy による正規化を用いる
#' 関東の都県同士の距離を表示しなさい
glimpse(js_daisy_euc) # 距離行列のもつ情報を見る
attr(js_daisy_euc, "Labels") # 県名を確認 ('attributes(js_daisy_euc)$Labels' でも良い)
as.matrix(js_daisy_euc)[8:14, 8:14]
as.matrix(js_daisy_man)[8:14, 8:14]
#' dist/dissimilarity オブジェクトは距離以外の様々な属性 (attributes) を持つ
#' glimpse(obj) : オブジェクトの構造(structure)を見る(関数strでも良い)
#' attributes(obj) : 属性を表示(変更)する
#' attr(obj,属性名) : 特定の属性を表示(変更)する

#' 大阪と四国の間の距離
as.matrix(js_daisy_euc)[27, 36:39, drop = FALSE] # 行列として表示
as.matrix(js_daisy_man)["Osaka", # 1行なので標準ではベクトルとして扱われる
                        c("Tokushima","Kagawa","Ehime","Kochi")]

#' ユークリッド距離とマンハッタン距離の散布図
p <- tibble( # 列名に特殊な文字(空白など)を含む場合は `` で囲む
  `Euclid dist.` = as.vector(js_daisy_euc),
  `Manhattan dist.` = as.vector(js_daisy_man)) |>
  ggplot(aes(x = `Euclid dist.`, y = `Manhattan dist.`)) +
  geom_point(colour = "blue")

p + geom_abline(slope = 1, intercept = 0, colour = "red") # 基準線を付けて表示
p + xlim(0,3) + ylim(0,3) # 原点近傍のみ表示
#' いくつか順序が入れ替わっていることがわかる

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 階層的クラスタリング

#' クラスタリングの実行
#' 正規化してユークリッド距離を測る
js_dist <- js_df |> scale() |> dist() # 'dist(scale(js_df))' でも良い
js_hclust <- js_dist |>
  hclust(method = "average") # 群平均法
js_hclust |> as.dendrogram() |>  
  ggdendrogram(rotate = FALSE, # 横向きにしない
               theme_dendro = FALSE) + # 'TRUE'とすれば無地
  labs(title = "Euclidean + Average",
       x = "Prefecture", y = "Height") +
  theme(axis.text.y = element_text(size = 9))
#' 少数のクラスタに分割してみる
k <- 5 # 分割数を指定
js_clust <- cutree(js_hclust, k = k) # デンドログラムを分割
js_pref <- rownames(js_df) # 県名の取得
for(i in 1:k){
  cat("=== cluster",i,"===\n")
  print(js_pref[js_clust==i])
}
#' @notes
#' 関数 stats::hclust では base R の関数 plot を利用して
#' 下記のような方法で簡便にクラスタの分割を表示できる
#' ggplot で同様なグラフを描く方法は後述
plot(js_hclust,
     hang = -1, # ラベルを揃えて表示
     cex = 0.8, # 文字のサイズを調整
     sub = "", xlab = "", main = "")
rect.hclust(js_hclust, k = k, border = "orange") 

#' 主成分分析を併用して表示
js_df |>
  prcomp() |> # 主成分分析
  autoplot(data = tibble(cluster = factor(js_clust)),
           colour = 'cluster',
           ## 主成分分析の図をクラスタ毎に色分けするためのデータを追加
           frame = TRUE, # クラスタ毎に枠を付ける
           frame.type = "convex", # 凸包 "convex"・楕円 "norm,t" が指定できる
           label = TRUE, # ラベルを付加
           label.repel = TRUE, # 重なりを回避(ラベルが消える場合もあるので注意)
           label.size = 3, # ラベルの大きさ
           label.show.legend = FALSE) # 凡例の中のアルファベットを除く

#' 最大クラスタを再評価
table(js_clust) # 最大を確認
m <- which.max(table(js_clust)) # 最大クラスタの番号を取り出す
js_df_sub <- js_df[js_clust==m,]
js_hclust_sub <- js_df_sub |>
  scale() |> dist() |> hclust(method = "average") 
js_hclust_sub |> as.dendrogram() |>  
  ggdendrogram(rotate = FALSE, theme_dendro = FALSE) +
  labs(title = "euclidean + average",
       x = "prefecture", y = "distance") +
  theme(axis.text.y = element_text(size = 9))
js_clust_sub <- cutree(js_hclust_sub, k = 4) # 最大クラスタをさらに分割
js_df_sub |>
  prcomp() |> # 主成分分析
  autoplot(data = tibble(cluster = factor(js_clust_sub)),
           colour = 'cluster',
           frame = TRUE, 
           frame.type = "convex", 
           label = TRUE,
           label.repel = TRUE,
           label.size = 3,
           label.show.legend = FALSE)

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 階層的クラスタリング

#' データの読み込み("omusubi.csv"を用いる)
om_data <- bind_cols( # 日本語表記・地方の情報を追加
  read_csv(file = "data/omusubi.csv"),
  read_csv(file = "data/prefecture.csv"))
#' 距離計算用のデータフレーム
om_df <- om_data |> 
  select(ume:etc,jp) |>
  set_names(c("梅","鮭","昆布","鰹","明太子","鱈子","ツナ","その他","県名")) |>
  column_to_rownames(var = "県名")
#' 日本語表示のための設定
if(Sys.info()["sysname"] == "Darwin") { # MacOSか調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraMaruProN-W4"))}
if(Sys.info()["sysname"] == "Darwin") { # MacOSか調べて日本語フォントを指定
  label_family <- "HiraMaruProN-W4"} else {label_family <- NULL}
#' データの散布図:
om_data |>
  select(ume:etc,jp,area_jp) |>
  set_names(c("梅","鮭","昆布","鰹","明太子","鱈子","ツナ","その他","県名","地方")) |>
  ggpairs(columns = 1:8,
          upper = list(continuous = "cor"),
          diag = list(continuous = "barDiag"),
          lower = list(continuous = wrap("points", size = 0.5),
                       mapping = aes(colour = 地方)))
#' 県別の人気比率:
om_data |>
  select(ume:etc,jp) |>
  set_names(c("梅","鮭","昆布","鰹","明太子","鱈子","ツナ","その他","県名")) |>
  pivot_longer(-県名) |>
  mutate(県名 = fct_rev(as_factor(県名)),
         name = as_factor(name)) |> # ggplot(aes(x = 県名, y = value)) +
  ggplot(aes(y = 県名, x = value)) +
  geom_bar(aes(fill = name),
           stat = "identity",
           position = position_stack(reverse=TRUE)) +
  labs(title = "おむすびの具 県別人気アンケート (2009)",
       x = "人気比率", fill = "具材") +
  theme(axis.text.y = element_text(size = 9))

#' Hellinger距離による階層的クラスタリング
om_agnes <- om_df |> sqrt() |> agnes()
#' @notes
#' 1/2乗してEuclid距離を計算すればHellinger距離に比例した量が得られる
#' 定数倍まで厳密に計算するのであれば '1/sqrt(2)*daisy(sqrt(om_df/100))'

#' デンドログラムの表示
om_agnes |> as.dendrogram() |>  
  ggdendrogram(rotate = TRUE, theme_dendro = FALSE) +
  labs(title = "おむすびの具人気アンケート",
       x = "県名", y = "距離") +
  theme(axis.text.y = element_text(size = 9))
#' クラスタ数7としてクラスタの表示
k <- 7
om_clust <- cutree(om_agnes, k = k)  # クラスタを作成
om_df |>
  prcomp() |>
  autoplot(data = tibble(cluster = factor(om_clust)),
           colour = 'cluster',
           label = TRUE,
           label.repel = TRUE,
           label.size = 3,
           label.family = label_family)
#' @notes
#' ggplotでは繁雑となるが下記のような図を描くこともできる
om_dendr <- dendro_data(om_agnes, type="rectangle") # ggplot用に変換
om_rect <- left_join(label(om_dendr),
                     tibble(label = rownames(om_df),
                            cluster = om_clust)) |>
  group_by(cluster) |>
  summarize(xmin = min(x)-0.3, xmax = max(x)+0.3)
om_ymax <- mean(sort(om_agnes$height, decreasing = TRUE)[k-0:1])
om_agnes |>
  as.dendrogram() |>
  ggdendrogram(rotate = TRUE, theme_dendro = FALSE) +
  labs(title = "クラスタの分割",
       x = "県名", y = "距離") +
  theme(axis.text.x = element_text(size = 9)) +
  geom_rect(data = om_rect,
            aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = om_ymax,
                fill = as_factor(cluster)),
            alpha = 0.3, show.legend = FALSE)
#' @notes
#' 'package::cluster' には base R 流の描画関数が含まれている
#' 例えば上記と同様なグラフは以下のようにして書ける
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraMaruProN-W4")}
plot(om_agnes, which.plot = 2, cex = 0.8,
     main = "Dendrogram of Omusubi Data")
clusplot(x = om_df,
         clus = cutree(om_agnes, k = k),
         labels = 2,
         col.p = "green", col.txt = "blue", col.clus = "orange", cex = 0.8,
         main = "Cluster of Omusubi Data")

#' ---------------------------------------------------------------------------
