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
    theme_update(text = element_text(family = "HiraMaruProN-W4"))
    label_family <- "HiraMaruProN-W4"    
} else {label_family <- NULL}

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
    GGally::ggscatmat(columns = 3:10, color = "地方名", alpha = .5) +
    theme(text = element_text(size = 8))

ja_data |>
    GGally::ggscatmat(columns = 11:19, color = "地方名", alpha = .5) +
    theme(text = element_text(size = 8))

ja_data |>
    GGally::ggscatmat(columns = 20:27, color = "地方名", alpha = .5) +
    theme(text = element_text(size = 8))

ja_fit <- ja_data |> 
  column_to_rownames(var = "県名") |>
  select(where(is.double)) |>
  prcomp(scale. = TRUE) # 主成分分析の実行
autoplot(ja_fit, # バイプロット
#         data = ja_data,
         label = TRUE, colour = "royalblue", # ラベルの表示
         label.repel = TRUE, # ラベルの表示を自動調整 (パッケージ ggrepel)
         label.family = label_family, label.size = 3,
         loadings = TRUE, loadings.colour = "orchid", # 負荷の表示
         loadings.label = TRUE, loadings.label.colour = "orchid", # 負荷ラベルの表示
         loadings.label.family = label_family, loadings.label.size = 3.5)

#' ---------------------------------------------------------------------------
#' @practice 主成分分析の考え方

#' 人工データ(2次元)による例
set.seed(123123)
n <- 100 # データ数
a <- c(1, 2)/sqrt(5) # 主成分負荷量(単位ベクトル)の設定 (適宜変更せよ)
toy_data <- # aのスカラー倍に正規乱数を重畳(行列として作成)
  runif(n,-1,1) %o% a + rnorm(2*n, sd=0.3) 
colnames(toy_data) <- paste0("x", 1:2) # 行列に列名(x1,x2)を付与
toy_data <- as_tibble(toy_data) 
p <-
  toy_data |>
  ggplot(aes(x = x1, y = x2)) +
  geom_point(colour = "blue", shape = 4) +
  geom_abline(slope = a[2]/a[1], # 真の傾き=真の負荷量のy成分/x成分
              intercept = 0, colour = "red") +  # 主成分負荷量の図示
  xlim(c(-2,2)) + ylim(c(-2,2)) # xy軸を揃えて直交関係を見易くする
  ## coord_fixed() # 縦横比を1としてもよいが，領域はデータに依存する
#' a方向に本質的な情報が集約されていることがわかる
print(p)
#' 主成分負荷量の推定
toy_pca <- prcomp(toy_data)
a_hat <- toy_pca$rotation[,1]
#' 第１主成分負荷量がaに非常に近い (乱数によっては符号が反対になることもある)
p +
  geom_abline(slope = a_hat[2]/a_hat[1], intercept = 0,
              colour = "orange", linetype = "dashed")
#' 主成分得点の計算
pc1 <- predict(toy_pca)[,1] # 第１主成分得点の取得
toy_pc1 <- pc1 %o% a_hat
colnames(toy_pc1) <- paste0("x", 1:2)
toy_pc1 <- as_tibble(toy_pc1)
p +
  geom_point(data = toy_pc1, # 第1主成分を元の散布図上で図示
             aes(x = x1, y = x2),
             colour = "purple", shape = 18)

#' ---------------------------------------------------------------------------

#' 中心化を行う
X <- scale(toy_data, scale = FALSE)
#' 詳細は '?base::scale' を参照
#' Gram 行列を計算する
G <- crossprod(X)
#' 固有値・固有ベクトルを求める
eigen(G) # 返り値 'values, vectors' を確認
#' 詳細は '?base::eigen' を参照

#' ---------------------------------------------------------------------------
#' @practice 第1主成分の求め方

#' 人工データ(3次元)による例
set.seed(242424)
n <- 50 # データ数
d <- 3
a <- rnorm(d)
(a <- a/sqrt(sum(a^2))) # 主成分負荷量(単位ベクトル)を生成
toy_data <- runif(n, -1, 1) %o% a + rnorm(d*n, sd = 0.1)
colnames(toy_data) <- paste0("x", 1:d) # 行列に列名(x1,...,xd)を付与
toy_data <- as_tibble(toy_data)
GGally::ggpairs(toy_data) # 散布図行列
toy_pca <- prcomp(toy_data)
pc1 <- predict(toy_pca)[,1] # 推定された第一主成分負荷量を取得
#' 以下の図は3次元のときのみ実行可能
s3d <- scatterplot3d::scatterplot3d(toy_data, type="h", asp=1,
                                    highlight.3d=TRUE)
s3d$points3d(pc1 %o% a, col="blue")

#' 主成分負荷量の推定を固有値分解と比較
toy_eigen <- eigen(crossprod(scale(toy_data,scale=FALSE))) # 固有値分解
toy_pca$rotation # 主成分負荷量
toy_eigen$vectors  # 固有ベクトル (符号を除いて主成分負荷量と一致)
toy_pca$sdev               # 主成分の標準偏差
sqrt(toy_eigen$values/(n-1)) # 固有値と主成分の標準偏差の関係

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 主成分分析

#' 都道府県別の社会生活統計指標の主成分分析

#' データの読み込み
js_data <- read_csv("data/japan_social.csv") |>
  mutate(Area = as_factor(Area)) # 地方区分を因子化
#' データの視覚化
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  select(!c(Pref,Area)) |>  # 都道府県名・地方区分は削除
  GGally::ggpairs() 
js_data |> # 箱ひげ図．変数のばらつきに大きな違いがある
  pivot_longer(!c(Pref,Area)) |> # 都道府県名・地方区分以外をまとめる
  ggplot(aes(x = name, y = value)) + # 既定値の name と value を利用
  geom_boxplot(aes(fill = name), show.legend = FALSE) # 変数ごとに色を変える
(js_pca <- js_data |>
   select(where(is.double)) |> # 実数値の列(都道府県名・地方区分)を抽出
   prcomp(scale. = TRUE)) # 変数のばらつきを規格化
#' 主成分方向から読み取れること:
#' 第1: 人の多さに関する成分(正の向きほど人が多い)
#' 第2: 農業生産力に関する成分(正の向きほど高い)
#' 第1，第2主成分得点による地図の作成
as_tibble(predict(js_pca)) |> # 主成分得点
  mutate(Pref = js_data[["Pref"]], # 都道府県名
         Area = js_data[["Area"]]) |> # 因子化した地方区分
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(aes(colour = Area), shape = 19, size = 2) + # 地方区分ごとに色を変える
  geom_text(aes(label = Pref), colour = "darkgray", 
            hjust = 0, nudge_x = 0.1, check_overlap = TRUE)
#' 寄与率の表示．次週詳しく説明する
summary(js_pca) 

#' @notes
#' 関数ggfortify::autoplot()の利用．次週詳しく説明する
autoplot(js_pca) + 
  geom_text(label = js_data[["Pref"]], # 都道府県名を追加 
            hjust = 0, nudge_x = 0.01) # 横にずらして表示
autoplot(js_pca, data = js_data, colour = "Area") + # 地方区分ごとに点の色を変える
  geom_text(label = js_data[["Pref"]], 
            hjust = 0, nudge_x = 0.01,
            check_overlap = TRUE) # 重なっているラベルを消す

#' 2変数での解析例 (AgriとLandを取り上げる，その他の組み合わせでも試みよ)
#' 多変数での視覚化は次週詳しく説明する
js_pca2 <- js_data |>
  select(c(Agri,Land)) |>
  prcomp(scale. = TRUE)
a_hat <- js_pca2$rotation[,1] # 主成分負荷量のベクトルを取得
js_pc1 <- predict(js_pca2)[,1] %o% a_hat # 第1主成分を行列・ベクトルで計算
colnames(js_pc1) <- paste0("x", 1:2) # 列名の付与
js_pc1 <- as_tibble(js_pc1) # データフレーム化
js_data |> # 数値列のみ中心化・規格化する
  mutate(across(where(is.double), \(x)c(scale(x)))) |>
  ggplot(aes(x = Agri, y = Land)) +
  geom_point(colour = "blue", shape = 4) +
  geom_abline(slope = a_hat[2]/a_hat[1], # 主成分負荷量(方向)の図示
              intercept = 0, colour = "orange") +  
  geom_point(data = js_pc1, # 第1主成分を元の散布図上で図示
             aes(x = x1, y = x2),
             colour = "purple", shape = 18) +
  coord_fixed() # 縦横比を1に指定

#' @notes
#' データフレームの特定の列のみ変換を行うには関数 dplyr::across() を利用する
#' 関数を組み合わせることで様々な条件が表現できる．
#' 具体的な例は '?dplyr::across' や '?tidyselect::where' を参照
#' 関数 base::scale() は行列を返すため，そのまま用いると列名が変更される
#' 上記の例ではベクトルを作成する関数 base::c() と 無名関数を用いてこれを解消している
#' 無名関数の代わりにラムダ式 '~c(scale(.))' などを用いることもできる
#' ---------------------------------------------------------------------------
