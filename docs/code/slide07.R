### 第7講 サンプルコード
library(tidyverse)
library(ggfortify)

#' @exercise
#' 散布図 (正規化なし)
js_data <- read_csv("data/japan_social.csv")
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  select(!Pref) |> GGally::ggpairs()

js_data |> # 箱ひげ図．変数のばらつきに大きな違いがある
  pivot_longer(!Pref) |> # 都道府県名以外をまとめる
  ggplot(aes(x = name, y = value)) + # 既定値の name と value を利用
  geom_boxplot(aes(fill = name), show.legend = FALSE) # 変数ごとに色を変える

#' データの正規化
#' データの視覚化
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  mutate(across(where(is.double), \(x)signif(c(scale(x)), digits = 3))) |>
  select(!Pref) |> GGally::ggpairs()

js_data |> # 箱ひげ図．変数のばらつきをそろえる
  mutate(across(where(is.double), \(x)c(scale(x)))) |>
  pivot_longer(!Pref) |> # 都道府県名以外をまとめる
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
js_data <- read_csv("data/japan_social.csv")
#' 視覚化の例 (データの性質を見ておくことは重要)
js_data |> select(!Pref) |> GGally::ggpairs() # 散布図
js_data |>
  ggplot(aes(x = Agri, y = Forest)) +
  geom_point(colour = "blue") +
  geom_text(aes(y = Forest-1, label = Pref), colour = "brown")
#' @notes
#' 点と文字が被らないように座標をずらしている
#' package::ggrepel を利用すれば自動調整してくれる
js_data |>
  ggplot(aes(x = Agri, y = Forest)) +
  geom_point(colour = "blue") +
  ggrepel::geom_text_repel(aes(label = Pref), colour = "brown")
#' 主成分分析
js_pca0 <- js_data |> select(!Pref) |> prcomp() # 正規化なし
js_pca1 <- js_data |> select(!Pref) |> prcomp(scale. = TRUE) # 正規化あり
#' 正規化しない場合
summary(js_pca0) # 第1,2主成分でほとんど説明できることが示唆される
js_pca0$rotation # 負荷量が偏る傾向があり，各主成分はほぼ1つの変数に対応している
autoplot(js_pca0) #
#' 正規化した場合
summary(js_pca1)
js_pca1$rotation
autoplot(js_pca1)

#' MASS::UScereal
    uc_data <- MASS::UScereal |> select(where(is.double)) |>
      rownames_to_column(var = "product") |> as_tibble() # 行名を製品名を作成
library(MASS)
UC.data <- UScereal[sapply(UScereal, is.double)] 
#' 適当な方法で視覚化をすることを推奨
UC.pca0 <- prcomp(UC.data)
UC.pca1 <- prcomp(UC.data, scale.=TRUE)
summary(UC.pca0)
plot(UC.pca0) 
UC.pca0$rotation
summary(UC.pca1)
plot(UC.pca1)
UC.pca1$rotation

#' ---------------------------------------------------------------------------

## データフレームを分析
est <- prcomp( ~ x1の変数名 + ... + xpの変数名, data = データフレーム)
## 第1と第2主成分を利用した散布図
biplot(est)
## 第2と第3主成分を利用した散布図
biplot(est, choices = c(2,3))
## パラメタ s を変更 (既定値は1)
biplot(est, scale=0)

biplot(prcompの結果, choices=c(x軸成分,y軸成分)) # 主成分の指定

### 
### 練習問題 主成分分析の視覚化
### 

### 総務省統計局の都道府県別の社会生活統計指標データ
### - Pref: 都道府県名
### - Forest: 森林面積割合 (%) 2014年
### - Agri: 就業者１人当たり農業産出額(販売農家）(万円) 2014年
### - Ratio: 全国総人口に占める人口割合 (%) 2015年
### - Land: 土地生産性（耕地面積１ヘクタール当たり）(万円) 2014年
### - Goods: 商業年間商品販売額［卸売業＋小売業］（事業所当たり）(百万円) 2013年
js_data <- read.csv("data/japan_social.csv", row.names=1)
js_pca <- prcomp(js_data, scale.=TRUE) # データを正規化      
biplot(js_pca, # バイプロット(既定値: 第1 vs 第2主成分)
       cex=c(0.6, 0.8), # 文字の大きさを調整
       col=c("blue","red")) # 色の指定 (データ，変数)
## 第1主成分方向の正の向きには大都市をもつ県が集中
## 人口割合, 商品販売額および森林面積割合は１人当たり農業産出額とほぼ直交しており,
## 両者に関連はあまりないといえそう
## 第2主成分方向の正の向きには１人当たり農業産出額の上位県が集中

## 気になるデータをいくつか見てみる
## 並べ替えるためにデータフレームの列をベクトルとして取り出す
js_agri <- # １人当たり農業産出額に行名を付ける
    setNames(js_data$Agri, row.names(js_data))
head(sort(js_agri, decreasing=TRUE)) # 降順に並べてみる

## 第2,3主成分を確認する
biplot(js_pca, choices=c(2,3), # バイプロット(第2 vs 第3主成分)
       cex=c(0.6, 0.8), 
       col=c("blue","red")) 
## 第3主成分方向の負の向きには土地生産性の上位県が集中
js_land <- # 土地生産性に行名を付けて取出
    setNames(js_data$Land, row.names(js_data))
head(sort(js_land, decreasing=TRUE))
head(sort(js_land))
## 北海道の土地生産性は低いことがわかる

### UScereal
## 各変数の内容についてはhelpを参照
library(MASS)
UC.data <- UScereal[sapply(UScereal, is.double)]
UC.pca <- prcomp(UC.data, scale.=TRUE)
biplot(UC.pca, 
       cex=c(0.6, 0.8), 
       col=c("black","orange"))
biplot(UC.pca, choices=c(2,3),
       cex=c(0.6, 0.8), 
       col=c("black","orange"))

## 第1,2主成分得点で散布図を描く (上と比較せよ)
plot(PC2 ~ PC1, data=predict(UC.pca), type="n")
text(PC2 ~ PC1, data=predict(UC.pca),
     labels=rownames(UC.data), cex=0.5)
biplot(UC.pca,
       scale=0, # s=0とするとデータの座標は主成分得点となる
       cex=c(0.6, 0.8), 
       col=c("black","orange"))

js_pca <- prcomp(js_data, scale=TRUE)
js_pca$rot

summary(js_pca)

biplot(js_pca, scale=0,# バイプロット(既定値: 第1 vs 第2主成分)
	   xlim=xrange, ylim=yrange,
	   cex=c(1, 0.8), # 文字の大きさを調整
	   col=c("blue","red")) # 色の指定 (データ，変数)

biplot(js_pca, scale=0, choices=c(3,2), # バイプロット(第2 vs 第3主成分)
	   xlim=yrange, ylim=yrange,
	   cex=c(1, 0.8), 
	   col=c("blue","red"))

biplot(js_pca, scale=0, choices=c(1,2), # バイプロット(第1 vs 第2主成分)
	   xlim=c(-2,2), ylim=c(-2,2),
	   cex=c(1, 0.8), 
	   col=c("blue","red"))

biplot(js_pca, scale=0, choices=c(3,2), # バイプロット(第1 vs 第2主成分)
	   xlim=c(-2,2), ylim=c(-2,2),
	   cex=c(1, 0.8), 
	   col=c("blue","red"))
