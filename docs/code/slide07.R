### 
### 第7講 サンプルコード
###

## 散布図 (正規化なし)
js_data <- read.csv("data/japan_social.csv", row.names=1)
pairs(js_data, col="orchid") # 座標に注意

boxplot(js_data, col="orchid") # 箱ひげ図．変数のばらつきに大きな違いがある

## データの視覚化
pairs(scale(js_data), col="blue") # いくつかの変数は相関が強い

boxplot(scale(js_data), col="blue") # 箱ひげ図．変数のばらつきに大きな違いがある

### 
### 練習問題 寄与率・累積寄与率
###

### 総務省統計局の都道府県別の社会生活統計指標データ
### - Pref: 都道府県名
### - Forest: 森林面積割合 (%) 2014年
### - Agri: 就業者１人当たり農業産出額(販売農家）(万円) 2014年
### - Ratio: 全国総人口に占める人口割合 (%) 2015年
### - Land: 土地生産性（耕地面積１ヘクタール当たり）(万円) 2014年
### - Goods: 商業年間商品販売額［卸売業＋小売業］（事業所当たり）(百万円) 2013年
js_data <- read.csv("data/japan_social.csv", row.names=1)
## 視覚化の例 (データの性質を見ておくことは重要)
plot(js_data,col="blue") # 散布図
plot(Forest ~ Agri, data=js_data, col="blue") # 特定の要素
text(Forest -1 ~ Agri, data=js_data, # 県名を付与
     labels=rownames(js_data), col="orange")
## 主成分分析
js_pca0 <- prcomp(js_data) # 正規化なし
js_pca1 <- prcomp(js_data, scale.=TRUE) # 正規化あり
## 正規化しない場合
summary(js_pca0) # 第1,2主成分でほとんど説明できることが示唆される
plot(js_pca0) # 分散の棒グラフを表示 (寄与率の定数倍)
js_pca0$rotation # 負荷量が偏る傾向があり，各主成分はほぼ1つの変数に対応している
## 正規化した場合
summary(js_pca1)
plot(js_pca1)
js_pca1$rotation

### UScereal
## 各変数の内容についてはhelpを参照
library(MASS)
UC.data <- UScereal[sapply(UScereal, is.double)] 
## 適当な方法で視覚化をすることを推奨
UC.pca0 <- prcomp(UC.data)
UC.pca1 <- prcomp(UC.data, scale.=TRUE)
summary(UC.pca0)
plot(UC.pca0) 
UC.pca0$rotation
summary(UC.pca1)
plot(UC.pca1)
UC.pca1$rotation

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
