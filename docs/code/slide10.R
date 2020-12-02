### 第10回 資料

### 練習1
### 距離の計算

### パッケージの読み込み 
library(cluster) # require(cluster)

## データの読み込み
JS.data <- read.csv(file="data/japan_social.csv", row.names=1)

## ユークリッド距離とマンハッタン距離の計算
dst.euc <- dist(JS.data, method="euclidean")
dst.man <- dist(JS.data, method="manhattan")
dsy.euc <- daisy(JS.data, metric="euclidean")
dsy.man <- daisy(JS.data, metric="manhattan")
## 両者が同じことを確認
as.matrix(dst.euc)[1:5,1:5]
as.matrix(dsy.euc)[1:5,1:5]

## 正規化したユークリッド距離とマンハッタン距離の計算
dst.euc <- dist(scale(JS.data), method="euclidean")
dst.man <- dist(scale(JS.data), method="manhattan")
dsy.euc <- daisy(JS.data, metric="euclidean", stand=TRUE)
dsy.man <- daisy(JS.data, metric="manhattan", stand=TRUE)
## 正規化の方法が異なることに注意
as.matrix(dst.man)[10:15,10:15]
as.matrix(dsy.man)[10:15,10:15]

## 以下 daisy による正規化を用いる
## 関東の都県同士の距離を表示しなさい
str(dsy.euc) # 距離行列のもつ情報を見る
attr(dsy.euc, "Labels") # 県名を確認 (rownames(JS.data)でも良い)
as.matrix(dsy.euc)[8:14, 8:14]
as.matrix(dsy.man)[8:14, 8:14]

## 大阪と四国の間の距離
as.matrix(dsy.euc)[27, 36:39, drop=FALSE] # 行列として表示
as.matrix(dsy.man)["Osaka", # 1行なので標準ではベクトルとして扱われる
		   c("Tokushima","Kagawa","Ehime","Kochi")]

## ユークリッド距離とマンハッタン距離の散布図
plot(dsy.euc, dsy.man,
     xlab="Euclid dist.", ylab="Manhattan dist.")
plot(dsy.euc, dsy.man, 
     xlim=c(0,3), ylim=c(0,3), # 原点近傍のみ表示
     xlab="Euclid dist.", ylab="Manhattan dist.")
## いくつか順序が入れ替わっていることがわかる

### 練習2.1
### 階層的クラスタリング

## クラスタリングの実行
JS.dst <- dist(scale(JS.data)) # 正規化してユークリッド距離を測る
JS.est <- hclust(JS.dst, method="average") # 群平均法
plot(JS.est,
     cex=0.8, # 文字の大きさを調整
     sub="", xlab="", # 表示の一部を消去
     main="euclidean + average") # デンドログラムの表示

## クラスタの分割
k <- 5 # 分割数を指定
plot(JS.est,
     hang=-1, # ラベルを揃えて表示
     cex=0.8, 
     sub="", xlab="", main="")
rect.hclust(JS.est, k=k, border="orange") 
## 結果の確認 (各クラスタ内の県名を表示)
JS.clst <- cutree(JS.est, k=k) # デンドログラムを分割
JS.pref <- rownames(JS.data) # 県名の取得
for(i in 1:k){
    cat("<<cluster ",i,">>\n")
    print(JS.pref[JS.clst==i])
}

## 主成分分析を併用して表示 (参考)
JS.pca <- prcomp(JS.data, scale.=TRUE) # データを正規化      
plot(predict(JS.pca),
     pch=JS.clst, col=JS.clst) # クラスタ毎に色と形を変える
text(predict(JS.pca),
     labels=rownames(JS.data), col="orchid", cex=0.8) # 県名を表示
## 最大クラスタを再評価
table(JS.clst) # 最大を確認
m <- which.max(table(JS.clst)) # 最大クラスタの番号を取り出す
JS.pca <- prcomp(JS.data[JS.clst==m,], scale.=TRUE) 
plot(predict(JS.pca), col=m)
text(predict(JS.pca),
     labels=rownames(JS.data[JS.clst==l,]), col="orchid", cex=0.8)

### 練習2.2
### 階層的クラスタリング

## パッケージの読み込み
library(cluster) # 既に読み込んでいれば不要

## データの読み込み("omusubi.csv"を用いる)
OM.data <- read.csv(file="data/omusubi.csv", row.names=1)

## データの視覚化
## pairs plot
pairs(OM.data, panel=panel.smooth, # 各散布図の傾向を見る
      main="Favorite Filling in Omusubi (2009)")
## bar plot
barplot(t(as.matrix(OM.data)),col=rainbow(8),
        legend.text=colnames(OM.data),
        horiz=TRUE,xlim=c(0,130),las=2,
        cex.names=0.6,axes=FALSE)
title(main="Favorite Filling in Omusubi (2009)")

## Hellinger距離の計算
OM.dsy <- 1/sqrt(2)*daisy(sqrt(OM.data/100))
## 定数倍を気にしないのであれば daisy(sqrt(OM.data)) でよい

## 階層的クラスタリング
OM.agns <- agnes(OM.dsy) 
plot(OM.agns, which.plot=2, cex=0.8,
     main="Dendrogram of Omusubi Data")

## クラスタ数7として2次元のクラスタ表示
k <- 7
clusplot(x=OM.data,
         clus=cutree(OM.agns, k=k),
         labels=2,
         col.p="green", col.txt="blue", col.clus="orange", cex=0.8,
         main="Cluster of Omusubi Data")
