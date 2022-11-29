### 
### 第10講 サンプルコード
###

### 
### 練習問題 距離の計算
### 

### パッケージの読み込み 
library(cluster) # require(cluster)

## データの読み込み
js_data <- read.csv(file="data/japan_social.csv", row.names=1)

## ユークリッド距離とマンハッタン距離の計算
dst.euc <- dist(js_data, method="euclidean")
dst.man <- dist(js_data, method="manhattan")
dsy.euc <- daisy(js_data, metric="euclidean")
dsy.man <- daisy(js_data, metric="manhattan")
## 両者が同じことを確認
as.matrix(dst.euc)[1:5,1:5]
as.matrix(dsy.euc)[1:5,1:5]

## 正規化したユークリッド距離とマンハッタン距離の計算
dst.euc <- dist(scale(js_data), method="euclidean")
dst.man <- dist(scale(js_data), method="manhattan")
dsy.euc <- daisy(js_data, metric="euclidean", stand=TRUE)
dsy.man <- daisy(js_data, metric="manhattan", stand=TRUE)
## 正規化の方法が異なることに注意
as.matrix(dst.man)[10:15,10:15]
as.matrix(dsy.man)[10:15,10:15]

## 以下 daisy による正規化を用いる
## 関東の都県同士の距離を表示しなさい
str(dsy.euc) # 距離行列のもつ情報を見る
attr(dsy.euc, "Labels") # 県名を確認 (rownames(js_data)でも良い)
as.matrix(dsy.euc)[8:14, 8:14]
as.matrix(dsy.man)[8:14, 8:14]
## dist/dissimilarity オブジェクトは距離以外の様々な属性 (attributes) を持つ
## str(obj) : オブジェクトの構造(structure)を見る
## attributes(obj) : 属性を表示(変更)する
## attr(obj,属性名) : 特定の属性を表示(変更)する

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

### 
### 練習問題 階層的クラスタリング
### 

## クラスタリングの実行
js_dst <- dist(scale(js_data)) # 正規化してユークリッド距離を測る
js_est <- hclust(js_dst, method="average") # 群平均法
plot(js_est,
     cex=0.8, # 文字の大きさを調整
     sub="", xlab="", # 表示の一部を消去
     main="euclidean + average") # デンドログラムの表示

## クラスタの分割
k <- 5 # 分割数を指定
plot(js_est,
     hang=-1, # ラベルを揃えて表示
     cex=0.8, 
     sub="", xlab="", main="")
rect.hclust(js_est, k=k, border="orange") 
## 結果の確認 (各クラスタ内の県名を表示)
js_clst <- cutree(js_est, k=k) # デンドログラムを分割
js_pref <- rownames(js_data) # 県名の取得
for(i in 1:k){
    cat("=== cluster",i,"===\n")
    print(js_pref[js_clst==i])
}

## 主成分分析を併用して表示 (参考)
js_pca <- prcomp(js_data, scale.=TRUE) # データを正規化      
plot(predict(js_pca),
     pch=js_clst, # クラスタ毎に形を変える
     col="gray") 
text(predict(js_pca), 
     labels=paste0("  ",rownames(js_data)), # 空白を加えて県名を表示
     adj=c(0,0.5), # ラベルの位置をxは右寄せ(0)，yは真中(0.5)に指定
     col=js_clst, # クラスタ毎に色を変える
     cex=0.8) # 文字の大きさを調整
## 最大クラスタを再評価
table(js_clst) # 最大を確認
m <- which.max(table(js_clst)) # 最大クラスタの番号を取り出す
js_pca <- prcomp(js_data[js_clst==m,], scale.=TRUE) # 最大クラスタのみ処理
plot(predict(js_pca),
     pch=m, # クラスタの形を指定
     col="gray") 
text(predict(js_pca),
     labels=paste0("  ",rownames(js_data[js_clst==m,])),
     adj=c(0,0.5), 
     col=m, # クラスタの色を指定
     cex=0.8)

### 
### 練習問題 階層的クラスタリング
### 

## パッケージの読み込み
library(cluster) # 既に読み込んでいれば不要

## データの読み込み("omusubi.csv"を用いる)
om_data <- read.csv(file="data/omusubi.csv", row.names=1)

## データの視覚化
## pairs plot
pairs(om_data,
      col="blue",
      panel=panel.smooth, # 各散布図の傾向を見る回帰曲線を付加
      main="Favorite Filling in Omusubi (2009)")
## bar plot
barplot(t(as.matrix(om_data)), # barplot用にデータフレームを変換
        col=rainbow(8), # 具材ごとに色を変える
        legend.text=colnames(om_data), # 色の凡例を付加
        args.legend=list(cex=0.6), # 凡例の大きさを調整
        horiz=TRUE, # 横向きで作成
        las=1, # ラベルを水平に表示
        cex.names=0.6, # ラベルの文字の大きさを調整
        xlim=c(0,120), # 凡例のためにx軸に余白を付加
        axes=FALSE, # 座標軸を描かない
        main="Favorite Filling in Omusubi (2009)")

## Hellinger距離の計算
om_dsy <- 1/sqrt(2)*daisy(sqrt(om_data/100))
## 定数倍を気にしないのであれば daisy(sqrt(om_data)) でよい

## 階層的クラスタリング
om_agns <- agnes(om_dsy) 
plot(om_agns, which.plot=2, cex=0.8,
     main="Dendrogram of Omusubi Data")

## クラスタ数7として2次元のクラスタ表示
k <- 7
clusplot(x=om_data,
         clus=cutree(om_agns, k=k),
         labels=2,
         col.p="green", col.txt="blue", col.clus="orange", cex=0.8,
         main="Cluster of Omusubi Data")
