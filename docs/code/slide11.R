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

library(cluster)
js_data <- bind_cols(
  read_csv(file="data/japan_social.csv"),
  read_csv(file="data/prefecture.csv"))
myPlot <- function(k) {
  if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
  tmpa <- js_data |>
    slice(8:14) |>
    select(2:6,jp) |>
    column_to_rownames(var = "jp")
  # tmpa <- js_data[8:14,]
  tmpb <- list(c(1,2,3,4,1,6,7),
               c(1,2,3,1,1,6,7),
               c(1,2,2,1,1,6,7),
               c(1,2,2,1,1,6,1),
               c(1,1,1,1,1,6,1),
               c(1,1,1,1,1,1,1))
  clusplot(x=tmpa,
           clus=c(1,2,3,4,5,6,7),
           diss=FALSE,
           stand=TRUE, lines=0, labels=3, 
           main=NULL, sub=NULL, cex=1,
           xlim=c(-2.5,2.5), ylim=c(-2.5,2.5),
           xaxt="n", yaxt="n", ann=FALSE,
           col.p="blue", col.txt="darkgray", col.clus="white", shade=FALSE)
  if(k>0) {
    for(i in 1:k) {
      clusplot(x=tmpa,
               clus=tmpb[[i]],
               diss=FALSE, cex=0.2,
               stand=TRUE, add=TRUE, span=FALSE,
               lines=0, lwd=2, col.p="blue", col.clus="orange")
    }
  }
}
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
par(mfrow=c(4,2), mar=rep(0.5,4))
for(i in 0:6) myPlot(i)
js_data |>
  slice(8:14) |>
  select(2:6,jp) |>
  column_to_rownames(var = "jp") |>
  scale() |>
  agnes() |>
  plot(which.plots=2, main="",sub="",xlab="")

om_data <- read.csv(file="data/omusubi.csv", row.names=1)
pref <- read.csv(file="data/prefecture.csv", row.names=1)
rownames(om_data) <- pref$jp

om_subset <- om_data[-c(8:14,24:30),]

n <- nrow(om_subset)

jdx <- 1:ncol(om_subset) # jdx <- sample(ncol(om_subset),2)
om_pca <- prcomp(om_subset[,jdx])

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(predict(om_pca),
	 col="blue", pch=19, cex=0.5,
	 xlab="Component 1", ylab="Component 2")
text(transform(predict(om_pca), PC2=PC2+0.5),
	 labels=rownames(om_subset), col="blue", cex=1)

## 図示のための関数
myPlot <- function(cntr) {
  dsy <- daisy(rbind(om_subset[,jdx],cntr))
  clst <- apply(as.matrix(dsy)[1:n,(1:k)+n],
		  1,
		  function(x){which.min(x)})
  plot(predict(om_pca), col=clst, pch=clst, type="n",
	 xlab="Component 1", ylab="Component 2")
  text(predict(om_pca), labels=clst, col=clst, cex=1.5)
  points(predict(om_pca,newdata=cntr), col=1:k, pch=19, cex=2)
  cntr <- aggregate(. ~ clst,
		      data=data.frame(om_subset[,jdx],clst),
		      mean)[,-1]
  return(cntr)
}
set.seed(1212)
k <- 5
idx <- sample(n,k)
cntr <- om_subset[idx,jdx]
## 
cntr <- myPlot(cntr)

cntr <- myPlot(cntr)

cntr <- myPlot(cntr)

cntr <- myPlot(cntr)

cntr <- myPlot(cntr)

cntr <- myPlot(cntr)

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
dsy <- daisy(rbind(om_subset[,jdx],cntr))
clst <- apply(as.matrix(dsy)[1:n,(1:k)+n],
              1,
              function(x){which.min(x)})
plot(predict(om_pca),
     col="blue", pch=19, cex=0.5,
     xlab="Component 1", ylab="Component 2")
text(transform(predict(om_pca), PC2=PC2+0.5),
     labels=rownames(om_subset), col=clst, cex=1)

kmeans(x, centers, iter.max = 10, nstart = 1,
	   algorithm = "Hartigan-Wong")
## x: データフレーム
## centers: クラスタ数
## iter.max: 最大繰り返し数
## nstart: 初期値の候補数
## algorithm: 最適化法の指定．他に "Lloyd", "Forgy", "MacQueen" が指定可

js_data <- read.csv("data/japan_social.csv", row.names=1)

### 
### 練習問題 関数 kmeans による非階層的クラスタリング
### 

## データの読み込み
js_data <- read.csv(file="data/japan_social.csv", row.names=1)

## k-平均法の実行: 
set.seed(1234) # 必要に応じて初期値の乱数のシード
k <- 7 # 分割数を指定
js_km <- kmeans(scale(js_data), # 標準化
		  centers=k, # クラスタ数
		  nstart=20) # 初期値を20回変更して試す

## 各クラスター内の県名を表示
for(i in 1:k){
  cat("=== cluster",i,"===\n")
  print(names(which(js_km$cluster==i)))
}

## clusplotによるクラスタの表示
library(cluster) # clusplot を利用するため
clusplot(x=js_data,
	   clus=js_km$cluster, # クラスタ番号
	   stand=TRUE, # 標準化したデータで表示
	   lines=0, labels=3, # 表示の指定
	   main=NULL, sub=NULL, cex=0.8, # タイトルなどの調整
	   col.p=rainbow(k)[js_km$cluster], # 虹色で色付け
	   col.clus="orange", shade=FALSE)	 # クラスタ囲みの指定

pam(x, k, metric = "euclidean", stand = FALSE)
## x: データフレーム，または距離行列 
## k: クラスタの数
## metric: 距離の指定(xがデータフレームの場合)．他に "manhattan" が指定可
## stand: 標準化(平均0，絶対偏差1)

### 
### 練習問題 関数 pam による非階層的クラスタリング
### 

## k-medoids の実行
js_pam <- pam(js_data,
              stand=TRUE,
              k=k)

## 各クラスター内の県名を表示
for(i in 1:k){
    cat("=== cluster",i,"===\n")
    print(names(which(js_pam$clustering==i)))
}

## クラスタの表示
clusplot(x=js_data,
         clus=js_pam$clustering,
         stand=TRUE,
         lines=0, labels=3, 
         main=NULL, sub=NULL, cex=0.8,
         col.p=rainbow(k)[js_pam$clustering],
         col.clus="orange", shade=FALSE)

## 
om_data <- read.csv(file="data/omusubi.csv", row.names=1)

## k-medoids の実行
k <- 6
om_pam <- pam(daisy(sqrt(om_data)), # Hellinger距離 (スケールをさぼっている)
              k=k)

## 各クラスター内の県名を表示
for(i in 1:k){
    cat("=== cluster",i,"===\n")
    print(names(which(om_pam$clustering==i)))
}

## クラスタの表示
clusplot(x=om_data,
         clus=om_pam$clustering,
         stand=TRUE,
         lines=0, labels=3, 
         main=NULL, sub=NULL, cex=0.8,
         col.p=rainbow(k)[om_pam$clustering],
         col.clus="orange", shade=FALSE)

### 総務省統計局の統計データによる例
library(cluster)
library(RColorBrewer)
js_data <- read.csv(file="data/japan_social.csv", row.names=1)
pref <- read.csv(file="data/prefecture.csv", row.names=1)
rownames(js_data) <- pref$jp

## k-平均法の実行: 
set.seed(1234)
k <- 7 # 分割数を指定
js_km <- kmeans(scale(js_data), centers=k, nstart=20)

## 結果の確認 (各クラスター内の県名を表示)
for(i in 1:k){
	   cat("=== cluster",i,"===\n")
	   print(pref$jp[js_km$cluster==i])
}

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
clusplot(x=js_data,
	      clus=js_km$cluster,
	      diss=FALSE,
	      stand=TRUE, lines=0, labels=3, 
	      main=NULL, sub=NULL, cex=0.8,
	      col.p=brewer.pal(k,"Dark2")[js_km$cluster],
	      col.clus="orange", shade=FALSE)

## k-medoids の実行
js_pam <- pam(scale(js_data),
		     k=k)
		     ## metric="manhattan", stand=TRUE)

## 結果の確認 (各クラスター内の県名を表示)
for(i in 1:k){
	 cat("=== cluster",i,"===\n")
	 print(pref$jp[js_pam$clustering==i])
}

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
clusplot(x=js_data,
	      clus=js_pam$clustering,
	      diss=FALSE,
	      stand=TRUE, lines=0, labels=3, 
	      main=NULL, sub=NULL, cex=0.8,
	      col.p=brewer.pal(k,"Dark2")[js_pam$clustering],
	      col.clus="orange", shade=FALSE)

om_data <- read.csv(file="data/omusubi.csv", row.names=1)
pref <- read.csv(file="data/prefecture.csv", row.names=1)
rownames(om_data) <- pref$jp

## k-medoids の実行
k <- 6
om_pam <- pam(daisy(sqrt(om_data)),
		  k=k,
		  stand=TRUE)

## 結果の確認 (各クラスター内の県名を表示)
for(i in 1:k){
    cat("=== cluster",i,"===\n")
    print(pref$jp[om_pam$clustering==i])
}

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
clusplot(x=om_data,
	   clus=om_pam$clustering,
	   diss=FALSE,
	   stand=TRUE, lines=0, labels=3, 
	   main=NULL, sub=NULL, cex=0.8,
	   col.p=brewer.pal(k,"Dark2")[om_pam$clustering],
	   col.clus="orange", shade=FALSE)

### 凝集係数の取得
summary(agnes(x, # データフレーム
              stand = TRUE, # 正規化
		  metric = "euclidean", # ユークリッド距離
		  method = "average") # 群平均法
	    )$ac 
### 凝集係数の視覚化 (banner plot)
plot(agnes(x), # 階層的クラスタリングの結果
     which.plots=1) # banner plot を選択
## plot および cluster::bannerplot のオプションを参考

###
### 練習問題 凝集係数による距離の検討
### 

## データの読み込み (既に読み込んでいれば不要)
js_data <- read.csv("data/japan_social.csv", row.names=1)

## ユークリッド距離による階層的クラスタリング
js_agn.euc <- agnes(js_data,
			metric="euclidean", # データ距離
			stand=TRUE,	    # 標準化
			method="average")   # クラスタ距離
plot(js_agn.euc, which.plots=2, # デンドログラムの表示
	    main="euclidean") 

## マンハッタン距離による階層的クラスタリング
js_agn.man <- agnes(js_data,
			metric="manhattan",
			stand=TRUE,
			method="average")
plot(js_agn.man, which.plots=2,
	    main="manhattan")

## データ毎の凝集係数の表示
plot(js_agn.euc, which.plots=1, # banner plotの表示
	    nmax.lab=50,   # 表示するラベルの上限 (標準は40)
	    max.strlen=5,  # 表示するラベルの文字数の上限
	    main="euclidean")

plot(js_agn.man, which.plots=1,
	    nmax.lab=50,  
	    max.strlen=5,
	    main="manhattan")

## 一部のデータの距離が大きいと凝集係数は大きくなりがち (理由を考えてみよう)
## 北海道，東京，宮崎，鹿児島を除いて再計算する 
summary(agnes(js_data[-c(1,13,45,46),],
		     metric="euclidean",
		     stand=TRUE,
		     method="average"))$ac
summary(agnes(js_data[-c(1,13,45,46),],
		     metric="manhattan",
		     stand=TRUE,
		     method="average"))$ac
## ユークリッド距離の方が凝集係数は大きいことがわかる
## 個別の係数の確認
plot(agnes(js_data[-c(1,13,45,46),],
		  metric="euclidean",
		  stand=TRUE,
		  method="average"),
	    which.plots=1,
	    nmax.lab=50,  
	    max.strlen=5,
	    main="euclidean")
plot(agnes(js_data[-c(1,13,45,46),],
		  metric="manhattan",
		  stand=TRUE,
		  method="average"),
	    which.plots=1,
	    nmax.lab=50,  
	    max.strlen=5,
	    main="manhattan")

### シルエット係数関連の情報取得
summary(pam(x, k))$silinfo
### 各データのシルエット係数
summary(pam(x, k))$silinfo$widths
### 各クラスタのシルエット係数の平均
summary(pam(x, k))$silinfo$clus.avg.widths
### シルエット係数の平均
summary(pam(om_dsy,k=k))$silinfo$avg.width
### シルエット係数の視覚化 (silhouette plot)
plot(pam(om_dsy,k=k), which.plot=2)
## plot および cluster::silhouette のオプションを参考

### 
### 練習問題 シルエット係数によるクラスタ数の検討
### 

## データの読み込み (既に読み込んでいれば不要)
om_data <- read.csv(file="data/omusubi.csv", row.names=1)

## Hellinger距離の計算
om_dsy <- 1/sqrt(2)*daisy(sqrt(om_data/100))

## クラスタ数 4-10 で平均シルエット係数を確認
for(k in 4:10){
	   cat("k =",k," ")
	   print(summary(pam(om_dsy,k=k))$silinfo$avg.width)
}

## 7-9 のシルエット係数を視覚化
plot(pam(om_dsy,k=7), which.plot=2,
	    nmax.lab=50, # 表示するラベルの上限 (標準は40)
	    max.strlen=5, # 表示するラベルの文字数の上限
	    cex.names=0.1) # ラベルの文字の大きさの調整
plot(pam(om_dsy,k=8), which.plot=2,
	    nmax.lab=50, max.strlen=5, cex.names=0.1)
plot(pam(om_dsy,k=9), which.plot=2,
	    nmax.lab=50, max.strlen=5, cex.names=0.1)

## k=8が悪いシルエット係数が少ないという意味で良さそうなのでクラスタの結果を表示
k <- 8
om_pam <- pam(om_dsy,k=k)
plot(om_pam, which.plot=1, 
	    stand=TRUE,
	    lines=0, labels=3, 
	    main="", sub=NULL, cex=0.8, # タイトルと文字の大きさの調整
	    col.p=rainbow(k)[om_pam$clustering], # クラスタ番号ごとに色付け
	    col.clus="orange", shade=FALSE) # クラスタを楕円で表示
## cluster::clusplot のオプションを参考
## クラスタリングの結果は om_pam$clustering に保管されている

js_agn.euc <- agnes(js_data,
		      metric="euclidean",
		      stand=TRUE,
		      method="average")
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(js_agn.euc, which.plots=1,
	  nmax.lab=50, 
	  max.strlen=5, 
	  cex.axis=0.5,
	  main="")

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(js_agn.euc, which.plots=2,
	  cex=0.8, xlab="", main="")

js_agn.man <- agnes(js_data,
		      metric="manhattan",
		      stand=TRUE,
		      method="average")
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(js_agn.man, which.plots=1,
	  nmax.lab=50, 
	  max.strlen=5, 
	  cex.axis=0.5,
	  main="")

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(js_agn.man, which.plots=2,
	  cex=0.8, xlab="", main="")

cat("凝集係数 (ユークリッド距離)\n")
summary(agnes(js_data[-c(1,13,45,46),],
		     metric="euclidean",
		     stand=TRUE,
		     method="average"))$ac
cat("凝集係数 (マンハッタン距離)\n")
summary(agnes(js_data[-c(1,13,45,46),],
		     metric="manhattan",
		     stand=TRUE,
		     method="average"))$ac

## さまざまなクラスタ数で平均シルエット係数を確認
om_dsy <- daisy(sqrt(om_data))
cat("シルエット係数\n")
for(k in 4:10){
	   cat("k =",k," ")
	   print(summary(pam(om_dsy,k=k))$silinfo$avg.width)
}

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(pam(om_dsy,k=7), which.plot=2,
	  nmax.lab=50, 
	  max.strlen=5, 
	  cex.names=0.5,
	  main="")

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(pam(om_dsy,k=8), which.plot=2,
	  nmax.lab=50, 
	  max.strlen=5, 
	  cex.names=0.5,
	  main="")

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(pam(om_dsy,k=9), which.plot=2,
	  nmax.lab=50, 
	  max.strlen=5, 
	  cex.names=0.5,
	  main="")

## k=8が良さそう
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
k <- 8
om_pam <- pam(om_dsy,k=k)
plot(om_pam, which.plot=1,
	  stand=TRUE, lines=0, labels=3,
	  main="", vsub=NULL, cex=0.8,
	  col.p=brewer.pal(k,"Dark2")[om_pam$clustering],
	  col.clus="orange", shade=FALSE)
