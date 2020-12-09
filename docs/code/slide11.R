### 第11回 練習問題解答例

### 練習1.1
### 関数 kmeans による非階層的クラスタリング

## データの読み込み
JS.data <- read.csv(file="data/japan_social.csv", row.names=1)

## k-平均法の実行: 
set.seed(1234) # 必要に応じて初期値の乱数のシード
k <- 7 # 分割数を指定
JS.km <- kmeans(daisy(JS.data, stand=TRUE), # 標準化
		centers=k, # クラスタ数
		nstart=20) # 初期値を20回変更して試す

## 各クラスター内の県名を表示
for(i in 1:k){
    cat("<< cluster",i,">>\n")
    print(names(which(JS.km$cluster==i)))
}

## clusplotによるクラスタの表示
library(cluster) # clusplot を利用するため
clusplot(x=JS.data,
	 clus=JS.km$cluster, # クラスタ番号
	 stand=TRUE, # 標準化したデータで表示
	 lines=0, labels=3, # 表示の指定
	 main=NULL, sub=NULL, cex=0.8, # タイトルなどの調整
	 col.p=rainbow(k)[JS.km$cluster], # 虹色で色付け
	 col.clus="orange", shade=FALSE)  # クラスタ囲みの指定

### 練習1.2
### 関数 pam による非階層的クラスタリング

## k-medoids の実行
JS.pam <- pam(JS.data,
	      stand=TRUE,
	      k=k)

## 各クラスター内の県名を表示 (前問とは別の書き方)
for(i in 1:k){
    cat("<< cluster",i,">>\n")
    print(names(which(JS.pam$clustering==i)))
}

## クラスタの表示
clusplot(x=JS.data,
	 clus=JS.pam$clustering,
	 stand=TRUE,
	 lines=0, labels=3, 
	 main=NULL, sub=NULL, cex=0.8,
	 col.p=rainbow(k)[JS.pam$clustering],
	 col.clus="orange", shade=FALSE)

## 
OM.data <- read.csv(file="data/omusubi.csv", row.names=1)

## k-medoids の実行
k <- 6
OM.pam <- pam(daisy(sqrt(OM.data)), # Hellinger距離 (スケールをさぼっている)
	      k=k)

## 各クラスター内の県名を表示
for(i in 1:k){
    cat("<< cluster",i,">>\n")
    print(names(which(OM.pam$clustering==i)))
}

## クラスタの表示
clusplot(x=OM.data,
	 clus=OM.pam$clustering,
	 stand=TRUE,
	 lines=0, labels=3, 
	 main=NULL, sub=NULL, cex=0.8,
	 col.p=rainbow(k)[OM.pam$clustering],
	 col.clus="orange", shade=FALSE)

### 練習2.1
### 凝集係数による距離の検討

## データの読み込み (既に読み込んでいれば不要)
JS.data <- read.csv("data/japan_social.csv", row.names=1)

## ユークリッド距離による階層的クラスタリング
JS.agn.euc <- agnes(JS.data,
		 metric="euclidean", # データ距離
		 stand=TRUE,         # 標準化
		 method="average")   # クラスタ距離
plot(JS.agn.euc, which.plots=2, # デンドログラムの表示
     main="euclidean") 

## マンハッタン距離による階層的クラスタリング
JS.agn.man <- agnes(JS.data,
		 metric="manhattan",
		 stand=TRUE,
		 method="average")
plot(JS.agn.man, which.plots=2,
     main="manhattan")

## データ毎の凝集係数の表示
plot(JS.agn.euc, which.plots=1, # banner plotの表示
     nmax.lab=50,   # 表示するラベルの上限 (標準は40)
     max.strlen=5,  # 表示するラベルの文字数の上限
     main="euclidean")

plot(JS.agn.man, which.plots=1,
     nmax.lab=50,  
     max.strlen=5,
     main="manhattan")

## 一部のデータの距離が大きいと凝集係数は大きくなりがち (理由を考えてみよう)
## 北海道，東京，宮崎，鹿児島を除いて再計算する 
summary(agnes(JS.data[-c(1,13,45,46),],
	      metric="euclidean",
	      stand=TRUE,
	      method="average"))$ac
summary(agnes(JS.data[-c(1,13,45,46),],
	      metric="manhattan",
	      stand=TRUE,
	      method="average"))$ac
## ユークリッド距離の方が凝集係数は大きいことがわかる
## 個別の係数の確認
plot(agnes(JS.data[-c(1,13,45,46),],
           metric="euclidean",
           stand=TRUE,
           method="average"),
     which.plots=1,
     nmax.lab=50,  
     max.strlen=5,
     main="euclidean")
plot(agnes(JS.data[-c(1,13,45,46),],
           metric="manhattan",
           stand=TRUE,
           method="average"),
     which.plots=1,
     nmax.lab=50,  
     max.strlen=5,
     main="manhattan")

### 練習2.2
### シルエット係数によるクラスタ数の検討

## データの読み込み (既に読み込んでいれば不要)
OM.data <- read.csv(file="data/omusubi.csv", row.names=1)

## Hellinger距離の計算
OM.dsy <- 1/sqrt(2)*daisy(sqrt(OM.data/100))

## クラスタ数 4-10 で平均シルエット係数を確認
for(k in 4:10){
    cat("k =",k," ")
    print(summary(pam(OM.dsy,k=k))$silinfo$avg.width)
}

## 7-9 のシルエット係数を視覚化
plot(pam(OM.dsy,k=7), which.plot=2,
     nmax.lab=50, # 表示するラベルの上限 (標準は40)
     max.strlen=5, # 表示するラベルの文字数の上限
     cex.names=0.1) # ラベルの文字の大きさの調整
plot(pam(OM.dsy,k=8), which.plot=2,
     nmax.lab=50, max.strlen=5, cex.names=0.1)
plot(pam(OM.dsy,k=9), which.plot=2,
     nmax.lab=50, max.strlen=5, cex.names=0.1)

## k=8が悪いシルエット係数が少ないという意味で良さそうなのでクラスタの結果を表示
k <- 8
OM.pam <- pam(OM.dsy,k=k)
plot(OM.pam, which.plot=1, 
     stand=TRUE,
     lines=0, labels=3, 
     main="", sub=NULL, cex=0.8, # タイトルと文字の大きさの調整
     col.p=rainbow(k)[OM.pam$clustering], # クラスタ番号ごとに色付け
     col.clus="orange", shade=FALSE) # クラスタを楕円で表示
## cluster::clusplot のオプションを参考
## クラスタリングの結果は OM.pam$clustering に保管されている
