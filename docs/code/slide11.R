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
