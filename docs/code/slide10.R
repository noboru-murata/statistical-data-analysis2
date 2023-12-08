### 第10講 サンプルコード
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(tidyverse)
library(ggfortify)
library(cluster)

#' データの読み込み
js_data <- read_csv("data/japan_social.csv") |>
  mutate(Area = as_factor(Area))

#' データの視覚化
js_data |> # 散布図．いくつかの変数は相関強いことがわかる
  select(where(is.double)) |>  # 都道府県名・地方区分は削除
  GGally::ggpairs()

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

library(cluster)
js_data <- read.csv(file="data/japan_social.csv", row.names=1)
pref <- read.csv(file="data/prefecture.csv", row.names=1)
rownames(js_data) <- pref$jp
myPlot <- function(k) {
	if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
	tmpa <- js_data[8:14,]
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
		 xlim=c(-2.5,1.5), ylim=c(-1.5,2.2),
		 col.p="blue", col.clus="white", shade=FALSE)
	if(k>0) {
	    for(i in 1:k) {
		clusplot(x=tmpa,
			 clus=tmpb[[i]],
			 diss=FALSE,
			 stand=TRUE, add=TRUE, span=FALSE,
			 lines=0, lwd=2, col.p="blue", col.clus="orange")
	    }
	}
}
myPlot(0)

myPlot(1)

myPlot(2)

myPlot(3)

myPlot(4)

myPlot(5)

myPlot(6)

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(agnes(scale(js_data[8:14,])), which.plots=2,
     main="",sub="",xlab="")

### 距離の計算，返値は dist class (特殊なベクトル)
dst <- dist(x, method = "euclidean", diag = FALSE, upper = FALSE)
## x: データフレーム
## method: 距離 (標準はユークリッド距離，他は"manhattan","minkowski"など)
## diag: 対角成分を持たせるか 
## upper: 上三角成分を持たせるか (標準は下三角成分のみ)

### 距離行列全体の表示
dst # または print(dst)

### 特定の成分の取得
as.matrix(dst)[i，j]
## i,j: 行・列の指定 (数値ベクトル，データフレームの行名)

### パッケージの読み込み (標準で含まれているのでinstallは不要)
library(cluster) # require(cluster)
### 距離の計算，返値は dissimilarity class (distとほぼ互換)
dsy <- daisy(x, metric = "euclidean", stand = FALSE)
## x: データフレーム
## metric: 距離 (標準はユークリッド距離，他は"manhattan"など)
## stand: 正規化(平均と絶対偏差の平均による)の有無

### 距離行列全体の表示
dsy # または print(dsy)
### 特定の成分の取得
as.matrix(dsy)[i，j]
## i,j: 行・列の指定 (数値ベクトル，データフレームの行名)

### データの読み込み 
js_data <- read.csv(file="data/japan_social.csv", row.names=1)

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

hclst <- hclust(d, method = "complete")
## d: 距離行列
## method: 分析法 (標準は最長距離法，他は"single","average"など)

### 系統樹の表示 (一般的な plot のオプションが利用可能)
plot(hclst)

### クラスタの分割
cutree(tree = hclst, k = NULL, h = NULL)
## tree: hclustの結果を指定
## k: クラスタ数を指定して分割
## h: クラスタ距離を指定して分割

### クラスタの分割表示 (cutree とほぼ同様のオプション)
rect.hclust(tree = hclst, k = NULL, h = NULL)

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

agns <- agnes(x, metric = "euclidean", stand = FALSE,
              method = "average")
## x: データフレーム，または距離行列
## metric: 距離 (標準はユークリッド距離，他は"manhattan"など)
## stand: 正規化(平均と絶対偏差の平均による)の有無
## method: 分析法 (標準は群平均法，他は"single","complete"など)

### 系統樹の表示 (一般的な plot のオプションが利用可能)
plot(agns, which.plots=2)
## which.plots=1 は評価の際に利用

clusplot(x, clus, stand =FALSE, 
	     lines = 2, shade = FALSE, labels= 0, 
	     col.p = "dark green", col.txt = col.p, col.clus = 5)
## x: データフレーム
## clus: クラスタ分割
## stand: 正規化の有無
## lines: クラスタ間の繋がりの表示 (0:無，1:外，2:中心)
## shade: 網掛けの有無
## labels: ラベルの表示 (0:無，2:データとクラスタ, 3:データ, 4:クラスタ, など)
## col.p/txt/clue: データ点・文字・クラスタの色指定

### データの読み込み 
om_data <- read.csv(file="data/omusubi.csv", row.names=1)

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

## 階層的クラスタリングの実行:
js_dst <- dist(scale(js_data)) # 正規化してユークリッド距離を測る
js_est <- hclust(js_dst, method="average") # 群平均法
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(js_est,
     sub="", xlab="", cex=0.8,
     main="Euclid 距離 + 群平均法") # デンドログラムの表示

k <- 5 # 分割数を指定
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(js_est,
     hang=-1, cex=0.8,
     sub="", xlab="", main="")
rect.hclust(js_est, k=k, border="orange")

## パッケージの読み込み
library(cluster)
library(tidyverse) 
library(ggfortify)
library(GGally)
library(ggdendro)
## データの読み込み("omusubi.csv"を用いる)
om_data <- read.csv(file="data/omusubi.csv", row.names=1)
pref <- read.csv(file="data/prefecture.csv", row.names=1)
## 表示
## foo <- head(om_data,10) # print(om_data)
## rbind(c("Name",names(foo)),data.frame(rownames(foo),foo))
## 1つだと処理が難しいので北海道は東北に含める
areaname <- c("tohoku","kanto","chubu", 
              "kinki","chugoku","chikoku","kyushu")
area <- rep(areaname,c(7,7,9,7,5,4,8))
rownames(om_data) <- pref$jp

## 県別の人気比率:
om_long <- om_data %>%
    tibble::rownames_to_column(var = "prefecture") %>%
    pivot_longer(-prefecture)
ggplot(om_long,aes(x=prefecture,y=value,fill=name)) +
    geom_bar(stat="identity",position="fill") +
    coord_flip() +
    scale_x_discrete(limits=rev(rownames(om_data))) +
    labs(title="おむすびの具 県別人気アンケート (2009)",
         x="県名",y="人気比率") +
    theme(legend.position="top",
          text=element_text(family="HiraMaruProN-W4"))

## データの散布図: 
ggpairs(data.frame(om_data,area),
        columns=1:ncol(om_data), mapping=aes(colour=area)) +
    labs(title="おむすびの具 県別人気アンケート (2009)") +
    theme(text=element_text(family="HiraMaruProN-W4"))

## 距離計算
dst <- daisy(sqrt(om_data)) # Hellinger距離
## 階層的クラスタリング:
hclst <- agnes(dst) # dianaという関数もある
ggdendrogram(as.dendrogram(hclst),
             rotate=TRUE, theme_dendro=FALSE) +
    labs(title="おむすびの具人気アンケートによるクラスタ分析",
         x="県名",y="距離") +
    theme(text=element_text(family="HiraMaruProN-W4"))
