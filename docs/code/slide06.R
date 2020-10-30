### 第06回 練習問題解答例

### 練習1
### 主成分分析の考え方

### 人工データ(2次元)による例
set.seed(123123)
n <- 100 # データ数
a <- c(1, 2)/sqrt(5) # 主成分負荷量(単位ベクトル)の設定 (適宜変更せよ)
myData <- data.frame( # aのスカラー倍に正規乱数を重畳
    runif(n,-1,1) %o% a + rnorm(2*n, sd=0.3))
names(myData) <- paste0("x",1:2) # 列名を付与
plot(myData, asp=1, # 縦横比を1とした散布図
     pch=4, col="blue") 
## a方向に本質的な情報が集約されていることがわかる
abline(0, a[2]/a[1], # 切片と傾きを指定
       col="red", lwd=2) # 主成分負荷量の図示
## 主成分負荷量の推定
est <- prcomp(myData)
ahat <- est$rotation[,1]
## 第１主成分負荷量がaに非常に近い (符号は反対)
abline(0, ahat[2]/ahat[1],
       col="orange", lty="dotted", lwd=2)
## 主成分得点の計算
pc1 <- predict(est)[,1] # 第１主成分得点の取得
points(pc1 %o% ahat, # 第1主成分を元の散布図上で図示
       pch=18, col="purple")

### 練習2
### 第1主成分の求め方

### 人工データ(3次元)による例
require(scatterplot3d)
set.seed(2468)
n <- 50 # データ数
d <- 3
a <- rnorm(d)
(a <- a/sqrt(sum(a^2))) # 主成分負荷量(単位ベクトル)を生成
myData <- data.frame(runif(n,-1,1) %o% a + rnorm(d*n, sd=0.3))
names(myData) <- paste0("x",1:d) # 観測データ
plot(myData, pch=4, col="blue") # 散布図行列
s3d <- scatterplot3d(myData, type="h", asp=1,
                     highlight.3d=TRUE)
est <- prcomp(myData)
pc1 <- predict(est)[,1]
s3d$points3d(pc1 %o% a, col="blue")

## 主成分負荷量の推定を固有値分解と比較
eig <- eigen(crossprod(scale(myData,scale=FALSE))) # 固有値分解
est$rotation # 主成分負荷量
eig$vectors  # 固有ベクトル (符号を除いて主成分負荷量と一致)
est$sdev               # 主成分の標準偏差
sqrt(eig$values/(n-1)) # 固有値と主成分の標準偏差の関係

### 練習3
### 主成分分析

### 実データによる確認
## データの読み込み
JS.data <- read.csv("data/japan_social.csv", row.names=1)
## データの視覚化
plot(JS.data, col="blue") # いくつかの変数は相関強い
boxplot(JS.data, col="green") # 箱ひげ図．変数のばらつきに大きな違いがある
(JS.pca <- prcomp(JS.data, scale=TRUE))
## 主成分方向から読み取れること:
## 第1: 人の多さに関する成分(正の向きほど人が多い)
## 第2: 農業生産力に関する成分(正の向きほど高い)
summary(JS.pca) # 寄与率の表示．来週詳しく説明する
## 2変数での解析例 (AgriとLandを取り上げる，その他の組み合わせでも試みよ)
## 多変数での視覚化は来週詳しく説明する
JS.subset <- scale(subset(JS.data, select=c(Agri,Land))) # 正規化
JS.pca2 <- prcomp(JS.subset)
ahat <- JS.pca2$rotation[,1]
pc1 <- predict(JS.pca2)[,1] 
plot(JS.subset, asp=1, pch=4, col="blue") 
abline(0, ahat[2]/ahat[1], col="orange", lty="dotted", lwd=2)
points(pc1 %o% ahat, pch=18, col="purple")
text(JS.subset, labels=row.names(JS.subset), cex=0.5) # 県名を表示
