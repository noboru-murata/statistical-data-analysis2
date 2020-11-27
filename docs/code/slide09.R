### 第09回 練習問題解答例

### 練習1.1
### 判別結果の評価

### 東京の気象データによる判別分析
library(MASS)

## 9月と10月の判別の例
## データの整理
TW.data <- transform(read.csv("data/tokyo_weather_reg.csv"),
		     month=substr(as.Date(date),6,7)) # 月を切り出し
TW.subset  <- transform(subset(TW.data,
			       subset= month %in% c("09","10"),
			       select=c(temp,humid,month)),
			month=factor(month)) # 因子にする
## 判別関数を作成
TW.lda <- lda(month ~ temp + humid, data=TW.subset)
TW.qda <- qda(month ~ temp + humid, data=TW.subset)
## 判別結果の評価
TW.lest <- predict(TW.lda)
TW.qest <- predict(TW.qda)

## 混同行列の作成
(ltab <- table(Prediction=TW.lest$class,
               Truth=TW.subset$month))
(qtab <- table(Prediction=TW.qest$class,
               Truth=TW.subset$month))
## positive=9月とする
## 線形判別の評価
ltab[1,1] # 真陽性 true positive
ltab[1,2] # 偽陽性 false positive
ltab[2,1] # 偽陰性 false negative
ltab[2,2] # 真陰性 true negative
(ltab[1,1]+ltab[2,2])/sum(ltab) # 精度 accuracy
sum(diag(ltab))/sum(ltab) # 上記と同様，行列のように扱うことも可能
ltab[1,1]/sum(ltab[1,]) # 適合率 precision
## 2次判別の評価
sum(diag(qtab))/sum(qtab) # 精度

### 判別境界の可視化の例
## 準備
myPch <- c(16,17) # 塗り潰し丸(9月)，三角(10月)
myCol <- c("red","green") # 赤(9月)，緑(10月)
myBg <- c(rgb(1,0,0,0.1), rgb(0,1,0,0.1)) # 薄赤(9月)，薄緑(10月)
sx <- pretty(with(TW.subset,range(temp)), 120)
sy <- pretty(with(TW.subset,range(humid)), 120)
myGrid <- expand.grid(temp=sx,humid=sy)

## 線形判別
with(TW.subset, 
     plot(temp, humid, # 試験データの散布図
          pch=myPch[month], col=myCol[month],
          xlab="temperature", ylab="humidity",
          main="linear discriminant"))
## 誤ったデータを表示
with(TW.subset[TW.lest$class!=TW.subset$month,], 
     points(temp, humid, 
	    pch=1, col="orchid", cex=2, lwd=2))
## 判別されるラベルごとに背景を着色
image(x=sx, y=sy, add=TRUE, col=myBg,
      z=matrix(as.numeric(predict(TW.lda,newdata=myGrid)$class),
               length(sx), length(sy)))

## 2次判別
with(TW.subset, 
     plot(temp, humid, # 試験データの散布図
          pch=myPch[month], col=myCol[month],
          xlab="temperature", ylab="humidity",
          main="quadratic discriminant"))
## 誤ったデータを表示
with(TW.subset[TW.qest$class!=TW.subset$month,], 
     points(temp, humid, 
	    pch=1, col="orchid", cex=2, lwd=2))
## 判別されるラベルごとに背景を着色
image(x=sx, y=sy, add=TRUE, col=myBg,
      z=matrix(as.numeric(predict(TW.qda,newdata=myGrid)$class),
               length(sx), length(sy)))

## 後述する caret package を使う場合
library(caret)
confusionMatrix(TW.lest$class, TW.subset$month) # 線形
confusionMatrix(TW.qest$class, TW.subset$month) # 2次

## 12ヶ月分のデータを用いた例 (説明変数は適宜選択せよ)
TW.subset  <- transform(subset(TW.data,
			       select=c(temp,solar,wind,humid,month)),
			month=factor(month)) 
## 判別関数を作成
TW.lda <- lda(month ~ ., # 右辺の . は month 以外の全てを説明変数として指定
	      data=TW.subset)
## 判別結果の評価
TW.est <- predict(TW.lda)
confusionMatrix(TW.est$class, TW.subset$month)
confusionMatrix(TW.est$class, TW.subset$month)$table
confusionMatrix(TW.est$class, TW.subset$month)$overall
confusionMatrix(TW.est$class, TW.subset$month)$byClass

### 練習1.2
### 判別結果の評価

## Wine Quality Data Set を用いた判別分析
WQ.org <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
		   sep=";")
table(WQ.org$quality) 
WQ.data <- transform(WQ.org,
		     quality=factor(
			 ifelse(quality %in% 7:10, "A",
			 ifelse(quality %in% 5:6, "B" ,"C"))))
## 判別関数を作成
WQ.lda <- lda(quality ~ ., data=WQ.data)
WQ.qda <- qda(quality ~ ., data=WQ.data)
## 判別結果の評価
confusionMatrix(predict(WQ.lda)$class, WQ.data$quality)
confusionMatrix(predict(WQ.qda)$class, WQ.data$quality)

### 練習2.1
### 予測誤差の評価

### 東京の気象データによる判別分析
library(MASS)  # 既に読み込んでいれば不要

## データの整理 (既に整理してあれば不要)
TW.data <- transform(read.csv("data/tokyo_weather_reg.csv"),
		     month=substr(as.Date(date),6,7)) # 月を切り出し
TW.subset  <- transform(subset(TW.data,
			       subset= month %in% c("09","10"),
			       select=c(temp,humid,month)),
			month=factor(month)) # 因子にする

## LOO交叉検証法
lloo <- c()
qloo <- c() 
for(i in 1:nrow(TW.subset)) {
    ## 線形
    est <- lda(month ~ temp + humid,
               data=TW.subset, subset=-i)
    lloo[i] <- predict(est, newdata=TW.subset[i,])$class
    ## 2次
    est <- qda(month ~ temp + humid,
               data=TW.subset, subset=-i)
    qloo[i] <- predict(est, newdata=TW.subset[i,])$class
}
## l/qloo には LOO交叉検証法による予測値が入っている
table(Prediction=lloo, Truth=TW.subset$month) # 線形
table(Prediction=qloo, Truth=TW.subset$month) # 2次

## k-重交叉検証法
lfld <- c()
qfld <- c() 
k <- 10 
blk <- sample(k, nrow(TW.subset), replace=TRUE) 
for(i in 1:k) {
    idx <- which(blk==i) # 第iブロックのデータ番号を取得
    ## 線形
    est <- lda(month ~ temp + humid,
               data=TW.subset, subset=-idx)
    lfld[idx] <- predict(est, newdata=TW.subset[idx,])$class
    ## 2次
    est <- qda(month ~ temp + humid,
               data=TW.subset, subset=-idx)
    qfld[idx] <- predict(est, newdata=TW.subset[idx,])$class
}
## fld には k-重交叉検証法による予測値が入っている
table(Prediction=lfld, Truth=TW.subset$month) # 線形
table(Prediction=qfld, Truth=TW.subset$month) # 2次

### 練習2.2
### 予測誤差の評価

### Wine Quality Data Set による誤差の評価
library(MASS)  # 既に読み込んでいれば不要
library(caret) # 既に読み込んでいれば不要

## データの整理 (既に整理してあれば不要)
WQ.org <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
		   sep=";")
WQ.data <- transform(WQ.org,
		     quality=factor(
			 ifelse(quality %in% 7:10, "A",
			 ifelse(quality %in% 5:6, "B" ,"C"))))

## LOO CV の例 (lda/qdaは標準で装備している)
## 線形判別
WQ.lda <- lda(quality ~ ., data=WQ.data) 
WQ.ldloo <- lda(quality ~ ., data=WQ.data, CV=TRUE)
confusionMatrix(predict(WQ.lda)$class, WQ.data$quality)$table
confusionMatrix(WQ.ldloo$class, WQ.data$quality)$table
## 線形判別の過学習は微小

## 2次判別
WQ.qda <- qda(quality ~ ., data=WQ.data) 
WQ.qdloo <- qda(quality ~ ., data=WQ.data, CV=TRUE)
confusionMatrix(predict(WQ.qda)$class, WQ.data$quality)$table
confusionMatrix(WQ.qdloo$class, WQ.data$quality)$table
## 2次判別は若干過学習している

## 予測誤差の比較
confusionMatrix(WQ.ldloo$class, WQ.data$quality)$overall
confusionMatrix(WQ.qdloo$class, WQ.data$quality)$overall
## 予測誤差の観点からは線形判別の方が良さそう

## k-重交叉検証は caret package の機能を利用して求めることができる
(train(quality ~., data=WQ.data, method="lda",
	     trControl=trainControl(method="cv", number=10)))
(train(quality ~., data=WQ.data, method="qda",
	     trControl=trainControl(method="cv", number=10)))
## LOO CV も利用することができるが，計算は遅い
(train(quality ~., data=WQ.data, method="lda",
	     trControl=trainControl(method="LOOCV")))
(train(quality ~., data=WQ.data, method="qda",
	     trControl=trainControl(method="LOOCV")))
