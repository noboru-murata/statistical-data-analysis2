### 第08回 練習問題解答例

### 練習1
### 線形判別

### 東京の気象データによる判別分析
library(MASS)
## データの整理
TW.data <- transform(read.csv("data/tokyo_weather_reg.csv"),
		     month=as.numeric(months(as.Date(date), 
					     abbreviate=TRUE)))
TW.subset  <- subset(TW.data,
		     subset= month %in% c(10,11),
		     select=c(temp,humid,month))
idx <- seq(2,60,by = 2)
TW.train <- TW.subset[ idx,] # 訓練データ
TW.test  <- TW.subset[-idx,] # 試験データ
## 視覚化
with(TW.subset, 
    plot(temp, humid, # 散布図の作成
	 pch=month, col=month,
	 xlab="temperature",ylab="humidity",
	 main="Oct. & Nov"))
legend("bottomright",inset=.05, # 凡例の作成
       pch=c(10,11), col=c(10,11), legend=c("Oct","Nov"))
## 訓練データで判別関数を作成．等分散性を仮定
TW.lda <- lda(month ~ temp + humid, data=TW.train)
plot(TW.lda) # 訓練データの判別関数値
table(true=TW.train$month, pred=predict(TW.lda)$class) # 真値と予測値の比較
## 試験データによる評価
TW.pred <- predict(TW.lda, newdata=TW.test) 
table(true=TW.test$month, pred=TW.pred$class) # 真値と予測値の比較
TW.pred$class 
TW.test$month 
## 判別結果の図示
myLine <- function(z) { # 判別境界を引くための関数
    a0<-as.vector(colMeans(z$means) %*% z$scaling)
    a<-c(a0/z$scaling[2],-z$scaling[1]/z$scaling[2])
    return(a)
}
with(TW.test, 
    plot(temp, humid, # 試験データの散布図
	 pch=month, col=month,
	 xlab="temperature",ylab="humidity",
	 main="Oct. & Nov"))
with(TW.train, 
     points(temp, humid, # 訓練データの散布図
	 pch=month, col=month+3))
abline(myLine(TW.lda), col="blue", lwd=2)

### 練習2
### 2次判別

### 東京の気象データによる判別分析
library(MASS)
## データの整理 (前に実行している場合は不要)
TW.data <- transform(read.csv("data/tokyo_weather_reg.csv"),
		     month=as.numeric(months(as.Date(date), 
					     abbreviate=TRUE)))
TW.subset  <- subset(TW.data,
		     subset= month %in% c(10,11),
		     select=c(temp,humid,month))
idx <- seq(2,60,by = 2)
TW.train <- TW.subset[ idx,] # 訓練データ
TW.test  <- TW.subset[-idx,] # 試験データ
## 訓練データで判別関数を作成
TW.qda <- qda(month ~ temp + humid, data=TW.train)
table(true=TW.train$month, pred=predict(TW.qda)$class) # 真値と予測値の比較
## 試験データによる評価
TW.pred <- predict(TW.qda, newdata=TW.test) 
table(true=TW.test$month, pred=TW.pred$class) # 真値と予測値の比較
TW.pred$class 
TW.test$month 
## 判別結果の図示
## 判別境界を描くのは複雑なので，色と形で代用する
with(TW.test, 
    plot(temp, humid, # 試験データの散布図
	 pch=as.numeric(TW.pred$class),
	 col=month,
	 xlab="temperature",ylab="humidity",
	 main="Oct. & Nov"))

### 練習3
### 多値判別

### 東京の気象データによる判別分析
library(MASS)
## データの整理 (前に実行している場合は不要)
TW.data <- transform(read.csv("data/tokyo_weather_reg.csv"),
		     month=as.numeric(months(as.Date(date), 
					     abbreviate=TRUE)))
TW.subset  <- subset(TW.data,
		     subset= month %in% c(9,10,11),
		     select=c(temp,humid,month))
## 判別関数を作成
TW.lda <- lda(month ~ temp + humid, data=TW.subset)
table(true=TW.subset$month, pred=predict(TW.lda)$class) # 真値と予測値の比較
plot(TW.lda, col=TW.subset$month) # 判別関数値の図示

## 12ヶ月分のデータを用いる
## 数が多いのでサンプリングする
idx <- sample(nrow(TW.data), 100)
TW.multi <- lda(month ~ temp + solar + wind + humid,
		data=TW.data[idx,])
plot(TW.multi, col=TW.data[idx,]$month)
## 特徴量は説明変数の数までしか作成できないので，精度は低いことがわかる
