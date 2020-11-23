### 第09回 練習問題解答例

### 練習1
### 判別結果の評価

### 東京の気象データによる線形判別分析
library(MASS)
library(caret)

## 10月と11月の判別の例
## データの整理
TW.data <- transform(read.csv("data/tokyo_weather_reg.csv"),
		     month=substr(as.Date(date),6,7)) # 月を切り出し
TW.subset  <- transform(subset(TW.data,
			       subset= month %in% c("10","11"),
			       select=c(temp,humid,month)),
			month=as.factor(month)) # 因子にする
## 判別関数を作成
TW.lda <- lda(month ~ temp + humid, data=TW.subset)
## 判別結果の評価
TW.est <- predict(TW.lda)
confusionMatrix(TW.est$class, TW.subset$month)

## 12ヶ月分のデータを用いた例 (説明変数は適宜選択せよ)
TW.subset  <- transform(subset(TW.data,
			       select=c(temp,solar,wind,humid,month)),
			month=as.factor(month)) 
## 判別関数を作成
TW.lda <- lda(month ~ ., # 右辺の . は month 以外の全てを説明変数として指定
	      data=TW.subset)
## 判別結果の評価
TW.est <- predict(TW.lda)
confusionMatrix(TW.est$class, TW.subset$month)
confusionMatrix(TW.est$class, TW.subset$month)$table
confusionMatrix(TW.est$class, TW.subset$month)$overall
confusionMatrix(TW.est$class, TW.subset$month)$byClass

## Wine Quality Data Set を用いた判別分析
WQ.org <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
		   sep=";")
table(WQ.org$quality) 
WQ.data <- transform(WQ.org,
		     quality=as.factor(
			 ifelse(quality %in% 7:10, "A",
			 ifelse(quality %in% 5:6, "B" ,"C"))))
## 判別関数を作成
WQ.lda <- lda(quality ~ ., data=WQ.data)
WQ.qda <- qda(quality ~ ., data=WQ.data)
## 判別結果の評価
confusionMatrix(predict(WQ.lda)$class, WQ.data$quality)
confusionMatrix(predict(WQ.qda)$class, WQ.data$quality)

### 練習2
### 予測誤差の評価

### Wine Quality Data Set による誤差の評価
library(MASS)  # 既に読み込んでいれば不要
library(caret) # 既に読み込んでいれば不要

## データの整理 (既に整理してあれば不要)
WQ.org <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
		   sep=";")
WQ.data <- transform(WQ.org,
		     quality=as.factor(
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
