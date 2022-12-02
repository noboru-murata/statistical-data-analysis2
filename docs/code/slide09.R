### 
### 第9講 サンプルコード
###

### 
### 練習問題 判別結果の評価
###

### 東京の気象データによる線形判別分析
library(MASS)
library(caret)

## 10月と11月の判別の例
## データの整理
tw_data <- read.csv("data/tokyo_weather.csv")
tw_subset  <- transform(subset(tw_data,
                               subset= month %in% c("10","11"),
                               select=c(temp,humid,month)),
                        month=as.factor(month)) # 因子にする
## 判別関数を作成
tw_lda <- lda(month ~ temp + humid, data=tw_subset)
## 判別結果の評価
tw_est <- predict(tw_lda)
confusionMatrix(tw_est$class, tw_subset$month)

## 12ヶ月分のデータを用いた例 (説明変数は適宜選択せよ)
tw_subset  <- transform(subset(tw_data,
                               select=c(temp,solar,wind,humid,month)),
                        month=as.factor(month)) 
## 判別関数を作成
tw_lda <- lda(month ~ ., # 右辺の . は month 以外の全てを説明変数として指定
              data=tw_subset)
## 判別結果の評価
tw_est <- predict(tw_lda)
confusionMatrix(tw_est$class, tw_subset$month)
confusionMatrix(tw_est$class, tw_subset$month)$table
confusionMatrix(tw_est$class, tw_subset$month)$overall
confusionMatrix(tw_est$class, tw_subset$month)$byClass

## Wine Quality Data Set を用いた判別分析
wq_org <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
                   sep=";")
table(wq_org$quality) 
wq_data <- transform(wq_org,
                     quality=as.factor(
                         ifelse(quality %in% 7:10, "A",
                         ifelse(quality %in% 5:6, "B" ,"C"))))
## 判別関数を作成
wq_lda <- lda(quality ~ ., data=wq_data)
wq_qda <- qda(quality ~ ., data=wq_data)
## 判別結果の評価
confusionMatrix(predict(wq_lda)$class, wq_data$quality)
confusionMatrix(predict(wq_qda)$class, wq_data$quality)

### 
### 練習問題 予測誤差の評価
###

### MASS::biopsy による誤差の評価
library(MASS)  # 既に読み込んでいれば不要
library(caret) # 既に読み込んでいれば不要

## データの整理
head(biopsy)
tail(biopsy)
bio_data <- na.omit(biopsy)[-1] # NA および ID を除く

plot(~ ., data=bio_data , pch=20, col=class) # 散布図の表示

## LOO CV の例 (ldaは標準で装備している．qdaも同様)
## 線形判別
bio_lda <- lda(class ~ ., data=bio_data) 
bio_ldloo <- lda(class ~ ., data=bio_data, CV=TRUE)
## 訓練誤差の評価
confusionMatrix(predict(bio_lda)$class, bio_data$class)$table
## LOO CV による予測誤差の推測
confusionMatrix(bio_ldloo$class, bio_data$class)$table
## 様々な評価指標の比較する場合
## confusionMatrix(predict(bio_lda)$class, bio_data$class)
## confusionMatrix(bio_ldloo$class, bio_data$class)

## caret package を利用した場合
## LOO CV
(bio_loocv <- train(class ~., data=bio_data, method="lda",
                    trControl=trainControl(method="LOOCV")))
## k-重交叉検証 (5-fold)
(bio_kfold <- train(class ~., data=bio_data, method="lda",
                    trControl=trainControl(method="cv", number=5)))

### Wine Quality Data Set による誤差の評価
library(MASS)  # 既に読み込んでいれば不要
library(caret) # 既に読み込んでいれば不要

## データの整理 (既に整理してあれば不要)
wq_org <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
                   sep=";")
wq_data <- transform(wq_org,
                     quality=as.factor(
                         ifelse(quality %in% 7:10, "A",
                         ifelse(quality %in% 5:6, "B" ,"C"))))

## LOO CV の例 (lda/qdaは標準で装備している)
## 線形判別
wq_lda <- lda(quality ~ ., data=wq_data) 
wq_ldloo <- lda(quality ~ ., data=wq_data, CV=TRUE)
confusionMatrix(predict(wq_lda)$class, wq_data$quality)$table
confusionMatrix(wq_ldloo$class, wq_data$quality)$table
## 線形判別の過学習は微小

## 2次判別
wq_qda <- qda(quality ~ ., data=wq_data) 
wq_qdloo <- qda(quality ~ ., data=wq_data, CV=TRUE)
confusionMatrix(predict(wq_qda)$class, wq_data$quality)$table
confusionMatrix(wq_qdloo$class, wq_data$quality)$table
## 2次判別は若干過学習している

## 予測誤差の比較
confusionMatrix(wq_ldloo$class, wq_data$quality)$overall
confusionMatrix(wq_qdloo$class, wq_data$quality)$overall
## 予測誤差の観点からは線形判別の方が良さそう

## k-重交叉検証は caret package の機能を利用して求めることができる
(train(quality ~., data=wq_data, method="lda",
       trControl=trainControl(method="cv", number=10)))
(train(quality ~., data=wq_data, method="qda",
       trControl=trainControl(method="cv", number=10)))
## LOO CV も利用することができるが，計算は遅い
(train(quality ~., data=wq_data, method="lda",
       trControl=trainControl(method="LOOCV")))
(train(quality ~., data=wq_data, method="qda",
       trControl=trainControl(method="LOOCV")))
