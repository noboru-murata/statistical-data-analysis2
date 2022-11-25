### 
### 第8講 サンプルコード
###

### 
### 練習問題 線形判別
###

### 東京の気象データによる判別分析
library(MASS)
## データの整理
tw_data <- read.csv("data/tokyo_weather.csv")
tw_subset  <- subset(tw_data,
                     subset= month %in% c(10,11),
                     select=c(temp,humid,month))
idx <- seq(2,60,by = 2)
tw_train <- tw_subset[ idx,] # 訓練データ
tw_test  <- tw_subset[-idx,] # 試験データ
## 視覚化
with(tw_subset, 
     plot(temp, humid, # 散布図の作成
          pch=month, col=month,
          xlab="temperature",ylab="humidity",
          main="Oct. & Nov"))
legend("bottomright",inset=.05, # 凡例の作成
       pch=c(10,11), col=c(10,11), legend=c("Oct","Nov"))
## 訓練データで判別関数を作成．等分散性を仮定
tw_lda <- lda(month ~ temp + humid, data=tw_train)
plot(tw_lda) # 訓練データの判別関数値
tw_est <- predict(tw_lda) # 判別関数によるクラス分類結果の取得
table(true=tw_train$month, pred=tw_est$class) # 真値と予測値の比較
## 試験データによる評価
tw_pred <- predict(tw_lda, newdata=tw_test) 
table(true=tw_test$month, pred=tw_pred$class) # 真値と予測値の比較
tw_pred$class 
tw_test$month 
## 判別結果の図示
my_lda_border <- function(z) { # 判別境界を引くための関数 
    a0<-as.vector(colMeans(z$means) %*% z$scaling)
    a<-c(a0/z$scaling[2],-z$scaling[1]/z$scaling[2])
    return(a)
}
## scale * (x-mu) = 0 を変形している
## ただし x は説明変数のベクトルで，x2 に関して解いている
with(tw_test, 
     plot(temp, humid, # 試験データの散布図
          pch=month, col=month,
          xlab="temperature",ylab="humidity",
          main="Oct. & Nov"))
with(tw_train, 
     points(temp, humid, # 訓練データの散布図
            pch=month, col=month+3))
abline(my_lda_border(tw_lda), col="blue", lwd=2)
## もう少し凝った図の作り方の例
## 全データで線形判別関数を作成して誤ったデータを表示
my_color <- rainbow(12)[c(2,5,8)]
tw_lda <- lda(month ~ temp + humid, data=tw_subset)
tw_lest <- predict(tw_lda)
tw_lerr <- which(tw_lest$class!=tw_subset$month)
## 判別結果の図示
my_lda_border <- function(z) { # 判別境界を引くための関数
  a0<-as.vector(colMeans(z$means) %*% z$scaling)
  a<-c(a0/z$scaling[2],-z$scaling[1]/z$scaling[2])
  return(a)
}
with(tw_subset, 
     plot(temp, humid, # 全データの散布図
          pch=month+6, col=my_color[month-8],
          xlab="temperature",ylab="humidity",
          main="linear discriminant"))
with(tw_subset[tw_lerr,], 
     points(temp, humid, # 誤ったデータの散布図
            pch=1, col="orchid", cex=2, lwd=2))
abline(my_lda_border(tw_lda), col="orange", lwd=2)
rx <- with(tw_subset,range(temp))
ry <- with(tw_subset,range(humid))
sx <- pretty(rx,100)
sy <- pretty(ry,100)
my_grid <- expand.grid(temp=sx,humid=sy)
ldesc <- predict(tw_lda,newdata=my_grid)
image(sx,sy,add=TRUE,
      matrix(as.numeric(ldesc$class),length(sx),length(sy)),
        col=c(rgb(1,0,0,0.2),rgb(0,1,0,0.2)))

### 
### 練習問題 2次判別
### 

### 東京の気象データによる判別分析
library(MASS)
## データの整理 (前に実行している場合は不要)
tw_data <- read.csv("data/tokyo_weather.csv")
tw_subset  <- subset(tw_data,
                     subset= month %in% c(10,11),
                     select=c(temp,humid,month))
idx <- seq(2,60,by = 2)
tw_train <- tw_subset[ idx,] # 訓練データ
tw_test  <- tw_subset[-idx,] # 試験データ
## 訓練データで判別関数を作成
tw_qda <- qda(month ~ temp + humid, data=tw_train)
tw_est2 <- predict(tw_qda) # 判別関数によるクラス分類結果の取得
table(true=tw_train$month, pred=tw_est2$class) # 真値と予測値の比較
## 試験データによる評価
tw_pred2 <- predict(tw_qda, newdata=tw_test) 
table(true=tw_test$month, pred=tw_pred2$class) # 真値と予測値の比較
tw_pred2$class 
tw_test$month 
## 判別結果の図示
## 判別境界を描くのは複雑なので，色と形で代用する
with(tw_test, 
     plot(temp, humid, # 試験データの散布図
          pch=as.numeric(tw_pred2$class),
          col=month,
          xlab="temperature",ylab="humidity",
          main="Oct. & Nov"))
## 同様に少し凝った図の作り方の例
tw_qda <- qda(month ~ temp + humid, data=tw_subset) 
tw_qest <- predict(tw_qda) # 判別関数によるクラス分類結果の取得
tw_qerr <- which(tw_qest$class!=tw_subset$month)
with(tw_subset, 
     plot(temp, humid, # データ全体の散布図
          pch=month+6, col=my_color[month-8],
          xlab="temperature",ylab="humidity",
          main="quadratic discriminant"))
with(tw_subset[tw_qerr,], 
     points(temp, humid, # 誤ったデータ
            pch=1, col="orchid", cex=2, lwd=2))
qdesc <- predict(tw_qda,newdata=my_grid)
image(sx,sy,add=TRUE,
      matrix(as.numeric(qdesc$class),length(sx),length(sy)),
      col=c(rgb(1,0,0,0.2),rgb(0,1,0,0.2)))

### 
### 練習問題 多値判別
### 

### 東京の気象データによる判別分析
library(MASS)
## データの整理 (前に実行している場合は不要)
tw_data <- read.csv("data/tokyo_weather.csv")
tw_subset  <- subset(tw_data,
                     subset= month %in% c(9,10,11),
                     select=c(temp,humid,month))
## 判別関数を作成
tw_lda3 <- lda(month ~ temp + humid, data=tw_subset)
tw_est3 <- predict(tw_lda3) # 判別関数によるクラス分類結果の取得
table(true=tw_subset$month, pred=tw_est3$class) # 真値と予測値の比較
plot(tw_lda3, col=tw_subset$month) # 判別関数値の図示

## 12ヶ月分のデータを用いる
## 数が多いのでサンプリングする
idx <- sample(nrow(tw_data), 100)
tw_multi <- lda(month ~ temp + solar + wind + humid,
                data=tw_data[idx,])
plot(tw_multi, col=tw_data[idx,]$month)
## 特徴量は説明変数の数までしか作成できないので，精度は低いことがわかる

## 雨の有無を識別する例
tw_rdata <- transform(tw_data,
                      rain=factor(rain>0), # 雨の有無でラベル化する
                      month=factor(month)) # 月ごとの気候の違いの補正のため
tw_rain <- lda(rain ~ temp + solar + wind + month,
               data=tw_rdata,
               subset=idx) # 一部のデータで推定，12ヶ月分の例とは別の指定の仕方
plot(tw_rain)
tw_rpred <- predict(tw_rain, newdata=tw_rdata) # 全データを予測
table(true=tw_rdata$rain[idx], est=tw_rpred$class[idx])
table(true=tw_rdata$rain[-idx], est=tw_rpred$class[-idx])
