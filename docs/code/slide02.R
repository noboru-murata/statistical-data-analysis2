### 
### 第2講 サンプルコード
###

#### "データフレーム"

### データフレームの作成

## 同じ長さのベクトルを並べる
## (... <- ...) は代入した結果を表示
(foo <- data.frame(one=c(1,2,3),two=c("AB","CD","EF"))) 
(bar <- data.frame(three=c("x","y","z"),four=c(0.9,0.5,-0.3)))
## データフレームを結合する
(baz <- cbind(foo,bar)) # column bind

### 
### 練習問題 データフレームの作成
###

## 各項目が同じ長さのベクトルを並べる
(grade <- data.frame( # 変数名は自由に決めてよい
   math=c(90,80,70,60,50),
   phys=c(25,50,75,100,80),
   chem=c(65,100,70,40,75),
   bio =c(70,50,30,80,100)))

## 行・列の名前の操作
rownames(grade) # 行の名前を表示する
rownames(grade) <- c("A","B","C","D","E") # 行の名前を変更する
names(grade)    # 列の名前を表示する
grade # 変更されたデータフレームを表示する
## データの取り出し方 (後ほど詳しく)
grade["B","phys"] # 特定の要素を名前で参照する
grade["C",] # 特定の行を表示
grade["bio"] # 特定の列を表示 (データフレームになる)
grade[,"bio"] # 特定の列を表示 (ベクトルになる)
grade[["bio"]] # 上記と同じ操作 (リストと同様な扱い)
grade$bio # 上記と同じ操作

#### "ファイルの操作"

### 
### 練習問題 ファイルの読み書き
###   前の練習問題で作ったデータフレームを利用する
###

### csv形式の操作
## 関数 write.csv の使い方 (ファイルの書き出し)
dim(grade) # 大きさを確認
write.csv(grade, file="data/mydata.csv")
## 関数 read.csv の使い方 (ファイルの読み込み)
(grade2 <- read.csv(file="data/mydata.csv", row.names=1)) 
dim(grade2) # 大きさが一致していることを確認
grade2["A","math"] <- 100 # データの一部を修正
grade2 # 中身を確認しておく

### Rdata形式の操作
## 関数 save の使い方 (ファイルの書き出し)
save(grade, grade2, file="data/mydata.RData") 
## 関数 load の使い方 
grade <- c(1,2,3) # 上書きしておく
grade # 中身を確認しておく
rm(grade2) # 消しておく
grade2 # "オブジェクト 'grade2' がありません"というエラーになる 
load(file="data/mydata.RData") # RData形式の読み込み
grade # save したときの grade が復元されている
grade2 # save したときの grade2 が復元されている

### download したファイルの読み込み
## ファイル名 pcr_case_daily.csv として作業ディレクトリの data に保存
pcr_data <- read.csv(file="data/pcr_case_daily.csv") # 一般的な読み込み方
## 読み込めない場合は文字コードを指定する
## read.csv(file="data/pcr_case_daily.csv")
##          fileEncoding="shift-jis") # 文字コードの指定 (shift-jis/utf-8)
pcr_colname <- names(pcr_data) # 機関名などの列名を保存しておく
names(pcr_data) <- c("date",letters[1:(length(pcr_colname)-1)]) # 英語に付け替える
head(pcr_data) # 中身を確認する
## 読み込み時に列名を指定することも可能 (以下は上記と同じ結果)
pcr2_data <- read.csv(file="data/pcr_case_daily.csv",
                 col.names=c("date",letters[1:(length(pcr_colname)-1)]))
head(pcr2_data) # 中身を確認する
## Filesタブの操作で読み込みことも可能なので確認しなさい
## ただし tibble+data.frame オブジェクトになるので若干扱いが異なる
## URLを指定して読み込むこともできる (更新される情報を追い掛ける場合に利用を推奨)
## pcr_data <- read.csv("https://www.mhlw.go.jp/content/pcr_case_daily.csv")

#### "データフレームの操作"

### 要素の選択

z <- data.frame(one=c(1,2,3), two=c("AB","CD","EF"), three=6:8)
z[1,2] # 1行2列の要素を選択
z[-c(1,3),] # 1,3行を除外
z[c(TRUE,FALSE,TRUE),] # 1,3行を選択
z[,"two"]  # 列名"two"を選択(ベクトルになる)
z[["two"]] # 上記と同様の結果となる(1列の場合しか使えない)
z$two      # 上記と同様の結果となる(1列の場合しか使えない)
z["two"]   # 列名"two"を選択(1列のデータフレームになる)
z[,c("one","three")] # 列名"one"と"three"を選択(データフレームになる)
z[c("one","three")]   # 上記と同様の結果となる

## 国立感染症研究所(a)の検査件数が 0 でないデータ
subset(pcr_data, subset= a!=0) # subset オプションに条件を指定する

## 検疫所(b)と民間検査会社(d)の検査件数データ
subset(pcr_data, select= c(b,d)) # select オプションに列名を指定する

### 
### 練習問題 データフレームの操作
###

## 関数subsetの使い方
## 医療機関(f)での検査数が2000を越えたときの国立感染症研究所(a)と医療機関(f)のデータ
subset(pcr_data,       # データフレーム
       subset= f>2000, # 条件による絞り込み
       select= c(a,f)) # 列の選択
## 大学等(e)と医療機関(f)でともに検査件数が2000を越えたデータ
subset(pcr_data,                # データフレーム
       subset= e>2000 & f>2000) # 複合的な条件の指定

#### "データフレームの集約"

### 関数 apply() の使用例
## 学生の成績表 grade の各教科の平均値

apply(X=grade,  # データフレーム
      MARGIN=2, # 列ごとの処理
      FUN=mean) # 処理内容の指定 (関数)
apply(grade, 2, mean) # 上記と同じ(変数名は省略可能)

### 関数 aggregate() の使用例
## 月毎の医療機関のPCR数の集計
library(lubridate) # 関数 month() などを利用可能とする
pcr_data$date # 日付を取り出す
month(pcr_data$date) # 月を取り出す
transform(pcr_data,month=month(date)) # 列を追加
## 注意:
## transform は pcr_data の操作をしているので，dateだけで列名が指定できる
## lubridate の関数 month() は月名での表示も可能
## ただし名前は言語環境に依存するので注意
## month(foo,label=TRUE) # 短縮形
## month(foo,label=TRUE,abbr=FALSE) # 月名
## 強制的に英語にするには
## Sys.setlocale(category = "LC_TIME", locale="C")
## などを指定すればよい．もとに戻すには
## Sys.setlocale(category = "LC_TIME", locale="")

aggregate(formula= f ~ month,      # 式による集計の指定
          data=transform(pcr_data, # データフレームの書き換え
                         month=month(date)),
          FUN=sum)                 # 集計内容の指定 (関数)

### 
### 練習問題 データフレームの集約
###

## 関数applyの使い方
## 各機関でのPCR検査件数の最大値
apply(subset(pcr_data, select= -date), # date は文字列なので集計から除く
      2, # 列の計算
      max, na.rm=TRUE) # max の計算で NA を除く

## 関数aggregateの使い方
## 2020年の月ごとの各機関でのPCR検査件数の最大値
aggregate(cbind(a,b,c,d,e,f,g) ~ month,
          transform(subset(pcr_data, year(date)==2020),
                    month=month(date)),
          max,
          na.action=na.pass) # NAだけの列があっても集計するための指定
aggregate(. ~ date, # ちょっとした細工(上書き)で簡単になる
          transform(subset(pcr_data, year(date)==2020),
                    date=month(date)),
          max, na.action=na.pass)

## datasets::mtcars での例
## 気筒数ごとに排気量の最大値，最小値
aggregate(disp ~ cyl,
          mtcars,
          max) 
aggregate(disp ~ cyl,
          mtcars,
          min)
aggregate(disp ~ cyl, # まとめて計算することも可能
          mtcars,
          function(x){return(c(max=max(x),min=min(x)))})
## 気筒数とギア数ごとの燃費の平均値
aggregate(mpg ~ cyl + gear, # 条件を並べる場合は + を用いる
          mtcars,
          mean)

#### "描画の基礎"

### 関数 plot() の使用例 (ベクトル) 
## 民間検査会社(d)と医療機関(f)の検査件数の関係

plot(pcr_data$d, # データフレームからD列のベクトルを抽出
     type="l", col="blue") # 線での描画と色を指定
lines(pcr_data$f, col="red") # 線を重ね描き

## 複数のデータを同時に描画する方法も用意されている
matplot(pcr_data[-1], # データフレームから1列目を取り除いたデータフレームを作成
        type="l") # 線での描画を指定，色も個別に指定できる

### 関数 plot() の使用例 (散布図) 
## 国立感染症研究所(a)と医療機関(f)の検査件数の関係

## MacOSかどうか調べて日本語フォントを指定する
if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
plot(f ~ a, data=pcr_data, # y軸=f，x軸=a で散布図を作成
     col="blue", pch=19, # 色と形を指定
     xlab=pcr_colname[2], ylab=pcr_colname[7]) # 軸の名前を指定

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
## x軸を日付とすることで日付と検査数の関係を表すことも可能
plot(f ~ as.Date(date), data=pcr_data, # 線で描画する
     type="l", col="red", # 色と形を指定
     xlab=pcr_colname[1], ylab=pcr_colname[7], # 軸の名前を指定
     main="PCR検査件数の推移")

### 関数 plot() の使用例 (散布図行列) 
## 各検査機関での件数の関係

plot(pcr_data[-1], col="blue", pch=19) # データフレームから1列目を除いて描画

### 
### 練習問題 基本的なグラフの描画
###  書き方はいろいろあるので，以下はあくまで一例
###

## 検疫所(b)，地方衛生研究所.保健所(c)，民間検査会社(d)における検査件数の推移
apply(pcr_data[-1],2,max,na.rm=TRUE) # 最大値を確認しておく
plot(d ~ as.Date(date), data=pcr_data, # 最大値を基準に描画は行われる
     type="l", col="orchid", xlab="日付",ylab="検査件数")
lines(b ~ as.Date(date), data=pcr_data, col="orange")
lines(c ~ as.Date(date), data=pcr_data, col="tomato")
## y軸を対数表示する場合には以下のとおり
plot(d ~ as.Date(date), data=pcr_data, log="y", # y軸を対数変換
     type="l", col="orchid", xlab="日付",ylab="検査件数")
lines(b ~ as.Date(date), data=pcr_data, col="orange")
lines(c ~ as.Date(date), data=pcr_data, col="tomato")
## log(0) の計算で警告が出る場合がある

## 民間検査会社(d)，大学等(e)，医療機関(f)での検査件数の関係(散布図)
plot(pcr_data[c("d","e","f")], # 必要なデータフレームを抽出
     labels=pcr_colname[5:7], # 変数名を日本語に変更
     col="blue", pch=18) # pch については help(points) を参照
plot(~ d + e + f, data=pcr_data, # 式を使った指定の方法の例
     labels=pcr_colname[5:7], col="blue", pch=18)

#### "さまざまなグラフ"

### 関数 hist() の使用例
## 民間検査会社(d)での検査件数の分布

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
hist(pcr_data$d, breaks=25, labels=TRUE, # ビンの数と度数表示を指定
     col="lightblue", border="blue", # 中と境界の色を指定
     main="検査件数のヒストグラム", xlab=pcr_colname[5]) # 軸の名前を指定

### 関数 boxplot() の使用例
## 大学等(e)での検査件数の分布(2021年分)

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
boxplot(e ~ date,
        data=transform(subset(pcr_data, year(date)==2021),
                       date=month(date)),
        col="orange", main="月ごとの検査件数")

### 関数 barplot() の使用例
## 機関ごとの月の検査件数の推移 (2021年分)

if(Sys.info()["sysname"]=="Darwin"){par(family="HiraginoSans-W4")}
foo <- aggregate(. ~ date, # 集計したデータを保存
                 transform(subset(pcr_data,
                                  subset = year(date)==2021,
                                  select = 1:7),
                           date=month(date)),
                 sum, na.action=na.pass)
barplot(as.matrix(foo[-1]), col=rainbow(12), # 作成した月の色を利用
        names.arg=pcr_colname[2:7], # 変数名を日本語で表示
        beside=TRUE, space=c(.3,3), # 横並びの指定とスペースの設定
        legend.text=foo[,1], args.legend=list(ncol=2)) # 凡例の指定

#### "疑似乱数"

### 
### 練習問題 擬似乱数
###

## 関数sampleの使い方
(x <- 1:10)   # サンプリング対象の集合を定義
set.seed(123) # 乱数のシード値(任意に決めてよい)を指定
sample(x, 5)               # xから5つの要素を重複なしでランダムに抽出
sample(x, 5, replace=TRUE) # xから5つの要素を重複ありでランダムに抽出
sample(x, length(x))       # xの要素のランダムな並べ替え
sample(1:6, 10, replace=TRUE)           # サイコロを10回振る実験の再現
sample(1:6, 10, prob=6:1, replace=TRUE) # 出る目の確率に偏りがある場合

## 関数rbinomの使い方
rbinom(10, size=4, prob=0.5) # 表(1)の出る確率が0.5にコインを4枚投げる試行を10回
rbinom(20, size=4, prob=0.2) # 個数を20, 確率を0.2に変更

## 関数runifの使い方
runif(5, min=-1, max=2) # 区間(-1,2)上の一様乱数を5個発生
runif(5)                # 指定しない場合は区間(0,1)が既定値

## 関数rnormの使い方
rnorm(10, mean=5, sd=3) # 平均5，分散3^2の正規乱数を10個発生
rnorm(10)               # 指定しない場合は mu=0, sd=1 が既定値

## 関数set.seedについて
set.seed(1) # 乱数の初期値をseed=1で指定
runif(5) 
set.seed(2) # 乱数の初期値をseed=2で指定
runif(5)    # seed=1の場合と異なる結果
set.seed(1) # 乱数の初期値をseed=1で指定
runif(5)    # 初めのseed=1の場合と同じ結果

#### "モンテカルロ法"

### 中心極限定理

## 確率変数の分布の設定 (例 : 区間[-1,1]の一様乱数)
myrand <- function(n) { # n個の乱数を生成
  return(runif(n,min=-1,max=1))
}
## 標本平均の計算
mymean <- function(n) { # n個のデータで計算
  return(mean(myrand(n)))
}

## Monte-Carlo実験
set.seed(123) # 実験を再現したい場合はシードを指定する
mu <- 0; sigma <- sqrt(1/3) # 理論平均と標準偏差
mc <- 5000 # 実験の繰り返し回数
for(n in c(1,2,4,8,16)){ # nを変えて実験
  xbars <- replicate(mc, mymean(n)) # mc回実験し標本平均を記録
  hist(xbars, breaks=25, freq=FALSE, # 分布を表示
       col="orchid", border="slateblue",
       xlab=expression(bar(X)), main=paste0("n=",n))
  thdist <- function(x){dnorm(x,mean=mu,sd=sigma/sqrt(n))}
  curve(thdist, add=TRUE, col="orange", lwd=2) # 理論曲線を重ねる
}

### コイン投げの賭け

## コイン投げの試行 (いろいろな書き方があるので以下は一例)
my_trial <- function(){
  while(TRUE){ # 永久に回るループ
    if(rbinom(1,size=1,prob=0.5)==1){return("A")} # Aが表で終了
    if(rbinom(1,size=1,prob=0.5)==1){return("B")} # Bが表で終了
    ## どちらも裏ならもう一度ループ
  }
}

## Monte-Carlo実験
set.seed(8888) # 実験を再現したい場合はシードを指定する
mc <- 10000 # 実験回数を設定 
my_data <- replicate(mc,my_trial()) 
## 簡単な集計
table(my_data)    # 頻度
table(my_data)/mc # 確率(推定値)

### 
### 練習問題 双六ゲーム
###

## 双六の試行
my_trial <- function(){
  step <- 0 # 最初の位置
  num <- 0  # さいころを振る回数
  while(TRUE){ # 永久に回るループ
    step <- step + sample(1:6,1) # さいころを振る
    num <- num + 1 # 回数を記録
    if(step >= 100) { # ゴールしたか?
      return(num) # 回数を出力して関数を終了
    }
  }
}

## 試行を行ってみる
for(i in 1:10) print(my_trial())

## Monte-Carlo実験
set.seed(12345)
mc <- 10000 # 実験回数を設定 
my_data <- replicate(mc,my_trial()) 
hist(my_data) # ヒストグラムを出力
summary(my_data) # 簡単な集計
