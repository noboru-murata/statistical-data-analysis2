### 第4講 サンプルコード
library(tidyverse)
if(Sys.info()["sysname"] == "Darwin") { # MacOSか調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))}

#' @exercise 東京の気候データを用いた例

#' データの読み込み
tw_subset <- read_csv("data/tokyo_weather.csv") |>
  filter(month == 8) |> # 8月のデータの抽出
  mutate(date=date(paste(year,month,day,sep="-")), .before = 1) |>
  select(-c(year,month,day,day_of_week))

#' モデル式
tw_model1 <- temp ~ press
tw_model2 <- temp ~ solar
tw_model3 <- temp ~ press + solar
tw_model4 <- temp ~ press + solar + humid
tw_model5 <- temp ~ press + solar + cloud

#' 推定
tw_lm1 <- lm(tw_model1, data=tw_subset, y=TRUE)
tw_lm2 <- lm(tw_model2, data=tw_subset, y=TRUE)
tw_lm3 <- lm(tw_model3, data=tw_subset, y=TRUE)
tw_lm4 <- lm(tw_model4, data=tw_subset, y=TRUE)
tw_lm5 <- lm(tw_model5, data=tw_subset, y=TRUE)

#' データの表示
#' 列名を日本語にして表示する
foo <- set_names(names(tw_subset), # 列名に対応する日本語ベクトルを用意
                    c("日付","気温","降雨","日射","降雪","風向","風速","気圧","湿度","雲量"))
(bar <- tw_subset |> rename(all_of(foo)))

View(bar) # 左上ペインに表として表示

#' 関連データの散布図
tw_subset |>
  select(temp,press,solar,humid,cloud) |>
  GGally::ggpairs(columnLabels=c("気温","気圧","日射","湿度","雲量"))

#' モデル1の推定結果
tw_subset |>
  ggplot(aes(x = press, y = temp)) +
  geom_point(colour = "brown", shape = 20) +
  geom_smooth(method = lm, se = FALSE, colour = "blue") +
  labs(x = "気圧", y = "気温")

#' モデル2の推定結果
tw_subset |>
  ggplot(aes(x = solar, y = temp)) +
  geom_point(colour = "brown", shape = 20) +
  geom_smooth(method = lm, se = FALSE, colour = "blue") +
  labs(x = "気圧", y = "気温")

#' モデル3の推定結果
if(Sys.info()[["sysname"]] == "Darwin") par(family = "HiraginoSans-W4")
s3d <- scatterplot3d::scatterplot3d( 
                        tw_subset[c("press","solar","temp")], # x,y,z の順
                        type="p", # plotの種類: "p"点，"l"線，"h"足付き
                        pch=16,# 点の種類 (?points 参照)
                        angle=45, # xy平面の見る方向 (適宜調整)
                        zlim=c(20,35),
                        color="brown",
                        xlab="気圧", ylab="日射", zlab="気温",
                        #'    highlight.3d=TRUE # 高さ(z)ごとに色を変える
                        )
s3d$plane3d(
      tw_lm3, col="blue", # 回帰式の定める平面の追加
      draw_polygon=TRUE, # 平面の塗り潰しの設定
      polygon_args=list(col=rgb(0,0,1,0.1)))

#' 観測値とあてはめ値の比較
tw_subset |>
  mutate(モデル1 = fitted(tw_lm1),    # モデルごとに予測値をデータフレームに追加
         モデル2 = fitted(tw_lm2),
         モデル3 = fitted(tw_lm3),
         モデル4 = fitted(tw_lm4),
         モデル5 = fitted(tw_lm5)) |>
  pivot_longer(starts_with("モデル"), # モデルをラベルとして予測値をまとめる
               names_to = "model", values_to = "fitted") |>
  ggplot(aes(x = temp, y = fitted)) + # 気温の実測値をx軸，予測値をy軸で表示
  geom_abline(slope = 1, intercept = 0, colour = "red") + # 基準線
  geom_point(aes(colour = model, shape = model)) + # 予測値をモデル別に表示
  labs(x = "気温", y = "あてはめ値") +
  xlim(22,32) + ylim(22,32) + theme(legend.position = c(.88,.15))

#' 関数 stargazer::stargazer() を利用して結果をまとめる
stargazer::stargazer(tw_lm1, tw_lm2, tw_lm3, tw_lm4, tw_lm5, # 既定値を '#' の後に記している
                     column.labels = c("モデル1","モデル2","モデル3","モデル4","モデル5"), # NULL,
                     covariate.labels = c("気圧","日射","湿度","雲量"), # NULL,
                     dep.var.caption = "目的変数", # NULL,
                     dep.var.labels = "気温", # NULL,
                     ## dep.var.labels.include = TRUE,
                     keep.stat = c("rsq","adj.rsq"), # NULL,
                     ## model.names = NULL,
                     model.numbers = FALSE, # NULL,
                     ## object.names = FALSE,
                     omit.table.layout = "n", # NULL, # "sn"
                     report = "vcs", # NULL,
                     single.row = TRUE, # FALSE,
                     title = "寄与率によるモデルの比較",
                     type = "text")
#' 'type = "html" または "latex"' など形式を選択することができる
#' それ以外のオプションの設定で様々な表を出力することができる

#' @exercise 人工データによる推定量の性質の確認

set.seed(987) # 乱数のシード値を設定
x_obs <- tibble(x0 = 1, x1 = c(1,3,5,7)) # 説明変数の観測値
epsilon <- rnorm(nrow(x_obs), sd = 0.5) # 誤差項の生成
beta <- c(2, -3) # 回帰係数
toy_data <- x_obs |> # 目的変数の観測値を追加
  mutate(y = as.vector(as.matrix(x_obs) %*% beta) + epsilon)
toy_lm <- lm(y ~ x1, data = toy_data) # 回帰係数の推定
coef(toy_lm) # 回帰係数の取得
summary(toy_lm) # 分析結果の概要の表示

#' @notes
#' 上記の例では行列とベクトルの積を用いているが
#' 以下のように説明変数・目的変数を個別に作成してもよい
x_obs <- c(1,3,5,7) # 説明変数の観測値
epsilon <- rnorm(length(x_obs), sd = 0.5) # 誤差項の生成
y_obs <- 2 - 3 * x_obs + epsilon # 目的変数の観測値
toy_data <- tibble(x1 = x_obs, y = y_obs) # データフレームの作成

#' ---------------------------------------------------------------------------
#' @practice 推定量の性質を調べる数値シミュレーション (Monte-Carlo法)

#' 人工データによる確認
set.seed(2468) # 乱数のシード値 (適宜変更せよ)

#' 試行の設定
x_obs <- tibble(x0 = 1, # 説明変数の観測値
                x1 = c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4)) 
beta <- set_names(c(-1, 2), # 回帰係数 (切片,係数の真値)
                  c("beta0","beta1"))
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)を設定
mc_trial <- function(){ 
  epsilon <- rnorm(nrow(x_obs), sd = sigma) # 誤差項の生成
  toy_data <- x_obs |> # 目的変数の観測値を追加
    mutate(y = as.vector(as.matrix(x_obs) %*% beta) + epsilon)
  toy_lm <- lm(y ~ x1, data = toy_data) # 回帰係数の推定
  return(coef(toy_lm)) # 推定された係数だけ返す
}

#' 数値実験 (少数で確認してみる)
mc <- 5 # 実験回数
replicate(mc, mc_trial())

#' 数値実験
mc <- 5000 # 実験回数
mc_data <- replicate(mc, mc_trial()) |> # mc回試行を行う
  t() |> as_tibble() # 得られる結果を転置してデータフレームにする
names(mc_data) <- names(beta) # 列名を変更(上書き)

#' 回帰係数の分布(2次元)
mc_data |>
  ggplot(aes(x = beta0, y = beta1)) +
  geom_point(colour = "blue", shape = 20) + # 推定値の散布図
  geom_vline(xintercept = beta["beta0"], colour = "orchid") + # beta0の真値 (垂直線)
  geom_hline(yintercept = beta["beta1"], colour = "orchid")   # beta1の真値 (水平線)

#' @notes
#' 軸名をギリシャ文字にしたい場合は以下を加えればよい
last_plot() + # 直前のプロットを指す
  labs(x = expression(beta[0]), y = expression(beta[1]))
#' 周辺分布のヒストグラムを追加する場合は関数 ggExtra::ggMarginal() が利用できる
#' 必要であれば install.packages("ggExtra") を実行
ggExtra::ggMarginal(last_plot(), type = "histogram")
#' 各回帰係数の周辺分布は以下のようにしても描ける
beta_cov <- sigma^2 * solve(crossprod(as.matrix(x_obs))) # 推定量の共分散行列
#' beta0 (k=1), beta1 (k=2)
for(k in 1:2) { # 同じ処理であればfor文などの利用を推奨
  bar <- tibble(x = mc_data[[k]]) |>
    ggplot(aes(x = x)) + 
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "lightblue", colour = "blue") +
    geom_vline(xintercept = beta[k], # 真の値
               colour = "orchid") + 
    geom_function(fun = \(x) dnorm(x,
                                   mean = beta[k],
                                   sd = sqrt(beta_cov[k, k])),
                  colour = "orchid") + # 理論分布
    labs(x = names(mc_data)[k])
  print(bar) # for 文などの block 内でのグラフ表示は明示的に print する
}
#' あるいは変数名を評価して列を指定することもできる
for(k in 1:2) {
  foo <- sym(names(mc_data)[k]) # 文字列を扱うための手続き
  bar <- mc_data |>
    ggplot(aes(x = !!foo)) + # foo に入った文字列を評価する
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "lightblue", colour = "blue") +
    geom_vline(xintercept = beta[k], colour = "orchid") + # 真の値
    geom_function(fun = \(x) dnorm(x,
                                   mean = beta[k],
                                   sd = sqrt(beta_cov[k, k])),
                  colour = "orchid") # 理論分布
  print(bar)
}

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice 標準誤差の性質

#' 人工データによる標準誤差と真の誤差の比較
set.seed(1313) # 乱数のシード値 (適宜変更せよ)

#' 試行の設定 (重回帰，以下適宜変更せよ)
x_obs <- tibble(x0 = 1,
                x1 = c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4), # 説明変数1
                x2 = c(3, 19, 1, 4, 18, 7, 2, 10, 6, 12)) # 説明変数2
beta <- c(-1, 2, -3) # (切片，x1の係数, x2の係数)
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)
mc_trial <- function(){ 
    epsilon <- rnorm(nrow(x_obs), sd = sigma) # 誤差項
    toy_data <- x_obs |> # 目的変数の観測値を追加
      mutate(y = as.vector(as.matrix(x_obs) %*% beta) + epsilon)
    toy_lm <- lm(y ~ x1 + x2, data = toy_data) # 回帰係数の推定
    return(set_names(summary(toy_lm)$coef[,"Std. Error"], # 標準誤差を
                     c("beta0.se","beta1.se","beta2.se"))) # 名前を付けて返す
}

#' 数値実験
mc <- 5000 # 実験回数
mc_data <- 
  replicate(mc, mc_trial()) |> # mc回の試行
  t() |> as_tibble() # データフレームの作成

#' 各回帰係数の標準誤差の分布
beta_cov <- sigma^2*solve(crossprod(as.matrix(x_obs))) # 推定量の共分散行列
#' beta0 (k=1), beta1 (k=2), beta2 (k=3)
for(k in 1:3) {
  bar <- tibble(x = mc_data[[k]]) |>
    ggplot(aes(x = x)) + 
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "lightblue", colour = "blue") +
    geom_vline(xintercept = sqrt(beta_cov[k,k]), # 真の値
               colour = "orchid") +
    labs(x = names(mc_data)[k], main = "std. errors")
  print(bar) 
}

#' 広告費と売上データによる分析

#' データの読み込み
adv_data <- read_csv('https://www.statlearning.com/s/Advertising.csv')

#' モデルの推定
adv_lm1 <- lm(sales ~ TV, data=adv_data)
adv_lm2 <- lm(sales ~ radio, data=adv_data)
adv_lm3 <- lm(sales ~ TV + radio, data=adv_data)

#' 推定値とその標準誤差
summary(adv_lm1)$coef[,1:2] 
summary(adv_lm2)$coef[,1:2] 
summary(adv_lm3)$coef[,1:2]

#' 東京の気候データによる分析

#' データの整理 (8月のデータの抽出)
tw_subset <-
  read_csv("data/tokyo_weather.csv") |>
  filter(month == 8)

#' 回帰モデルの設定
tw_model1 <- temp ~ solar
tw_model2 <- temp ~ solar + press
tw_model3 <- temp ~ solar + press + cloud

#' 回帰モデルの推定
tw_lm1 <- lm(tw_model1, data=tw_subset, y=TRUE)
tw_lm2 <- lm(tw_model2, data=tw_subset, y=TRUE)
tw_lm3 <- lm(tw_model3, data=tw_subset, y=TRUE)

#' モデルの推定値とその標準誤差は以下のとおり
summary(tw_lm1)$coef[,c("Estimate","Std. Error")]
summary(tw_lm2)$coef[,1:2] # 名前ではなく列番号で指定する場合
summary(tw_lm3)$coef[,1:2] # cloud の標準誤差が大きく精度が悪いことが示唆される

#' ---------------------------------------------------------------------------

#' ---------------------------------------------------------------------------
#' @practice t統計量の性質

#' 人工データによる確認
set.seed(2525) # 乱数のシード値 (適宜変更せよ)

#' 試行の設定 (重回帰，以下適宜変更せよ)
x_obs <- tibble(x0 = 1,
                x1 = c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4), # 説明変数1
                x2 = c(3, 19, 1, 4, 18, 7, 2, 10, 6, 12)) # 説明変数2
beta <- c(-1, 2, 0) # (切片，x1の係数，x2の係数) 
#' x1の係数 2 : 帰無仮説に従わない
#' x2の係数 0 : 帰無仮説に従う 
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)
mc_trial <- function(){ 
  epsilon <- rnorm(nrow(x_obs), sd = sigma) # 誤差項
  toy_data <- x_obs |> # 目的変数の観測値を追加
    mutate(y = as.vector(as.matrix(x_obs) %*% beta) + epsilon)
  toy_lm <- lm(y ~ x1 + x2, data = toy_data) # 回帰係数の推定
  return(set_names(summary(toy_lm)$coef[,"t value"], # t統計量を返す
                   c("beta0.tval","beta1.tval","beta2.tval"))) 
}

#' 数値実験
mc <- 5000 # 実験回数
mc_data <- 
  replicate(mc, mc_trial()) |> t() |> as_tibble()
#' 各回帰係数のt統計量の分布
n <- length(x_obs1) # データ数 n
p <- 2 # 説明変数の次元
#' beta0 (k=1), beta1 (k=2), beta2 (k=3)
for(k in 1:3) { # 同じ処理であればfor文などの利用を推奨
  bar <- tibble(x = mc_data[[k]]) |>
    ggplot(aes(x = x)) + 
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "lightblue", colour = "blue") +
    geom_function(fun = \(x) dt(x, df = n-p-1), # 自由度 n-p-1 のt分布
                  colour = "orchid") + 
    labs(x = names(mc_data)[k])
  print(bar) # for 文などの block 内でのグラフ表示は明示的に print する
}

#' 広告費と売上データによる分析

#' 全てを用いたモデルと newspaper を除いたモデルを比較する
summary(lm(sales ~ ., data = adv_data)) # "." は全て
summary(lm(sales ~ . -newspaper, data = adv_data)) # "-" は除外

#' @notes
#' newspaperの係数のt統計量から有意性は低いと考えられる
#' 自由度調整済決定係数も除いた方が高くなることが確認できる

#' 東京の気候データによる分析

#' solarとpressを用いたモデルを比較する
summary(lm(temp ~ press, data = tw_subset))
summary(lm(temp ~ solar + press, data = tw_subset))
summary(lm(temp ~ solar, data = tw_subset))

#' @notes
#' press単体では係数の推定精度も決定係数も低いが
#' solarと組み合わせることにより精度が上がり説明力も高くなる
#' また組み合わせた方が自由度調整済決定係数はsolar単体より大きくなる

#' ---------------------------------------------------------------------------

#' F統計量とその自由度は以下のようにして取り出せる
data_lm <- lm(formula, data)
summary(data_lm)$fstat
summary(data_lm)$fstatistic # 省略しない場合

#' ---------------------------------------------------------------------------
#' @practice F統計量の性質

#' 人工データによる確認
set.seed(2525) # 乱数のシード (適宜変更せよ)

#' 試行の設定 (重回帰，以下適宜変更せよ)
x_obs <- tibble(x0 = 1,
                x1 = c(1, 20, 13, 9, 5, 15, 19, 8, 3, 4), # 説明変数1
                x2 = c(3, 19, 1, 4, 18, 7, 2, 10, 6, 12)) # 説明変数2
beta <- c(-1, 0, 0) # (切片, x1の係数, x2の係数)
#'  x1,x2 の係数はどちらも0なので帰無仮説が成り立つ
sigma <-  sqrt(2) # 誤差の標準偏差(分散の平方根)
mc_trial <- function(){ 
  epsilon <- rnorm(nrow(x_obs), sd = sigma) # 誤差項
  toy_data <- x_obs |> # 目的変数の観測値を追加
    mutate(y = as.vector(as.matrix(x_obs) %*% beta) + epsilon)
  toy_lm <- lm(y ~ x1 + x2, data = toy_data) # 回帰係数の推定
  return(set_names(summary(toy_lm)$fstat[1], "fstat")) # F統計量を返す
}

#' 数値実験 (帰無仮説が成り立つ場合)
mc <- 5000 # 実験回数
mc_data <- 
  replicate(mc, mc_trial()) |> as_tibble_col("fstat")
#' 1次元なので転置は不要．ただし列名の設定が必要

#' モデルのF統計量の分布
n <- nrow(x_obs) # データ数
p <- 2 # 説明変数の次元
mc_data |>
  ggplot(aes(x = fstat)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "lightblue", colour = "blue") +
  geom_function(fun = \(x) df(x, df1 = p, df2 = n-p-1), # 自由度p,n-p-1のF-分布
                colour = "orchid") +
  labs(x = "F statistic", title="null hypothesis is true")

#' 数値実験 (帰無仮説が成り立たない場合)
beta <- c(-1, 2, 0) # x1の係数 : 帰無仮説が成り立たない
mc_data <-
  replicate(mc, mc_trial()) |> as_tibble_col("fstat") 

#' モデルのF統計量の分布は帰無分布に従わない
mc_data |>
  ggplot(aes(x = fstat)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "lightblue", colour = "blue") +
  geom_function(fun = \(x) df(x, df1 = p, df2 = n-p-1), # 自由度p,n-p-1のF-分布
                colour = "orchid") +
  labs(x = "F statistic", title="null hypothesis is false")

#' 広告費と売上データによる分析の例

#' 説明変数1つのモデルを検討する
summary(lm(sales ~ TV, data = adv_data)) 
summary(lm(sales ~ radio, data = adv_data)) 
summary(lm(sales ~ newspaper, data = adv_data))

#' @notes
#' radio, newspaper は決定係数は小さく説明力は無いが，
#' F-stat はそれなりに小さいのでモデルの有効性は無いとは言えない

#' 東京の気候データによる分析の例

#' press, solar, rain によるモデルを検討する
summary(lm(temp ~ press, data = tw_subset))
summary(lm(temp ~ press + solar, data = tw_subset))
summary(lm(temp ~ press + solar + rain, data = tw_subset))

#' @notes
#' press のみではモデルの有効性があるとは言えないが
#' solar と組み合わせることにより有効性が確認できる
#' rain を加えても press の係数に変化は見られないが
#' solar の係数が変化し決定係数が大きくなることから
#' solar と rain が相補的にモデルの精度を上げている可能性が示唆される

#' ---------------------------------------------------------------------------

#' 関数 stats::lm() による推定結果の診断プロット
tw_lm6 <- lm(temp ~ press + solar + rain, data = tw_subset)
#' 関数 ggfortify::autoplot() を利用する
#' 必要であれば 'install.packages("ggfortify")' を実行
library(ggfortify)
autoplot(tw_lm6)
#' 診断プロットは1から6まで用意されており 1,2,3,5 がまとめて表示される
#' 個別に表示する場合は 'autoplot(tw_lm6, which = 1)' のように指定する
#' 詳細は '?ggfortify::autoplot.lm' を参照
