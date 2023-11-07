### 第5講 サンプルコード
library(tidyverse)
if(Sys.info()["sysname"] == "Darwin") { # MacOSか調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))}

#' @exercise 東京の気候データを用いた例

#' データの読み込み
tw_subset <- read_csv("data/tokyo_weather.csv") |>
  filter(month == 8) |> # 8月のデータの抽出
  mutate(date = date(paste(year, month, day, sep = "-")), .before = 1) |>
  select(-c(year, month, day, day_of_week))

#' モデル式
tw_model1 <- temp ~ press
tw_model2 <- temp ~ solar
tw_model3 <- temp ~ press + solar
tw_model4 <- temp ~ press + solar + humid
tw_model5 <- temp ~ press + solar + cloud

#' 推定
tw_lm1 <- lm(tw_model1, data = tw_subset, y = TRUE)
tw_lm2 <- lm(tw_model2, data = tw_subset, y = TRUE)
tw_lm3 <- lm(tw_model3, data = tw_subset, y = TRUE)
tw_lm4 <- lm(tw_model4, data = tw_subset, y = TRUE)
tw_lm5 <- lm(tw_model5, data = tw_subset, y = TRUE)

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

#' 決定係数のまとめ
stargazer::stargazer(tw_lm1, tw_lm2, tw_lm3, tw_lm4, tw_lm5,
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
                     ## report = "vc*st", # NULL,
                     ## single.row = TRUE, # FALSE,
                     title = "寄与率によるモデルの比較",
                     type = "text")

#' F統計量のまとめ
stargazer::stargazer(tw_lm1, tw_lm2, tw_lm3, tw_lm4, tw_lm5, # 既定値を '#' の後に記している
                     column.labels = c("モデル1","モデル2","モデル3","モデル4","モデル5"), # NULL,
                     covariate.labels = c("気圧","日射","湿度","雲量"), # NULL,
                     dep.var.caption = "目的変数", # NULL,
                     dep.var.labels = "気温", # NULL,
                     ## dep.var.labels.include = TRUE,
                     keep.stat = c("rsq","adj.rsq","ser","f"), # NULL,
                     ## model.names = NULL,
                     model.numbers = FALSE, # NULL,
                     ## object.names = FALSE,
                     ## omit.table.layout = "n", # NULL, # "sn"
                     ## report = "vc*st", # NULL,
                     single.row = TRUE, # FALSE,
                     title = "F統計量によるモデルの比較",
                     type = "text")

#' t統計量のまとめ
stargazer::stargazer(tw_lm1, tw_lm2, tw_lm3, tw_lm4, tw_lm5, # 既定値を '#' の後に記している
                     column.labels = c("モデル1","モデル2","モデル3","モデル4","モデル5"), # NULL,
                     covariate.labels = c("気圧","日射","湿度","雲量"), # NULL,
                     dep.var.caption = "目的変数", # NULL,
                     dep.var.labels = "気温", # NULL,
                     ## dep.var.labels.include = TRUE,
                     ## keep.stat = c("rsq","adj.rsq"), # NULL,
                     ## model.names = NULL,
                     model.numbers = FALSE, # NULL,
                     ## object.names = FALSE,
                     omit.table.layout = "sn", # NULL, 
                     report = "vc*stp", # NULL,
                     single.row = TRUE, # FALSE,
                     title = "t統計量によるモデルの比較",
                     type = "text")

#' 診断プロット (モデル4)
library(ggfortify)
autoplot(tw_lm4)

#' 診断プロット (モデル5)
autoplot(tw_lm5)

#' 9,10月のデータでモデルを構築し，8,11月のデータを予測
#' データの整理
tw_data <- read_csv("data/tokyo_weather.csv")
tw_train <- tw_data |> # モデル推定用データ
  filter(month %in% c(9,10)) # %in% は集合に含むかどうかを判定
tw_test  <- tw_data |> # 予測用データ
  filter(month %in% c(8,11))
#' モデルの構築
tw_model <- temp ~ solar + press # モデルの定義 
tw_lm <- lm(tw_model, data = tw_train) # モデルの推定
summary(tw_lm) # モデルの評価
#' あてはめ値の計算
tw_train_fitted <- tw_train |>
  mutate(fitted = predict(tw_lm)) # データのあてはめ値
tw_test_fitted <- tw_test  |>
  mutate(fitted = predict(tw_lm, newdata = tw_test)) # 予測

#' 予測結果を図示
bind_rows(tw_train_fitted, tw_test_fitted) |> # 2つのデータフレームを結合
  mutate(month = as_factor(month)) |> # 月を因子化して表示に利用
  ggplot(aes(x = fitted, y = temp)) +
  geom_point(aes(colour = month, shape = month)) + # 月ごとに色と形を変える
  geom_abline(slope = 1, intercept = 0, # 予測が完全に正しい場合のガイド線
              colour = "gray") +
  labs(y = "observed")

#' @notes
#' 関数 lubridate::month() を用いると月を文字列のラベルとすることもできる
bind_rows(tw_train_fitted, tw_test_fitted) |>
  mutate(month = month(month, label = TRUE)) |> # 文字にする場合
  ggplot(aes(x = fitted, y = temp)) +
  geom_point(aes(colour = month)) + # 月ごとに色を変える
  geom_abline(slope = 1, intercept = 0, # 予測が完全に正しい場合のガイド線
              colour = "gray") +
  labs(y = "observed")
#' この場合は順序付きの因子になるので，'shape' を利用すると警告が出る

#' ---------------------------------------------------------------------------
#' @practice 回帰式を用いた予測

#' 東京の気候データによる分析

#' 信頼区間と予測区間の計算
tw_data <- read_csv("data/tokyo_weather.csv")
tw_train <- tw_data |> filter(month %in% 8) # 推定用データ
tw_test  <- tw_data |> filter(month %in% 9) # 予測用データ
tw_model <- temp ~ solar + press + cloud # モデルの定義 
tw_lm <- lm(tw_model, data = tw_train) # モデルの推定

#' 信頼区間
tw_train_conf <- # あてはめ値と信頼区間を付加
  bind_cols(tw_train,
            predict(tw_lm, interval = "confidence"))
tw_test_conf <- # 新規データへのあてはめ値と信頼区間を付加
  bind_cols(tw_test, 
            predict(tw_lm, newdata = tw_test, interval = "confidence"))

#' 予測区間
tw_train_pred <- # あてはめ値と予測区間を付加
  bind_cols(tw_train,
            predict(tw_lm, interval = "prediction"))
tw_test_pred <- # 新規データへのあてはめ値と予測区間を付加
  bind_cols(tw_test, 
            predict(tw_lm, newdata = tw_test, interval = "prediction"))

#' 8月のデータで推定したモデルで8月をあてはめた信頼区間
tw_train_conf |>
  ggplot(aes(x = day, y = temp)) +
  geom_point(colour = "red", shape = 16) +
  geom_point(aes(y = fit), colour = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "royalblue") +
  ylim(c(20,34)) + # 以降4つのグラフの値域を揃える
  labs(x = "August", y = "Temperature", title = "Confidence Interval")

#' 8月のデータで推定したモデルで8月をあてはめた予測区間
tw_train_pred |>
  ggplot(aes(x = day, y = temp)) +
  geom_point(colour = "red", shape = 16) +
  geom_point(aes(y = fit), colour = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "steelblue") +
  ylim(c(20,34)) + 
  labs(x = "August", y = "Temperature", title = "Prediction Interval")

#' 8月のモデルで9月をあてはめた信頼区間
tw_test_conf |>
  ggplot(aes(x = day, y = temp)) +
  geom_point(colour = "red", shape = 16) +
  geom_point(aes(y = fit), colour = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "royalblue") +
  ylim(c(20,34)) + 
  labs(x = "September", y = "Temperature", title = "Confidence Interval")

#' 8月のモデルで9月をあてはめた予測区間
tw_test_pred |>
  ggplot(aes(x = day, y = temp)) +
  geom_point(colour = "red", shape = 16) +
  geom_point(aes(y = fit), colour = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), colour = "steelblue") +
  ylim(c(20,34)) + 
  labs(x = "September", y = "Temperature", title = "Prediction Interval")

#' 人工データによる検討例
#'   (以下はあくまで例なので自由に数値実験を設計して下さい)

#' 試行の設定
#' モデル: y = -1 + 2 * x1
#' 人工データの生成
set.seed(1515) # 乱数のシード値(適宜変更せよ)
n <- 50 # データ数の設定
x_obs <- tibble(x0 = 1,
                x1 = runif(n), # 説明変数1
                x2 = runif(n)) # 説明変数2
beta <- c(-1, 2, 0) # (切片, x1の係数, x2の係数) 実質x2は使われていない
sigma <-  1/2 # 誤差の標準偏差
epsilon <- rnorm(nrow(x_obs), sd = sigma) # 誤差項
toy_data <- x_obs |> # 目的変数の観測値を追加
  mutate(y = as.vector(as.matrix(x_obs) %*% beta) + epsilon)

#' モデルの推定と評価
toy_lm1 <- lm(y ~ x1, data = toy_data) # x1のみの正しいモデル
summary(toy_lm1)
toy_lm2 <- lm(y ~ x1 + x2, data = toy_data) # x1とx2の冗長なモデル
summary(toy_lm2)
toy_lm3 <- lm(y ~ x2, data = toy_data) # x2のみの誤ったモデル
summary(toy_lm3)

#' 新規データに対する予測
x_new <- tibble(x0 = 1,
                x1 = runif(n),
                x2 = runif(n,-10,10)) 
#' 新規データに対する目的変数の真値 (誤差なし)
y_tilde <- as.vector(as.matrix(x_new) %*% beta)
#' 各モデルでの予測
y_hat1 <- predict(toy_lm1, newdata = x_new) # x_lm1による予測値
y_hat2 <- predict(toy_lm2, newdata = x_new) # x_lm2による予測値
y_hat3 <- predict(toy_lm3, newdata = x_new) # x_lm3による予測値

#' 散布図による可視化
tibble(obs = y_tilde,
       model1 = y_hat1, model2 = y_hat2, model3 = y_hat3) |>
  pivot_longer(!obs) |>
  ggplot(aes(x = value, y = obs)) +
  geom_point(aes(colour = name)) +
  geom_abline(slope = 1, intercept = 0, colour = "gray") +
  labs(x = "fitted value", y = "observed value") +
  theme(legend.title = element_blank())

#' 相関係数による数値的な評価 (R-squared と等価)
cor(y_tilde, y_hat1)^2 
cor(y_tilde, y_hat2)^2
cor(y_tilde, y_hat3)^2

#' ---------------------------------------------------------------------------

#' factor属性の与え方
X <- c("A", "S", "A", "B", "D")
Y <- c(85, 100, 80, 70, 30)
toy_data1 <- tibble(X, Y)
toy_data2 <- toy_data1 |> # 因子化
  mutate(X2 = factor(X))  # 関数as_factor()を用いてもよい
str(toy_data2) # 作成したデータフレームの素性を見る
toy_data3 <- toy_data2 |> # 順序付き(levels)の因子化
  mutate(X3 = factor(X, levels=c("S","A","B","C","D")))
str(toy_data3) # toy_data2とはfactorの順序が異なる
toy_data4 <- toy_data2 |>
  mutate(Y2 = factor(Y > 60)) # 条件による因子化
str(toy_data4) # 条件の真偽で2値に類別される

#' ---------------------------------------------------------------------------
#' @practice 交互作用と非線形を含むモデルとカテゴリカル変数の扱い

#' 9月から11月のデータによる分析 (交互作用と非線形性)

#' データの整形
tw_subset <- tw_data |> filter(month %in% 9:11)

#' 日射量，気圧，湿度の線形回帰モデル
summary(lm(temp ~ solar + press + humid, data = tw_subset))
#' 湿度の対数を考えた線形回帰モデル
summary(lm(temp ~ solar + press + log(humid), data = tw_subset))
#' 最初のモデルにそれぞれの交互作用を加えたモデル (書き方はいろいろある)
summary(lm(temp ~ (solar + press + humid)^2, data = tw_subset))
#' 更に3つの変数の積を加えたモデル
summary(lm(temp ~ solar * press * humid, data = tw_subset))

#' 用いた変数の散布図
tw_subset |>
  select(temp, solar, press, humid) |>
  GGally::ggpairs(columnLabels = c("気温","日射量","気圧","湿度"))

#' 最後のモデルの視覚的な評価 (診断プロット)
autoplot(lm(temp ~ solar * press * humid, data = tw_subset))

#' 雨と気温の関係の分析 (カテゴリカル変数)
#' 雨の有無および月(整数値)をダミー化(因子化)する
tw_data_fact <- tw_data |>
  mutate(rain = factor(rain > 0),
         month = factor(month)) 
summary(lm(temp ~ rain, data = tw_data_fact))
#' 通年では雨と気温の関係は積極的に支持されない

summary(lm(temp ~ rain + month, data = tw_data_fact))
#' 月毎の気温の差を考慮した回帰式が推定される
#' 月毎に比較した結果，雨の日の方が気温が低いことが支持される

#' ---------------------------------------------------------------------------

#' モデルの探索
adv_data <- read_csv('https://www.statlearning.com/s/Advertising.csv')
summary(lm(sales ~ radio, data = adv_data))
summary(lm(sales ~ TV + radio, data = adv_data))
summary(lm(sales ~ TV + radio + newspaper, data = adv_data))
summary(init <- lm(sales ~ TV * radio * newspaper, data = adv_data))
opt <- step(init) # step関数による探索 (最大のモデルから削減増加を行う)
summary(opt) # 探索された(準)最適なモデルの確認
