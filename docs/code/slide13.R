### 第13講 サンプルコード
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)

toy_acf <- arima.sim(model = list(ar = c(0.8, -0.64),
                                  ma = c(-0.5)),
                     n = 200) |>
  as_tsibble() |> ACF(value) 
toy_acf |> autoplot()

#' ---------------------------------------------------------------------------
#' @practice 自己相関

#' 前回作成した自作の関数 my_arma() を利用しても良いが
#' 今回は関数 arima.sim() を用いた方法を紹介する
Tmax <- 500 # 時系列の長さ t=1,..,Tmax
K <- 4 # 表示する時系列の数 (4つを並べて比較する)
ts_ar <- ts(replicate(K, arima.sim(list(ar = c(0.67, 0.26)),
                                   n = Tmax,
                                   innov = rnorm(Tmax))))
ts_ma <- ts(replicate(K, arima.sim(list(ma = c(0.44, -0.28)),
                                   n = Tmax,
                                   innov = rnorm(Tmax))))
ts_arma <- ts(replicate(K, arima.sim(list(ar = c(0.8, -0.64),
                                          ma = c(-0.5)),
                                     n = Tmax,
                                     innov = rnorm(Tmax))))

#' AR(2)モデルの自己相関
ts_ar |> as_tsibble() |> ACF(value) |> autoplot()
ts_ar |> as_tsibble() |> ACF(value) |>
    autoplot() + # 格子状に並べるには facet を指定する
    facet_wrap(key ~ .) + # 適宜調整してくれる
    labs(title = "AR(2)")

#' MA(2)モデルの自己相関
ts_ma |> as_tsibble() |> ACF(value) |>
    autoplot() + 
    facet_wrap(key ~ .) +
    labs(title = "MA(2)")

#' ARMA(2,1)モデルの自己相関
ts_arma |> as_tsibble() |> ACF(value) |>
    autoplot() + 
    facet_wrap(key ~ .) +
    labs(title = "ARMA(2,1)")

#' ---------------------------------------------------------------------------

#' @excercise ARモデルの推定

toy_ar <- arima.sim(model = list(ar = c(0.7,-0.6, 0.5)),
                    n = 1000) |> as_tsibble()
toy_ar |> model(AR(value)) # モデルを自動選択する場合
toy_ar |> model(AR(value ~ order(3))) # モデルの次数を指定する場合
toy_ar |> model(AR(value ~ 0 + order(3))) # 平均項を含めない場合

#' @excercise ARIMAモデルの推定

toy_arima <- arima.sim(model = list(order = c(2,1,2),
                                 ar = c(0.8,-0.5),
                                 ma = c(-0.2,0.2)),
                    n = 1000) |> as_tsibble()
toy_arima |> model(ARIMA(value)) # 自動選択
toy_arima |> model(ARIMA(value ~ 0 + pdq(2,1,2))) # 次数を指定

#' @excercise モデルの評価

toy_fit <- toy_arima |> model(ARIMA(value))
toy_fit |> accuracy()
toy_fit |> glance()
toy_fit |> gg_tsresiduals()

#' ---------------------------------------------------------------------------
#' @practice AR/ARMAモデルの推定

#' AR(2)過程の生成
toy_ar <- arima.sim(model = list(ar = c(0.67, 0.26)),
                       n = 5000) |> # 時系列の長さも自由に変更せよ
    as_tsibble()
#' 関数 fabletools::model() + fable::AR() による推定
toy_ar_fit <- # 自動的に推定されたモデルを保存
    toy_ar |> model(AR(value)) 
toy_ar_fit |> report()   # 推定されたモデル
toy_ar_fit |> tidy()     # 推定された係数とその評価
toy_ar_fit |> accuracy() # 推定されたモデルのあてはまりの評価
toy_ar_fit |> glance()   # 推定されたモデルの情報量規準など
#' ある程度長い系列であれば良い推定が得られる
#' n = 1000 などとして試してみよ

#' ARMA(2,1)過程の生成
toy_arma <- arima.sim(model = list(ar = c(0.67, 0.26), ma = c(-0.5)),
                      n = 2000) |> 
    as_tsibble()
#' 関数 fabletools::model() + fable::ARIMA() による推定
toy_arma_fit <- # 自動的に推定されたモデルを保存
    toy_arma |> model(
                    model0 = ARIMA(value ~ pdq(2,0,1)), # 正しいモデル
                    model1 = ARIMA(value ~ pdq(3,0,1)), # 間違ったモデル
                    model2 = ARIMA(value ~ pdq(2,0,2)), # 間違ったモデル
                    auto0  = ARIMA(value ~ pdq(d = 0)), # ARMAで自動推定(階差を取らない)
                    auto1  = ARIMA(value), # ARIMA全体で自動推定(モデルとしてはかなり冗長)
                    )
toy_arma_fit # 推定されたモデルの表示
toy_arma_fit |> glance() # 推定されたモデルの評価
toy_arma_fit |> glance() |> arrange(AIC) # AIC順に並べる
#' 対数尤度(log_lik)は大きい方が観測データへのあてはまりは良い
#' AICは小さい方が良い予測が良いことが期待される
#' 自動推定では必ずしも正しいモデルが推定される訳ではないことに注意
#' 特に短い時系列では推定が難しい場合が多い
#' 生成する系列の長さを変えて実験してみよう

#' 残差の評価
toy_arma_fit |> select(model0) |> # 正しいモデル
    gg_tsresiduals()
toy_arma_fit |> select(auto0) |>  # 自動推定されたARMAモデル
    gg_tsresiduals()
#' 残差は無相関になっていることが確認できる

#' ---------------------------------------------------------------------------

tsibble(date = as_date("2024-01-01") + 0:9,
        value = rnorm(10))
tibble(year = 2001:2020,
       value = rnorm(20)) |>
  as_tsibble(index = year) # yearを時間情報に指定
AirPassengers |> as_tsibble() # 時系列オブジェクトの変換

AirPassengers |>
  as_tsibble() |>
  filter_index("1955-10" ~ "1956-03")

tw_data <- read_csv("data/tokyo_weather.csv")

#' ---------------------------------------------------------------------------
#' @practice 東京の気温データの時系列モデル

#' データの整理
tw_data <- read_csv("data/tokyo_weather.csv")
tw_tsbl <- tw_data |>
    mutate(date = as.Date(paste(year, month, day, sep = "-"))) |>
    select(date, temp) |>
    as_tsibble(index = date) # date を時系列の index に指定

#' データの視覚化
tw_tsbl |>
    autoplot(temp, colour = "red") +
    labs(title = "Temperature in Tokyo",
         x = "date", y = "temperature")
tw_tsbl |> # 一部を切り出して視覚化する
    filter_index("2022-06-01" ~ "2022-07-31") |>
    autoplot(temp, colour = "red") +
    labs(title = "Temperature in Tokyo",
         x = "date", y = "temperature")

#' データの性質を確認
tw_tsbl |> ACF(temp) |> # 自己相関
    autoplot() # 減衰が遅いので差分をとった方が良さそう
tw_tsbl |> 
    autoplot(difference(temp), colour = "orange")
tw_tsbl |> ACF(difference(temp)) |> # 階差系列の自己相関
    autoplot() 

#' 階差系列にARMAモデルをあてはめる (d=1)
tw_fit <- tw_tsbl |>
  model(ARIMA(temp ~ pdq(d = 1) + PDQ(D = 0)))
report(tw_fit) # 推定されたモデルの仕様を表示
tw_fit |> # 残差の評価
  gg_tsresiduals() # そこそこあてはまりは良さそう
tw_fit |> # データとあてはめ値の比較
    augment() |>
    autoplot(temp) +
    geom_line(aes(y = .fitted), # y軸にあてはめ値を指定
              colour = "blue", alpha = 0.3) +
    labs(title = "Fitted by ARIMA model",
         x = "date", y = "temperature")

#' ---------------------------------------------------------------------------

#' 後半を予測してみる
cp_3rd_arima |> 
  forecast(h = nrow(cp_3rd_test)) |>
  autoplot(cp_3rd_train, level = 80) +
  autolayer(cp_3rd_test, .vars = patients, colour = "red") +
  labs(title = "Prediction by ARIMA model")

#' @excercise 時系列の予測

as_tsibble(AirPassengers) |>
  model(ARIMA(log(value))) |>
  forecast(h = 36) |> autoplot(AirPassengers)

#' @excercise ETSモデルの推定

as_tsibble(AirPassengers) |>
  model(ETS(value ~ season("M"))) |>
  components() |> autoplot()

#' ---------------------------------------------------------------------------
#' @practice 時系列の予測

#' 厚生労働省のCOVID-19の感染者数データ
#' 以下は解析事例で紹介した内容
#' データの取得と整理 
cp_tbl <-
  read_csv("https://covid19.mhlw.go.jp/public/opendata/newly_confirmed_cases_daily.csv") |>
  select(1:2) |>
  set_names(c("date","patients")) |>
  mutate(date = as_date(date))
#' 時系列データ(tsibbleクラス)への変更
cp_tsbl <-
  cp_tbl |>
  as_tsibble(index = date)
#' データ(全国・全期間)の表示
cp_tsbl |>
  autoplot(patients, colour = "skyblue") +
  geom_col(fill = "skyblue") + # 塗り潰しを行う
  labs(title = "COVID-19 patients in Japan",
       x = "date",
       y = "number of patients")
#'
#' 第3波 (2020/9/15-2021/1/31) に着目する
#' 
cp_3rd_train <- # 訓練データ
  cp_tsbl |>
  filter_index("2020-09-15" ~ "2020-11-30")
cp_3rd_test <-  # 試験データ
  cp_tsbl |>
  filter_index("2020-12-01" ~ "2021-01-31")
#' 第3波の表示
bind_rows(cp_3rd_train,cp_3rd_test) |>
  autoplot(patients, colour = "skyblue") +
  geom_col(fill = "skyblue") + # 塗り潰しを行う
  labs(title = "COVID-19 patients in Japan",
       x = "date",
       y = "number of patients")
#' 階差系列の描画
cp_3rd_train |>
  gg_tsdisplay(difference(patients),
               plot_type = "partial")
#' 7日周期の影響があることがわかる
#' 対数変換+階差系列の描画
cp_3rd_train |>
  gg_tsdisplay(difference(log(patients)),
               plot_type = "partial")
#' やはり7日周期の影響があることがわかる
#' 7日の周期(季節成分)での階差系列の描画
cp_3rd_train |>
  gg_tsdisplay(difference(difference(log(patients), lag = 7)),
               lag_max = 21,
               plot_type = "partial")
#' 7日の相関は負になっているので，季節階差は取らなくても良さそう
#' 以下では対数変換した系列に対してARIMAモデルをあてはめる
cp_3rd_arima <-
  cp_3rd_train |>
  model(ARIMA(log(patients) ~ PDQ(D = 0)))
#' 推定結果の検討
cp_3rd_arima |> report()
cp_3rd_arima |> accuracy()
cp_3rd_arima |> glance()
#' データとあてはめ値を表示
cp_3rd_arima |>
  augment() |>
  autoplot(patients, colour = "skyblue") +
  geom_line(aes(y = .fitted), colour = "orange") +
  labs(title = "Fitted by ARIMA model",
       x = "date", y = "log(patients)")
#' 残差の診断
cp_3rd_arima |> gg_tsresiduals()
#' 残差の相関はだいぶ消えているので，そこそこ良いモデルといえそう
#' 後半を予測してみる
cp_3rd_arima |> 
  forecast(h = nrow(cp_3rd_test)) |>
  autoplot(cp_3rd_train, level = 80) +
  autolayer(cp_3rd_test, .vars = patients, colour = "red") +
  labs(title = "Prediction by ARIMA model")
#' 感染が収まるまでの増加傾向を上手く推定できている

#' 
#' 第8波 (2022/11/01-2023/1/31) に着目する
#' 
cp_8th_train <- # 訓練データ
  cp_tsbl |>
  filter_index("2022-11-01" ~ "2022-11-30")
cp_8th_test <-  # 試験データ
  cp_tsbl |>
  filter_index("2022-12-01" ~ "2023-01-31")
#' 第8波の表示
bind_rows(cp_8th_train,cp_8th_test) |>
  autoplot(patients, colour = "skyblue") +
  geom_col(fill = "skyblue") + # 塗り潰しを行う
  labs(title = "COVID-19 patients in Japan",
       x = "date",
       y = "number of patients")
#' 階差系列の描画
cp_8th_train |>
  gg_tsdisplay(difference(patients),
               plot_type = "partial")
#' 対数変換+階差系列の描画
cp_8th_train |>
  gg_tsdisplay(difference(log(patients)),
               plot_type = "partial")
#' 7日の周期(季節成分)での階差系列の描画
cp_8th_train |>
  gg_tsdisplay(difference(difference(log(patients), lag = 7)),
               lag_max = 21,
               plot_type = "partial")
#' 対数変換した系列に対してARIMAモデルをあてはめる
cp_8th_arima <-
  cp_8th_train |>
  model(ARIMA(log(patients) ~ PDQ(D = 0)))
#' 推定結果の検討
cp_8th_arima |> report()
cp_8th_arima |> accuracy()
cp_8th_arima |> glance()
#' データとあてはめ値を表示
cp_8th_arima |>
  augment() |>
  autoplot(patients, colour = "skyblue") +
  geom_line(aes(y = .fitted), colour = "orange") +
  labs(title = "Fitted by ARIMA model",
       x = "date", y = "log(patients)")
#' 残差の診断
cp_8th_arima |> gg_tsresiduals()
#' 後半の予測
cp_8th_arima |> 
  forecast(h = nrow(cp_8th_test)) |>
  autoplot(cp_8th_train, level = 80) +
  autolayer(cp_8th_test, .vars = patients, colour = "red") +
  labs(title = "Prediction by ARIMA model")
#'
#' その他の方法
#' fableに実装されているいくつかの方法で予測してみる
cp_8th_models <-
  cp_8th_train |>
  model( # 自動推定
    naive  = NAIVE(log(patients)),  # random walk model
    arima  = ARIMA(log(patients) ~ pdq() + PDQ(0,0,0)),
    sarima = ARIMA(log(patients) ~ pdq() + PDQ(D = 0)),
    ets    = ETS(log(patients)),    # exponential smoothing
    )
cp_8th_models |>
  forecast(h = nrow(cp_8th_test)) |>
  autoplot(cp_8th_train, level = NULL) +
  autolayer(cp_8th_test, .vars = patients, colour = "gray") +
  labs(title = "Prediction by various models")

#' AirPassengersデータの分析
#' データの時間に関する情報を表示 (月ごとのデータ)
AirPassengers |> tsp()
#' データの表示
AirPassengers |> as_tsibble() |>
    autoplot(value) +
    labs(title = "AirPassengers",
         x = "Year", y = "Passengers/1000")
#' ほぼ線形のトレンドと増大する分散変動があることがわかる
#' 対数変換データの表示
AirPassengers |> log() |> as_tsibble() |>
    autoplot(value) +
    labs(title = "AirPassengers",
         x = "Year", y = "log(Passengers/1000)")
#' 対数変換により分散変動が安定化していることがわかる
#' 以下では対数変換したデータを扱う
ap_tsbl <- AirPassengers |> as_tsibble()
ap_train <- ap_tsbl |> filter_index(~ "1958-12") # 訓練データ
ap_test  <- ap_tsbl |> filter_index("1959-01" ~ .) # 試験データ(2年分)

#' まずトレンド(明らかな上昇傾向)について考察
#' 階差を取ることにより定常化できるか検討
ap_train |> 
    gg_tsdisplay(difference(log(value)),
                 plot_type = "partial") 
#' lag=12(1年),24(2年)に強い自己相関(季節成分)がある
#' lag=12(1年)に偏自己相関は残っている
#' 季節成分について考察
#' 12ヶ月で階差を取って同様に検討
ap_train |> 
    gg_tsdisplay(difference(difference(log(value), lag = 12)),
                 lag_max = 36,
                 plot_type = "partial")
#' lag=1,3,12(1年) に若干偏自己相関が残っている
#' lag=36(3年) まで見ると上記以外の偏自己相関は誤差内
#' SARIMAモデルの作成
#' 階差および季節成分の自己相関・偏自己相関から
#'  階差系列については ARMA(pまたはq=1-3)，
#'  季節成分 については ARMA(pまたはq=1-2)
#' あたりを考える必要がありそう
#' いくつかのARIMAモデルの推定
ap_fit <-
    ap_train |>
    model( # 名前に特殊な文字を含む場合は``で括る
        `arima(0,1,2)(0,1,1)` = ARIMA(log(value) ~ pdq(0,1,2) + PDQ(0,1,1)),
        `arima d=1 D=1` = ARIMA(log(value) ~ pdq(d = 1) + PDQ(D = 1)),
        `arima auto` = ARIMA(log(value)),
        `ets` = ETS(log(value)),
        )
#' 次数を指定したモデル order=pdq(0,1,2), seasonal=PDQ(0,1,1)
ap_fit |> select(`arima(0,1,2)(0,1,1)`) |> report() # 推定されたモデルの概要
ap_fit |> select(`arima(0,1,2)(0,1,1)`) |>
    gg_tsresiduals() # 残差の診断(モデルの診断)
ap_fit |> select(`arima(0,1,2)(0,1,1)`) |>
    augment() |> features(.innov, ljung_box, lag = 12) # Ljung-Box検定
#' 残差の自己相関は小さいが残差の正規性は低い
#' 残差に関する Ljung-Box 検定は
#' - 帰無仮説 : 残差は無作為
#' - 対立仮説 : 残差は無作為でない
#' であるので，p値が高い方が良い
#' 検討結果を踏まえ階差・季節成分の階差を指定した推定
ap_fit |> select(`arima d=1 D=1`) |> report() 
ap_fit |> select(`arima d=1 D=1`) |>
    gg_tsresiduals() 
ap_fit |> select(`arima d=1 D=1`) |>
    augment() |> features(.innov, ljung_box, lag = 12)
#' 自動的にモデル選択を行う
ap_fit |> select(`arima auto`) |> report() 
#' いずれにせよAIC最小のモデルは以下となる
#' order = pdq(0,1,1), seasonal = PDQ(0,1,1)
#' 以降，このモデルを利用する
#' 予測値と信頼区間の描画
ap_fit |> select(`arima auto`) |>
    forecast(h = nrow(ap_test)) |>
    autoplot(ap_train) +
    autolayer(ap_test, value, colour = "purple") +
    labs(title = "Forecast from SARIMA model",
         x = "Year", y = "Passengers/1000")
#' 時系列の分解
#' ETS (exponential smoothing) モデル
#' トレンド(level+slope) + 季節(12ヶ月周期) + ランダム
ap_fit |> select(`ets`) |> report() 
ap_fit |> select(`ets`) |> gg_tsresiduals() 
ap_fit |> select(`ets`) |> augment() |> features(.innov, ljung_box, lag = 12)
#' 残差に周期構造が残っているので帰無仮説が棄却される
#' 予測値と信頼区間の描画
ap_fit |> select(`ets`) |>
    forecast(h = nrow(ap_test)) |>
    autoplot(ap_train) +
    autolayer(ap_test, value, colour = "purple") +
    labs(title = "Forecast from ETS model",
         x = "Year", y = "Passengers/1000")
#' ARIMAとETSの比較
ap_train |>
    autoplot(value) +
    geom_line(aes(y = .fitted, colour = .model),
              data = ap_fit |> select(`arima auto`,`ets`) |> augment()) +
    labs(title = "ARIMA vs ETS (fitted)",
         x = "Year", y = "Passengers/1000")
ap_fit |> select(`arima auto`,`ets`) |>
    forecast(h = nrow(ap_test)) |>
    autoplot(ap_train, level = NULL) +
    autolayer(ap_test, value, colour = "violet") +
    labs(title = "ARIMA vs ETS (forecast)",
         x = "Year", y = "Passengers/1000")

#' ---------------------------------------------------------------------------
