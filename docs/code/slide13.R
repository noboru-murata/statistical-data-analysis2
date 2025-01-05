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

#' @excercise 時系列の予測

as_tsibble(AirPassengers) |>
  model(ARIMA(log(value))) |>
  forecast(h = 36) |> autoplot(AirPassengers)

#' @excercise ETSモデルの推定

as_tsibble(AirPassengers) |>
  model(ETS(value ~ season("M"))) |>
  components() |> autoplot()
