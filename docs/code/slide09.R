### 第9講 サンプルコード
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
  yardstick::spec(),
  yardstick::precision(),
  yardstick::recall(),
)
library(tidyverse)
library(ggfortify)
library(MASS)
library(tidymodels)

#' ---------------------------------------------------------------------------
#' @practice 判別結果の評価

#' 東京の気象データによる線形判別分析
#' 9月と10月の判別の例
tw_data <- read_csv("data/tokyo_weather.csv")
tw_subset  <- tw_data |> 
  filter(month %in% c(9,10)) |> # 9,10月のデータ
  select(temp, humid, month) |> # 気温・湿度・月を選択
  mutate(month = as_factor(month)) # 月を因子化
#' 判別関数を作成
tw_lda <- lda(formula = month ~ temp + humid,
              data = tw_subset)
#' 判別結果の評価
tw_lda_fitted <- predict(tw_lda)
tw_lda_cm <- tw_subset |>
  bind_cols(fitted = tw_lda_fitted[["class"]]) |>
  conf_mat(truth = month, estimate = fitted)
tw_lda_cm # 表示
summary(tw_lda_cm) # 詳細表示
autoplot(tw_lda_cm, type = "mosaic") # モザイクプロット
autoplot(tw_lda_cm, type = "heatmap") # 行列表示
#' ROC曲線とAUCの計算
tw_subset |>
  bind_cols(tw_lda_fitted[["posterior"]]) |>
  roc_curve(truth = month, `9`) |> # 9月への判別を陽性とする
  autoplot()
tw_subset |>
  bind_cols(tw_lda_fitted[["posterior"]]) |>
  roc_auc(truth = month, `9`)

#' 12ヶ月分のデータを用いた例 (説明変数は適宜選択せよ)
tw_subset12  <- tw_data |>
  select(temp, solar, wind, humid, month) |>
  mutate(month = as_factor(month))
#' 判別関数を作成
tw_lda12 <- lda(month ~ ., # 右辺の . は month 以外の全てを説明変数として指定
                data = tw_subset12)
#' 判別結果の評価
tw_lda12_fitted <- predict(tw_lda12)
tw_lda12_cm <- tw_subset12 |>
  bind_cols(fitted = tw_lda12_fitted[["class"]]) |>
  conf_mat(truth = month, estimate = fitted)
tw_lda12_cm # 表示
summary(tw_lda12_cm) # 詳細表示
autoplot(tw_lda12_cm, type = "mosaic") # モザイクプロット
autoplot(tw_lda12_cm, type = "heatmap") # 行列表示
tw_subset12 |>
  bind_cols(tw_lda12_fitted[["posterior"]]) |>
  roc_curve(truth = month, `1`:`12`) |> 
  autoplot()
tw_subset12 |> 
  bind_cols(tw_lda12_fitted[["posterior"]]) |>
  roc_auc(truth = month, `1`:`12`)

#' Wine Quality Data Set を用いた判別分析
wq_data <-
  read_delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
             delim = ";") |>
  mutate(grade = factor(case_when( # quality を A,B,C,D に割り当てる
           quality >= 7 ~ "A",
           quality >= 6 ~ "B",
           quality >= 5 ~ "C",
           .default = "D"
         )))
#' データを分割する
set.seed(987987) # 適宜シード値は設定する
wq_split <- initial_split(wq_data, prop = 0.8,
                          strata = grade)
#' gradeで層別の指定すると分割したデータのgradeの比率が保たれる

#' 判別関数を作成 (gradeのもとになっているqualityは除く)
wq_formula <- grade ~ . - quality
wq_lda <- lda(formula = wq_formula, data = training(wq_split))
wq_qda <- qda(formula = wq_formula, data = training(wq_split))

#' 訓練データによる判別結果の評価
wq_lda_train_cm <- training(wq_split) |>
  bind_cols(fitted = predict(wq_lda)[["class"]]) |>
  conf_mat(truth = grade, estimate = fitted)
summary(wq_lda_train_cm) # 線形判別の評価
autoplot(wq_lda_train_cm, type = "heatmap") +
  labs(title = "Linear Discriminant (training data)")
wq_qda_train_cm <- training(wq_split) |>
  bind_cols(fitted = predict(wq_qda)[["class"]]) |>
  conf_mat(truth = grade, estimate = fitted)
summary(wq_qda_train_cm) # 2次判別の評価
autoplot(wq_qda_train_cm, type = "heatmap") +
  labs(title = "Quadratic Discriminant (training data)")

#' 試験データによる判別結果の評価
wq_lda_test_cm <- testing(wq_split) |>
  bind_cols(fitted = predict(wq_lda,
                             newdata = testing(wq_split))[["class"]]) |>
  conf_mat(truth = grade, estimate = fitted)
summary(wq_lda_test_cm) # 線形判別の評価
autoplot(wq_lda_test_cm, type = "heatmap") +
  labs(title = "Linear Discriminant (test data)")
wq_qda_test_cm <- testing(wq_split) |>
  bind_cols(fitted = predict(wq_qda,
                             newdata = testing(wq_split))[["class"]]) |>
  conf_mat(truth = grade, estimate = fitted)
summary(wq_qda_test_cm) # 2次判別の評価
autoplot(wq_qda_test_cm, type = "heatmap") +
  labs(title = "Quadratic Discriminant (test data)")

#' ---------------------------------------------------------------------------
#' @practice 予測誤差の評価

#' MASS::biopsy による誤差の評価
#' データの整理
bio_data <- as_tibble(biopsy) |>
  na.omit() |> # NA を除く
  select(-ID)  # IDを除く

bio_data |>
  GGally::ggpairs(aes(colour = class))

#' 2次判別の LOO CV による評価の例 (線形判別も同様)
bio_formula <- class ~ . 
bio_qda <- qda(formula = bio_formula, data = bio_data) 
bio_qda_loo <- qda(formula = bio_formula, data = bio_data, CV = TRUE)
#' 訓練誤差の評価
bio_qda_cm <- bio_data |>
  bind_cols(fitted = predict(bio_qda)[["class"]]) |>
  conf_mat(truth = class, estimate = fitted)
summary(bio_qda_cm) # 2次判別によるあてはめ値の評価(訓練誤差)
autoplot(bio_qda_cm, type = "heatmap") +
  labs(title = "Training Error")
bio_qda_loo_cm <- bio_data |>
  bind_cols(fitted = bio_qda_loo[["class"]]) |>
  conf_mat(truth = class, estimate = fitted)
summary(bio_qda_loo_cm) # LOO CVによる予測の評価(予測誤差)
autoplot(bio_qda_loo_cm, type = "heatmap") +
  labs(title = "Test Error (LOO CV)")
#' あてはめ値による評価は LOO CV より若干良くなっており
#' あてはめ値では精度を過剰に評価する可能性があることが示唆される

#' Wine Quality Data Set による誤差の評価
#' 既に整理してある 'wq_data/wq_split' を用いる

#' 線形判別の LOO CV
#' 'wq_lda' と比較する
wq_lda_loo <- lda(formula = wq_formula, data = training(wq_split),
                  CV = TRUE)
training(wq_split) |> # 訓練誤差による評価
  bind_cols(fitted = predict(wq_lda)[["class"]]) |>
  conf_mat(truth = grade, estimate = fitted) |>
  autoplot(type = "heatmap") +
  labs(title = "Training Error (LDA)")
wq_lda_loo_cm <- training(wq_split) |> # LOO CV 予測誤差による評価
  bind_cols(fitted = wq_lda_loo[["class"]]) |>
  conf_mat(truth = grade, estimate = fitted)
wq_lda_loo_cm |> autoplot(type = "heatmap") +
  labs(title = "LOO CV Error (LDA)")
#' 線形判別の過学習は微小

#' 2次判別の LOO CV 
#' 'wq_qda' と比較する
wq_qda_loo <- qda(formula = wq_formula, data = training(wq_split),
                  CV = TRUE)
training(wq_split) |> # 訓練誤差による評価
  bind_cols(fitted = predict(wq_qda)[["class"]]) |>
  conf_mat(truth = grade, estimate = fitted) |>
  autoplot(type = "heatmap") +
  labs(title = "Training Error (QDA)")
wq_qda_loo_cm <- training(wq_split) |> # LOO CV 予測誤差による評価
  bind_cols(fitted = wq_qda_loo[["class"]]) |>
  conf_mat(truth = grade, estimate = fitted)
wq_qda_loo_cm |> autoplot(type = "heatmap") +
  labs(title = "LOO CV Error (QDA)")
#' 2次判別は若干過学習している

#' LOO CV による線形・2次判別の予測誤差の比較
summary(wq_lda_loo_cm) # 線形
summary(wq_qda_loo_cm) # 2次
#' 予測誤差の観点からは線形判別の方が良さそう

#' tidymodels による k-重交叉検証
#' 'lda/qda' を tidymodels 用に宣言
library(discrim) # 以下の判別モデルを設定するために必要
tidy_qda <- discrim_quad() |> set_engine("MASS") |> set_mode("classification")
tidy_lda <- discrim_linear() |> set_engine("MASS") |> set_mode("classification")
#' 交叉検証用にデータ分割を行う
wq_folds <- vfold_cv(training(wq_split),
                     v = 10) # 10-fold を指定 (既定値)
#' 評価指標を設定
wq_metrics <- metric_set(accuracy, # 精度
                         sens, # 感度 (真陽性率)
                         spec, # 特異度 (真陰性率)
                         precision, # 適合率
                         recall, # 再現率(sensと同じ)
                         roc_auc, # AUC
                         kap, # kappa
                         f_meas) # f値
wq_workflow <- workflow() |> # 共通の処理を定義
  add_formula(wq_formula) 
#' 線形判別
wq_lda_cv <- wq_workflow |>
  add_model(tidy_lda) |> 
  fit_resamples(resamples = wq_folds,
                metrics = wq_metrics)
wq_lda_cv |> collect_metrics()
#' 2次判別
wq_qda_cv <- wq_workflow |>
  add_model(tidy_qda) |>
  fit_resamples(resamples = wq_folds,
                metrics = wq_metrics)
wq_qda_cv |> collect_metrics()
