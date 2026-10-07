### 第3講 資料

#' 東京の気候データを用いた例
#' データの読み込み
tw_data <-
  read_csv("data/tokyo_weather.csv") |>
  mutate(date = date(paste(year,month,day,sep="-")), .before = 1) |>
  select(-c(year,month,day,day_of_week)) |>
  set_names(c("日付","気温","降雨","日射","降雪","風向","風速","気圧","湿度","雲量"))
tw_subset <- read_csv("data/tokyo_weather.csv") |>
  filter(month == 8) |> # 9月のデータの抽出
  mutate(date = date(paste(year,month,day,sep="-")), .before = 1) |>
  select(-c(year,month,day,day_of_week)) |>
  set_names(c("日付","気温","降雨","日射","降雪","風向","風速","気圧","湿度","雲量"))
#' モデル式
tw_formula1 <- 気温 ~ 気圧
tw_formula2 <- 気温 ~ 日射
tw_formula3 <- 気温 ~ 気圧 + 日射
tw_formula4 <- 気温 ~ 気圧 + 日射 + 湿度
tw_formula5 <- 気温 ~ 気圧 + 日射 + 雲量
#' 推定
tw_lm1 <- lm(tw_formula1, data = tw_subset, y = TRUE)
tw_lm2 <- lm(tw_formula2, data = tw_subset, y = TRUE)
tw_lm3 <- lm(tw_formula3, data = tw_subset, y = TRUE)
tw_lm4 <- lm(tw_formula4, data = tw_subset, y = TRUE)
tw_lm5 <- lm(tw_formula5, data = tw_subset, y = TRUE)

#' データの整理
tw_tbl <-
  tw_subset |>
  mutate(日付 = as.character(日付)) |> # pdfの表示
  gt()

#| tbl-cap: "東京の8月の気候データ"
tw_tbl

#| tbl-cap: "東京の8月の気候データ"
tw_tbl |>
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              table.width = pct(80),
              latex.tbl.pos = "h") |>
  as_latex()

#| fig-width: 5.5
#' 関連するデータの散布図
tw_subset |>
  select(気温,気圧,日射,湿度,雲量) |>
  ggpairs()

#| fig-width: 6
#' 観測値とあてはめ値の比較
tw_subset |>
  mutate(モデル1 = fitted(tw_lm1),    # モデルごとに予測値をデータフレームに追加
         モデル2 = fitted(tw_lm2),
         モデル3 = fitted(tw_lm3),
         モデル4 = fitted(tw_lm4),
         モデル5 = fitted(tw_lm5)) |>
  pivot_longer(starts_with("モデル"), # モデルをラベルとして予測値をまとめる
               names_to = "model", values_to = "fitted") |>
  ggplot(aes(x = 気温, y = fitted)) + # 気温の実測値をx軸，予測値をy軸で表示
  geom_abline(slope = 1, intercept = 0, colour = "red") + # 基準線
  geom_point(aes(colour = model, shape = model)) + # 予測値をモデル別に表示
  labs(y = "あてはめ値") 

#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x) |>
    add_significance_stars(
      hide_se = TRUE,
      pattern = "{estimate}({std.error}){stars}"
    ) |>
    modify_header(label = "**変数**",
                  estimate = "**係数(SE)**") |>
    remove_abbreviation() |>
    modify_abbreviation("SE = 標準誤差") |>
    add_glance_table(
      include = c(r.squared,adj.r.squared))}
tw_gt <- 
  tbl_merge(
    tbls = list(
      my_gts(tw_lm1),
      my_gts(tw_lm2),
      my_gts(tw_lm3),
      my_gts(tw_lm4),
      my_gts(tw_lm5)),
    tab_spanner = paste0("モデル",1:5)) |>
  modify_table_body(
    ~.x |> arrange(factor(variable,
                          levels = c("気圧",
                                     "日射",
                                     "湿度",
                                     "雲量")))
  ) |>
  as_gt()

tw_gt |>
  tab_options(table.font.size = px(16 * 1.2))

#| tbl-cap: "決定係数によるモデルの比較"
tw_gt |> 
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              latex.tbl.pos = "h") |>
  as_latex()

#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x) |>
    add_significance_stars(
      hide_se = TRUE,
      pattern = "{estimate}({std.error}){stars}"
    ) |>
    add_glance_table(include = c(r.squared,statistic,p.value)) |>
    modify_header(label = "**変数**",
                  estimate = "**係数(SE)**") |>
    remove_abbreviation() |>
    modify_abbreviation("SE = 標準誤差")
}
tw_gt <- 
  tbl_merge(
    tbls = list(
      my_gts(tw_lm1),
      my_gts(tw_lm2),
      my_gts(tw_lm3),
      my_gts(tw_lm4),
      my_gts(tw_lm5)),
    tab_spanner = paste0("モデル",1:5)) |>
  modify_table_body(
    ~.x |> arrange(factor(variable,
                          levels = c("気圧",
                                     "日射",
                                     "湿度",
                                     "雲量")))
  ) |>
  as_gt()

tw_gt |>
  tab_options(table.font.size = px(16 * 1.2))

#| tbl-cap: "F統計量によるモデルの比較"
tw_gt |> 
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              latex.tbl.pos = "h") |>
  as_latex()

#' 関数 gtsummary::tbl_regression() を利用して結果をまとめる
my_gts <- function(x){
  tbl_regression(x, intercept = TRUE) |>                      # 標準の表
    modify_column_unhide(columns = c(statistic)) |> # 標準誤差とt統計量を表示
    add_significance_stars(
      pattern = "{estimate}({std.error})"
    ) |>
    add_significance_stars(
      hide_ci = TRUE,
      hide_se = TRUE,
      hide_p = FALSE,
      pattern = "{p.value}{stars}"
    ) |>
    modify_header(label = "**変数**",
                  estimate = "**係数(SE)**",
                  statistic = "**t統計量**",
                  p.value = "**p値**") |>
    remove_abbreviation() |>
    modify_abbreviation("SE = 標準誤差")
} 
my_gts <- function(x){
  tbl_regression(x, intercept = TRUE) |>            # 標準の表
    modify_column_unhide(columns = c(statistic)) |> # 標準誤差とt統計量を表示
    add_significance_stars(
      hide_ci = TRUE,
      hide_se = FALSE,
      hide_p = FALSE,
      pattern = "{p.value}{stars}"
    ) |>
    modify_header(label = "**変数**",
                  estimate = "**係数**",
                  std.error = "**SE**",
                  statistic = "**t統計量**",
                  p.value = "**p値**") |>
    remove_abbreviation() |>
    modify_abbreviation("SE = 標準誤差")
} 
tw_gt <- 
  tbl_merge(
    tbls = list(
      ## my_gts(tw_lm1),
      ## my_gts(tw_lm2),
      my_gts(tw_lm3),
      my_gts(tw_lm4),
      my_gts(tw_lm5)),
    tab_spanner = paste0("モデル",3:5)) |>
  ## tab_spanner = paste0("モデル",1:5)) |>
  modify_table_body(
    ~ .x |> arrange(factor(variable,
                           levels = c("(Intercept)",
                                      "気圧",
                                      "日射",
                                      "湿度",
                                      "雲量")))
  ) |>
  as_gt()

tw_gt |>
  tab_options(table.font.size = px(16 * 1.2))

#| tbl-cap: "t統計量によるモデルの比較"
tw_gt |>
  tab_options(table.font.size = 12,
              heading.title.font.size = "normal",
              heading.subtitle.font.size = "small",
              latex.tbl.pos = "h") |>
  as_latex()

#' 診断プロット (モデル3)
autoplot(tw_lm3)

#' 診断プロット (モデル4)
autoplot(tw_lm4)

#' 診断プロット (モデル5)
autoplot(tw_lm5)
