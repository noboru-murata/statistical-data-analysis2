### 第8講 資料

#| include: false
#' 判別分析の例
#' 表の作成
bio_data <- MASS::biopsy |>
  as_tibble() |>
  na.omit() # NA を除く

bio_data |> View() # 左上ペインに表として表示

#| fig-width: 5.6
  bio_data |>
    select(!ID) |> # IDを除く
    ggpairs(upper = list(continuous = wrap("cor", size = 2.5)),
            diag = list(continuous = wrap("densityDiag", alpha = 0.7),
                        mapping = aes(colour = class)),
            lower = list(continuous = wrap("points", alpha = .7),
                         mapping = aes(colour = class))) +
theme(strip.text = element_text(size = 8),
      axis.text = element_text(size = 5))

#| fig-width: 6.4
bio_data |>
  select(where(is.numeric)) |>
  prcomp() |> # 主成分分析
  autoplot(data = bio_data,
           asp = 1,
           colour = "class")

#| fig-width: 5.6
bio_lda <- lda(class ~ . -ID, data = bio_data)
bio_data |>
  mutate(x = predict(bio_lda)[["x"]]) |>
  ggplot(aes(x = x)) +
  geom_histogram(aes(fill = class), alpha = 0.7,
                 show.legend = FALSE) +
  facet_grid(class ~ .)
