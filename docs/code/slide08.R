### 第8講 サンプルコード
library(conflicted)
library(tidyverse)
library(MASS) # 判別分析のため
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(broom)
library(gt)
library(gtsummary)
library(ggfortify)
library(ggrepel)
#' macOSのための日本語表示の設定
if(Sys.info()["sysname"] == "Darwin") { # macOSか調べる
  jp_font <- "HiraMaruProN-W4"
  theme_update(text = element_text(family = jp_font))
} else {jp_font <- NULL}

#' 判別分析の例
#' 表の作成
bio_data <- MASS::biopsy |>
  as_tibble() |>
  na.omit() # NA を除く

bio_data |> View() # 左上ペインに表として表示

bio_data |>
  select(!ID) |> # IDを除く
  GGally::ggpairs(diag = list(mapping = aes(colour = class)),
                  lower = list(mapping = aes(colour = class)))

autoplot(bio_data |> select(where(is.numeric)) |> prcomp(), # 主成分分析
         data = bio_data,
         colour = "class")

bio_lda <- lda(class ~ . -ID, data = bio_data)
bio_data |>
    mutate(x = predict(bio_lda)[["x"]]) |>
    ggplot(aes(x = x)) +
    geom_histogram(aes(fill = class), show.legend = FALSE) +
    facet_grid(class ~ .)
