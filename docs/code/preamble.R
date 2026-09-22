#' このファイルには資料の中の例題や演習問題の解答例があります
#' コメント行は "#'" で開始され，RStudio では緑(既定値)で表示されます
#' コメント内の "@..." は
#'   @exercise 例題 (スライドの説明と並行して実習)
#'   @practice 演習 (時間を取って各自で実習)
#'   @notes 注意 (講義の補足説明)
#'   @appendix 補遺 (講義では扱い切れなかった付加的な事柄)
#' のタグで，RStudio では青(既定値)でハイライトされます
#'
library(conflicted)
conflicts_prefer(
    dplyr::filter(),
    dplyr::select(),
    dplyr::lag(),
    )
library(tidyverse)
library(ggfortify)     # 分析結果の視覚化にggplotを利用するためのパッケージ
library(GGally)
library(ggrepel)
library(gt)            # 表を作成するためのパッケージ
library(gtsummary)
library(broom)
library(broom.helpers) # gtsummary のいくつかの関数で利用(インストールされていれば不要)
library(MASS)          # パッケージ MASS に含まれる判別分析のための関数を利用
library(cluster)       # クラスタ分析のためのパッケージ
library(ggdendro)      # ggplot でデンドログラムを描くためのパッケージ
library(tsibble)       # 時系列を扱うためのパッケージ
library(feasts)        # ggplotで時系列を扱うための拡張パッケージ
library(fable)         # 時系列関連
library(ggtime)        # 時系列関連
#' 日本語表示の設定
#' 出力形式に応じた作図デバイス
if (knitr::is_latex_output()) knitr::opts_chunk$set(dev = "cairo_pdf")

#' 日本語表示の設定
if (Sys.info()["sysname"] == "Darwin") {
    jp_font <- if (knitr::is_latex_output()) {
                   "Hiragino Maru Gothic ProN"   # cairo は fontconfig 経由なのでファミリ名
               } else {
                   "HiraMaruProN-W4"             # quartz/AGG は PostScript 名
               }
    theme_update(text = element_text(family = jp_font))
    update_geom_defaults("text", list(family = theme_get()$text$family))
    update_geom_defaults("label", list(family = theme_get()$text$family))
    update_geom_defaults("text_repel", list(family = theme_get()$text$family))
    update_geom_defaults("label_repel", list(family = theme_get()$text$family))
} else {
    jp_font <- NULL
}
