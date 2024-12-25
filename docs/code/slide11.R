### 第11講 サンプルコード
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::select(),
  dplyr::lag(),
)
library(tidyverse)
library(ggfortify)
library(cluster)
library(ggdendro)
library(gt)

om_data <- bind_cols(
  read_csv(file="data/omusubi.csv"),
  read_csv(file="data/prefecture.csv"))
om_subset <- om_data |>
  select(ume:etc,jp) |>
  slice(-c(8:14,24:30)) |>
  column_to_rownames(var = "jp") |>
  set_names(c("梅","鮭","昆布","鰹","明太子","鱈子","ツナ","その他")) 
n <- nrow(om_subset)

om_subset |> View() # 左上ペインに表として表示
