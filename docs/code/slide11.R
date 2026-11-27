### 第11講 資料

#| fig-cap: "凝集的手続きの例"
library(cluster)
js_data <- 
  read_csv(file="data/japan_social2.csv")
myPlot <- function(k) {
  if(Sys.info()["sysname"]=="Darwin"){par(family = jp_font)}
  tmpa <- js_data |>
    slice(8:14) |>
    select(where(is.double),都道府県名) |>
    column_to_rownames(var = "都道府県名")
  # tmpa <- js_data[8:14,]
  tmpb <- list(c(1,2,3,4,1,6,7),
               c(1,2,3,1,1,6,7),
               c(1,2,2,1,1,6,7),
               c(1,2,2,1,1,6,1),
               c(1,1,1,1,1,6,1),
               c(1,1,1,1,1,1,1))
  clusplot(x=tmpa,
           clus=c(1,2,3,4,5,6,7),
           diss=FALSE,
           stand=TRUE, lines=0, labels=3, 
           main=NULL, sub=NULL, cex=1.5,
           xlim=c(-2.5,2.5), ylim=c(-2.5,2.5),
           xaxt="n", yaxt="n", ann=FALSE,
           col.p="blue", col.txt="darkgray", col.clus="white", shade=FALSE)
  if(k>0) {
    for(i in 1:k) {
      clusplot(x=tmpa,
               clus=tmpb[[i]],
               diss=FALSE, cex=0.2,
               stand=TRUE, add=TRUE, span=FALSE,
               lines=0, lwd=3, col.p="blue", col.clus="orange")
    }
  }
}
if(Sys.info()["sysname"]=="Darwin"){par(family = jp_font)}
par(mfrow=c(4,2), mar=rep(0.5,4))
for(i in 0:6) myPlot(i)
js_data |>
  slice(8:14) |>
  select(where(is.double),都道府県名) |>
  column_to_rownames(var = "都道府県名") |>
  scale() |>
  agnes() |>
  plot(which.plots=2, main="",sub="",xlab="")

#| include: false
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
