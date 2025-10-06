### 第2講 サンプルコード

#' 最初に一度だけ以下のいずれかを実行しておく
#'  - Package タブから tidyverse をインストール
#'  - コンソール上で次のコマンドを実行 'install.packages("tidyverse")'
#' tidyverse パッケージの読み込み
library(tidyverse)

#' @notes
#' library(tidyverse) を実行すると読み込まれたパッケージが表示される
#' 同名の関数が存在する場合には "Conflicts" として衝突する関数名が表示される
#' 衝突する場合はパッケージ名を明示的に付ける
#' dplyer::filter() # データフレームの抽出のための関数
#' stats::filter() # 時系列処理のための線形フィルタ関数
#' 名前の衝突による不具合を避けたい場合は conflicted パッケージの利用を推奨
#' library(conflicted)

#' @exercise データフレームの作成

#' 同じ長さのベクトル(関数 base::c() で作成)を並べる (関数 tibble::tibble())
#' (... <- ...) は代入した結果を表示
(foo <- tibble(one = c(1,2,3),two = c("AB","CD","EF"))) 
(bar <- tibble(three = c("x","y","z"),four = c(0.9,0.5,-0.3)))
#' データフレームを結合する (関数 dplyr::bind_cols())
(baz <- bind_cols(foo,bar)) # bind columns

#' @notes
#' "foo", "bar", "baz" は使い捨ての変数名として良く用いられる
#' その他 "tmp", "temp" なども用いられることが多い

#' ダウンロードしたファイルの読み込み
#' ファイル名 pcr_case_daily.csv として作業ディレクトリの data 以下に保存
pcr_data <- read_csv("data/pcr_case_daily.csv") # 一般的な読み込み方
# View(pcr_data) # 中身を左上ペインに表示
(pcr_colnames <- names(pcr_data)) # 列名を確認して保存 colnames(pcr_data) でも良い
names(pcr_data) <- # 列名を扱い易いように英語略記に変更する
  c("date","niid","ciq","hc","ai","univ","mi","sub","si","total")
#' National Institute of Infectious Diseases
#' Customs-Immigration-Quarantine
#' Health Center
#' Administrative Inspection
#' University
#' Medical Institution
#' subtotal
#' Self Inspection
#' total
pcr_data # 中身を確認(10行だけ表示される)
#' 以降の処理のために date 列を関数 lubridate::date() で date 型に変換する
#' 列の変換・追加などには関数 dplyr::mutate() を用いる
(pcr_data <- mutate(pcr_data, date = date(date)))

#' @exercise 要素の選択

z <- tibble(one = c(1,2,3),
                two = c("AB","CD","EF"),
                three = 6:8)
z[1,2] # 1行2列の要素を選択
z[-c(1,3),] # 1,3行を除外
z[c(TRUE,FALSE,TRUE),] # 1,3行を選択
z[,"two"] # 列名"two"を選択(1列のデータフレームになる)
z["two"]  # 上記と同様の結果
z[,c("one","three")] # 列名"one"と"three"を選択(データフレームになる)
z[c("one","three")]  # 上記と同様の結果
z[["two"]] # 列名"two"のベクトルを選択(1列の場合しか使えない)
z$two      # 上記と同様の結果

#' @exercise 部分集合の選択

#' 前に作成したデータフレーム z を用いた例
(foo <- filter(z, three >= 7)) # 列 three の値が7以上の行を選択
(bar <- select(foo, c(one, three))) # 列 one,three を選択
#' パイプを用いると以下のように書ける
z |> filter(three >= 7) |> select(one, three)
#' 別の例
z |> 
  filter(one != 2) |>        # 列 one の値が2でない行を選択
  select(starts_with("t")) # 列 "t"wo,"t"hree を選択

#' @exercise データフレームの形式の変更

#' 練習問題の成績表を用いた例
pivot_longer(grade_data,
             !name, # name 列以外をまとめる
             names_to = "subject") # もとの列名を subject 列にまとめる
#' この例ではもとのデータフレームに "name" という列があるため
#' 既定値は使えないので，科目を表す "subject" を用いている

#' @exercise 列ごとの集計

#' 練習問題のデータフレームを用いた例
grade_data |>
  summarise(math_mean = mean(math), nums = n()) # 数学の平均を求める
grade_data |>
  summarise(across(!name, mean)) # 名前の列以外の平均を求める

#' @notes
#' 関数 dplyr::across() の列の指定にはさまざまな関数が使える
#' 詳細は '?dplyr::across' を参照

#' @exercise グループごとの集計

#' pcr_data を用いた例
#' 医療機関(mi)のPCR件数を各月で集計する
#' 日付の扱いに関数 lubridate::year(), lubridate::month() を利用
pcr_data |>
  group_by(year = year(date), month = month(date)) |> # 年と月でグループ化
  summarise(mi_total = sum(mi)) # mi の合計値を計算する

#' @notes
#' 関数 dplyr::mutate() で新たな列を加えておくと
#' 上記の処理は関数 summarise() のみでも可能(実験的な実装 .by)
pcr_data |>
  mutate(year = year(date), month = month(date)) |>
  summarise(mi_total = sum(mi), .by = c(year, month))
#' 関数 lubridate::month() は月名での表示も可能
pcr_data |> # 短縮形
  mutate(year = year(date), month = month(date, label = TRUE)) |>
  summarise(mi_total = sum(mi), .by = c(year, month))
pcr_data |> # 月名
  mutate(year = year(date), month = month(date, label = TRUE, abbr = FALSE)) |>
  summarise(mi_total = sum(mi), .by = c(year, month))
#' ただし名前は言語環境に依存するので注意
#' 強制的に英語にするには
#' Sys.setlocale(category = "LC_TIME", locale = "C")
#' などを指定すればよい．もとに戻すには
#' Sys.setlocale(category = "LC_TIME", locale = "")

#' @exercise 折れ線グラフの描画

#' 行政検査(ai)と医療機関(mi)の検査件数の推移の視覚化

pcr_data |> # パイプ演算子でデータフレームを関数 ggplot2::ggplot() に渡す
  ggplot(aes(x = date)) + # date をx軸に指定
  geom_line(aes(y = ai), colour = "blue") + # 行政検査を青
  geom_line(aes(y = mi), colour = "red") +  # 医療機関を赤
  labs(y = "number of tests") # y軸のラベルを変更

#' @notes
#' パイプ演算子を使わずに
#'   ggplot(pcr_data, aes(x = date)) + ...
#' としても良い．

#' 全ての機関の検査件数の推移の視覚化
#' 複数のデータを描画するためにはデータフレームを適切に書き換える必要がある

pcr_data |> select(!c(sub,total)) |> # 集計値を除く
  pivot_longer(!date, names_to = "organ", values_to = "nums") |> 
  ggplot(aes(x = date, y = nums, colour = organ)) + geom_line() +
  labs(title = "PCR Tests in Various Organizatios",
       x = "Date", y = "Number of Tests") # xy軸のラベルを変更

#' @notes
#' 関数 tidyr::pivot_longer() はデータフレームを縦長に変更する
#' pivot_longer(!date,              # date 列以外をまとめる
#'              names_to = "organ", # organ に元の列の名前を保存
#'              values_to = "nums") # nums に値を保存

#' それぞれを別のグラフとする場合には
#' 関数 ggplot2::facet_wrap() や関数 ggplot2::facet_grid() を用いると良い

pcr_data |> select(!c(sub,total)) |> 
  pivot_longer(!date, names_to = "organ", values_to = "nums") |> 
  ggplot(aes(x = date, y = nums, colour = organ)) +
  labs(title = "PCR Tests in Various Organizatios", x = "Date", y = "Number of Tests") +
  geom_line(show.legend  =  FALSE) + # 凡例を消す
  facet_grid(vars(organ)) # "organ" ごとに異なる図を並べる

#' @exercise 散布図の描画

#' 国立感染症研究所(niid)と医療機関(mi)の検査件数の関係

if(Sys.info()["sysname"] == "Darwin") { # MacOSか調べて日本語フォントを指定
  theme_update(text = element_text(family = "HiraginoSans-W4"))}
pcr_data |> 
  ggplot(aes(x = niid, y = mi)) + # x軸を niid，y軸を mi に設定
  geom_point(colour = "blue", shape = 19) + # 色と形を指定(点の形は '?points' を参照)
  labs(x = pcr_colnames["niid"], y = pcr_colnames["mi"]) # 軸の名前を指定

#' @notes
#' テーマ (theme) は ggplot の背景や色の既定値を設定する機能である
#' 関数 ggplot2::theme_update() は設定の書き換えを行う関数で，
#' 書き換えられた設定はテーマを変更しない限り有効となる

#' 各軸を対数表示に変更

pcr_data |> 
  ggplot(aes(x = niid, y = mi)) + 
  geom_point(colour = "blue", shape = 19) + 
  scale_x_log10() + scale_y_log10() + # 各軸を対数で表示
  labs(x = pcr_colnames["niid"], y = pcr_colnames["mi"])

#' @exercise 散布図行列の描画

#' 各検査機関での検査件数の関係
library(GGally)

pcr_data |>
  select(!c(date,sub,total)) |> # 日付と集計値を除いて必要なデータフレームに整形
  ggpairs() # 標準の散布図行列

#' 四半期ごとに分類して色分けして表示する

pcr_data |> select(!c(sub,total)) |> # 日付から四半期の因子を作成
  mutate(quarter = as_factor(quarter(date, with_year = TRUE))) |>
  ggpairs(columns = 2:8, columnLabels = pcr_colnames[-c(1,8,10)], axisLabels = "none",
          aes(colour = quarter), legend = c(2,1), # 四半期ごとに色づけて(1,1)の凡例を使用
          upper = "blank", diag = list(continuous = "barDiag")) +
  theme(legend.position = "top") # 凡例を上に表示

#' @notes
#' 上記の列や列名の選択を要素の名称で行う場合には例えば以下のように書くことができる
#'   columns = which(!(names(pcr_data) %in% c("date","g","i")))
#'   columnLabels = pcr_names[!(names(pcr_data) %in% c("date","g","i"))]

#' @exercise ヒストグラムの描画

#' 行政検査(ai)での検査件数の分布
pcr_data |>
  ggplot(aes(x = ai)) + # 分布を描画する列を指定
  geom_histogram(bins = 30, fill = "lightblue", colour = "blue") +
  labs(x = pcr_colnames["ai"], y = "頻度", title = "検査件数のヒストグラム")

#' @notes
#' 各ビンの頻度を表示するためには例えば以下のようにすればよい
pcr_data |>
  ggplot(aes(x = ai)) + # 分布を描画する列を指定
  geom_histogram(bins = 30, fill = "lightblue", colour = "blue") +
  geom_text(stat="bin", bins = 30, colour = "darkblue", size = 3, 
            aes(label = after_stat(count), y = after_stat(count) + 2)) +
  labs(x = pcr_colnames["ai"], y = "頻度", title = "検査件数のヒストグラム")

#' @exercise 箱ひげ図の描画

#' 大学等(univ)での検査件数の分布(2021年分)
pcr_data |>
  filter(year(date) == 2021) |> # 2021年を抽出
  mutate(date = as_factor(month(date))) |> # 月を因子化する
  ggplot(aes(x = date, y = univ)) + # 月毎に集計する
  geom_boxplot(fill = "orange") + # 塗り潰しの色を指定
  labs(title = "月ごとの検査件数 (2021年)", x = "月", y = pcr_colnames["univ"])

#' @exercise 棒グラフの描画

#' 機関ごとの月の検査件数の推移 (2021年分)
pcr_data |>
  filter(year(date) == 2021) |>
  mutate(month = as_factor(month(date))) |> # 月を作成
  select(!c(date,sub,total)) |> # 機関に限定
  group_by(month) |> # 月でグループ化
  summarize(across(everything(), sum)) |> # 全て(月以外)を集計
  pivot_longer(!month, names_to = "organ", values_to = "nums",
               names_transform = list(organ = as_factor)) |>
  ## 最後のオプションは organ 列のラベルを出てきた順で因子化して元の列の並びにしている
  ggplot(aes(x = organ, y = nums, fill = month)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  theme(legend.position = "top") + guides(fill = guide_legend(nrow = 1))

#' @exercise 中心極限定理

#' 確率変数の分布の設定 (例 : 区間[-1,1]の一様乱数)
mc_rand <- function(n) { # n個の乱数を生成
  return(runif(n, min = -1, max = 1))
}
#' 標本平均の計算
mc_mean <- function(n) { # n個のデータで計算
  return(mean(mc_rand(n)))
}
#' Monte-Carlo実験
set.seed(123) # 実験を再現したい場合はシード値を指定する
mu <- 0; sigma <- sqrt(1/3) # 理論平均と標準偏差
mc_num <- 5000 # 実験の繰り返し回数
for(n in c(1,2,4,8,16)){ # nを変えて実験
  p <- tibble(x = replicate(mc_num, mc_mean(n))) |> # 繰り返し実験し標本平均を記録
    ggplot(aes(x)) + 
    geom_histogram(aes(y = after_stat(density)), # 密度表示
                   fill = "orchid", alpha = 0.5, # 塗り潰しの色
                   colour = "purple") + # 境界線の色
    geom_function(fun = \(x) dnorm(x, mean = mu, sd = sigma/sqrt(n)),
                  colour = "orange", linewidth = 1.5) + # 理論曲線を重ねる
    labs(x = expression(bar(X)), # x軸の表示
         title = paste0("n=", n)) # タイトルにnを記載
  print(p) # for 文の中では明示的に print する必要がある
}

#' コイン投げの試行 (いろいろな書き方があるので以下は一例)
mc_trial <- function(){
  while(TRUE){ # 永久に回るループ
    if(rbinom(1, size = 1, prob = 0.5)==1){return("A")} # Aが表で終了
    if(rbinom(1, size = 1, prob = 0.5)==1){return("B")} # Bが表で終了
    #' どちらも裏ならもう一度ループ
  }
}
#' Monte-Carlo実験
set.seed(8888) # 実験を再現したい場合はシード値を指定する
mc_num <- 10000 # 実験回数を設定 
mc_data <- replicate(mc_num, mc_trial()) 
#' 簡単な集計
table(mc_data)        # 頻度
table(mc_data)/mc_num # 確率(推定値)
