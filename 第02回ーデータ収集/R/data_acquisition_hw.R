#==========================================
# Data Acquisition Program for Weather Data
#
# @date 2022-08-27
# @author Hisashi Takeda, Ph.D.
 
rm(list = ls())
library(rvest)
library(RSQLite)
Sys.setlocale("LC_TIME", "ja_JP.UTF-8")

DB <- 'weather_hw.db' # データベース名

# 気象観測所
site <- data.frame(
  id   = 47662,   # 番号
  name = 'Tokyo') # 名称

# 取得期間設定 
lt.fr <- as.POSIXlt('2021-12-30')
lt.to <- as.POSIXlt('2022-01-01')
lts   <- as.POSIXlt(seq(lt.fr, lt.to, by = 'days'))

# データベース接続
conn <- dbConnect(RSQLite::SQLite(), DB)
dbSendQuery(conn, paste('DROP TABLE IF EXISTS', site$name)) # 既存テーブル削除
  
for (i in seq_along(lts))
{
  year  <- 1900 + lts[i]$year
  month <- 1 + lts[i]$mon
  day   <- lts[i]$mday
  
  url <- 
    paste0('https://www.data.jma.go.jp/obd/stats/etrn/view/hourly_s1.php?prec_no=44&block_no=', site$id, '&year=', year, '&month=', month, '&day=', day, '&view=')
  
  cat('URL:', url, fill = T) # 作成したURLを表示
  
  tbl <- read_html(url) %>% html_table()
  d0 <- as.data.frame(tbl[[5]])
  str(d0)
  colnames(d0)
  
  # 日時整形
  hour <- d0[-1, '時'] # 1列目は時刻1～24（-1:一行目は不要なため削除）
  datetime <- as.POSIXlt(paste(lts[i], hour), format = '%Y-%m-%d %H')
  
  # 日照時間の補間（日没時間の値がNAになっている→日照時間は0）
  sunshine <- as.double(d0[-1, 11])
  sunshine[is.na(sunshine)] <- 0.0
  
  # 書込用テーブル作成
  d1 <- data.frame(
    site.id   = as.integer(site$id), # 整数型
    site.name = site$name,
    datetime  = format(datetime, '%Y-%m-%d %H:00'),
    temp      = as.double(d0[-1, 5]),  # 倍精度浮動小数点型
    humidity  = as.integer(d0[-1, 8]), # 整数型            【湿度追加】
    sunshine  = sunshine,              # 倍精度浮動小数点型【日照時間追加】
    wind      = d0[-1, 10])
 
  try(dbWriteTable(conn, site$name, d1, append = T)) # テーブル追記書込
  
  Sys.sleep(runif(1, min = 1, max = 2)) #【重要】システムスリープ（忘れたら0点）
}

res <- dbSendQuery(conn, 'SELECT * FROM Tokyo') # データ確認
dbFetch(res)       # 選択結果取得
dbClearResult(res) # 選択結果解放
dbDisconnect(conn) # データベース接続解除

#
# 採点項目（10点満点）
#

# 1. システムスリープが入っている（無ければ全評点0とする）
# 2. データベースが作成されている（+5点）
# 3. PRIMARY KEYが複数設定されている（+1点）
# 4. 湿度のデータ型はDB上でINTEGERになっている（+1点）
# 5. 日照時間のデータ型はDB上でREALになっている（+1点）
# 6. try()関数が使われている（+1点）
# 7. 日照時間の補間が行われている（+1点）
