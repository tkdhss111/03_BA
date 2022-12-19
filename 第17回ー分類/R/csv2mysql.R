rm(list = ls())
#sudo apt install -y libmariadb-dev
#sudo apt install -y libmysqlclient-dev
#install.packages(c("RMariaDB", "RMySQL"))
library(RMySQL)
library(DBI)

d0 <- read.csv(file = "univ_exam_data.csv")
head(d0)

# Use admin privilege to upload CSV to MySQL
# Then, check downloading with user privilege.
con <- dbConnect(RMySQL::MySQL(), 
#  user = "guest", password = "bemy", # NB: only SELECT is granted
#  dbname = "ba", host = "stats.dip.jp")
user = "admin", password = "lazydog11",
dbname = "ba", host = "localhost")

#dbSendStatement(con, "set global local_infile=true") # no guest privilege
#dbWriteTable(con, "univ_exam", d0, overwrite = T, row.names = F) # no guest privilege
d1 <- dbReadTable(con, "univ_exam")
head(d1)

dbDisconnect(con)
