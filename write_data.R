# get intra-day trading data
library(data.table)
library(magrittr)

# ticker list has all crsp total stock market constituents
tickers <- fread("ticker_list.csv")$Ticker %>% 
  unique

data <- ticks(tickers)

setorder(data, Date)

data[,index := seq_along(Date), by = Ticker]

fwrite(data, paste0("data/",Sys.Date(),"_crsp_constituents.csv"))

