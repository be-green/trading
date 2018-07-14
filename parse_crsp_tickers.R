# parse excel source
library(openxlsx)
library(data.table)
library(magrittr)

xl <- read.xlsx("CRSP-US-Total-Market.xlsx") %>% 
  as.data.table

ticker_list <- unique(xl[,.(Ticker, Company)])

fwrite(ticker_list, "ticker_list.csv")
