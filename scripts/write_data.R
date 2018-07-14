# get intra-day trading data
library(data.table)
library(magrittr)

# ticker list has all crsp total stock market constituents
tickers <- fread("ticker_list.csv")$Ticker %>% 
  unique

get_data <- function(ticker){
  data <- ticks(ticker)
  
  setorder(data, Date)
  
  data[,index := seq_along(Date), by = Ticker]
  data[,Day := as.Date(Date)]
  
  ticker_folder <- paste0("./data/",ticker)
  
  if (!dir.exists(ticker_folder)) {
    dir.create(ticker_folder)
  }
  
  data <- split(data, by = "Day")
 
}

write_date_file <- function(data){
  
  as_of_date <- unique(as.Date(data$Date))
  
  data_file <- paste0(ticker_folder,"/",as_of_date,
                      "_data.csv")
  
  if(!file.exists(data_file)){
    fwrite(data, data_file)
  }
  
}

log <- function(path, message){
  if (!file.exists(path)) {
    file.create(path)
  }
  con <- file(path)
  write(message, con, append = T)
  close(con)
}

log_path <- function(){
  if(exists("logging_path",envir = parent.frame())){
    get("logging_path",envir = parent.frame())
  } else {
    "error.log"
  }
}

write_trading_data <- function(tickers){
  for(i in 1:length(tickers)){
    ticker <- tickers[i]
    log_dir <- paste0("./logs/",ticker,"/")
    if (!dir.exists(log_dir)) {
      dir.create(log_dir)
    }
    logging_path <- paste0(log_dir,Sys.Date(),".log")
    
    tryCatch({
      get_data(ticker) %>% 
      lapply(., write_date_file)
    },
    error = function(e){
      log(path = log_path(), message = paste0(e, 
                                              collapse = "\n"))
      })
  }
}

write_trading_data("AAPL")

