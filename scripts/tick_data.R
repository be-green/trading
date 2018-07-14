# gets intraday trading data from google finance
process_ticks <- function(ticker, interval = 60, period = 50){

  link <- paste0("https://www.google.com/finance/getprices?i=",
                 interval,
                 "&p=",
                 period,
                 "d&f=d,o,h,l,c,v&df=cpct&q=",
                 ticker)

  test <- httr::GET(link)

  content <- rawToChar(test$content) %>%
    textConnection %>%
    readLines

  content <-
    list(header = content[1:7],
         data = content[8:length(content)])
  
  make_date <- function(DATE){
    convert <- function(DATE){
      if (stringr::str_detect(DATE, "a")) {
        as.numeric(stringr::str_replace_all(DATE,"a",""))
      } else {
        NA
      }
    }
    sapply(DATE, convert)
  }

  stock_prices <- data.table::rbindlist(
      lapply(
        stringr::str_split(content$data,","),
        function(x) data.table::as.data.table(t(data.table::data.table(x)))
      ),
      fill = T
    )

  coalesce <- function(...) {
    suppressWarnings({
      Reduce(function(x, y) {
        i <- which(is.na(x))
        x[i] <- y[i]
        x},
        list(...))
    })

  }

  data.table::setnames(stock_prices, c("DATE","CLOSE","HIGH","LOW","OPEN","VOLUME"))
  stock_prices[,START_DATE := zoo::na.locf(make_date(DATE))]
  stock_prices[,INTERVAL := coalesce(as.numeric(DATE)*interval,0)]
  stock_prices[,DATE := anytime::anytime(START_DATE + INTERVAL)]
  stock_prices[!is.na(DATE),
               .(Date = DATE,
                 Ticker = ticker,
                 Close = as.numeric(CLOSE),
                 High = as.numeric(HIGH),
                 Low = as.numeric(LOW),
                 Open = as.numeric(OPEN),
                 Volume = as.numeric(VOLUME))]

}

try_ticks <- function(ticker, interval = 60, period = 1){
    process_ticks(ticker, interval, period)
}

# takes in a list of tickers, returns a processed data.table 
# from process_ticks
ticks <- function(...){
  args <- list(...)
  if (length(args[[1]]) > 1)
    args <- args[[1]]
  data.table::rbindlist(lapply(args, try_ticks))
}


