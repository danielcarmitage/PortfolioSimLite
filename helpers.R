quandl_equity_download <- function(symbols, asset_name){
  quandl_equity_data <- Quandl(symbols, authcode = quandl_key)
  quandl_equity_data <- data.table(name = rep(asset_name, length(quandl_equity_data[,1])), date = quandl_equity_data[,1], open = quandl_equity_data[,2], high = quandl_equity_data[,3], low = quandl_equity_data[,4], close = quandl_equity_data[,5], volume = quandl_equity_data[,6], dividend = quandl_equity_data[,7], split = quandl_equity_data[,8])
  setorder(quandl_equity_data, date)
  
  split_index <- quandl_equity_data[split != 1]
  
  split_adjustment <- function(split_index2){
    quandl_equity_data[(quandl_equity_data$date < as.Date(split_index2["date"])), open := open/as.numeric(split_index2["split"])]
    quandl_equity_data[(quandl_equity_data$date < as.Date(split_index2["date"])), high := high/as.numeric(split_index2["split"])]
    quandl_equity_data[(quandl_equity_data$date < as.Date(split_index2["date"])), low := low/as.numeric(split_index2["split"])]
    quandl_equity_data[(quandl_equity_data$date < as.Date(split_index2["date"])), close := close/as.numeric(split_index2["split"])]
  }
  
  if(nrow(split_index) > 0){
    apply(split_index, 1, split_adjustment)
  }
  
  return(quandl_equity_data)
}

quandl_fx_download <- function(symbols){
  symbols <- toupper(symbols)
  quandl_code <- paste("CURRFX/", symbols, sep="")
  quandl_fx_data <- try(Quandl(quandl_code, authcode = quandl_key), silent = T)
  if(is.data.frame(quandl_fx_data) == F)
  {
    return(quandl_fx_data = NULL)
  }
  quandl_fx_data <- data.table(name = rep(symbols, length(quandl_fx_data[,1])), date = quandl_fx_data[,1], rate = quandl_fx_data[,2], high = quandl_fx_data[,3], low = quandl_fx_data[,4])
  setorder(quandl_fx_data, date)
  
  return(quandl_fx_data)
}

quandl_futures_download <- function(symbols, asset_name){
  quandl_futures_data <- Quandl(symbols, authcode = quandl_key)
  quandl_futures_data <- data.table(name = rep(asset_name, length(quandl_futures_data[,1])), date = quandl_futures_data[,"Date"], open = quandl_futures_data[,"Open"], high = quandl_futures_data[,"High"], low = quandl_futures_data[,"Low"], settle = quandl_futures_data[,"Settle"], volume = quandl_futures_data[,"Volume"])
  setorder(quandl_futures_data, date)
  
  return(quandl_futures_data)
}

builder_simulate <- function(quandl_equity_data, start_date, end_date, position){
  if(!exists("builder_data")){
    builder_data <<- NULL
  }
  if(!exists("builder_symbols")){
    builder_symbols <<- NULL
  }
  
  quandl_equity_data <- quandl_equity_data[date >= start_date & date <= end_date]
  quandl_equity_data$position_size <- rep(as.integer(position), nrow(quandl_equity_data)) 
  quandl_equity_data <- quandl_equity_data[, daily_notional := position_size*c(0,diff(close)) ][, acc_notional := cumsum(daily_notional)]
  
  builder_symbols <<- c(builder_symbols, as.character(unique(quandl_equity_data$name)))
  builder_data <<- rbind(builder_data, quandl_equity_data)
}

equity_data_globalize <- function(quandl_equity_data){
  if(!exists("equity_data")){
    equity_data <<- NULL
  }
  if(!exists("equity_symbols")){
    equity_symbols <<- NULL
  }
  
  equity_symbols <<- c(equity_symbols, as.character(unique(quandl_equity_data$name)))
  equity_data <<- rbind(equity_data, quandl_equity_data)
}

fx_data_globalize <- function(quandl_fx_data){
  if(!exists("fx_data")){
    fx_data <<- NULL
  }
  if(!exists("fx_symbols")){
    fx_symbols <<- NULL
  }
  
  fx_symbols <<- c(fx_symbols, as.character(unique(quandl_fx_data$name)))
  fx_data <<- rbind(fx_data, quandl_fx_data)
}

futures_data_globalize <- function(quandl_futures_data){
  if(!exists("futures_data")){
    futures_data <<- NULL
  }
  if(!exists("futures_symbols")){
    futures_symbols <<- NULL
  }
  
  futures_symbols <<- c(futures_symbols, as.character(unique(quandl_futures_data$name)))
  futures_data <<- rbind(futures_data, quandl_futures_data)
}

quandl_raw_futures_list <- function(){
  temp <- tempfile()
  cme_list_url <- "http://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Futures/CMEGroup.csv"
  download.file(cme_list_url, temp)
  cme_list <- data.table(read.csv(temp, header = F))
  setnames(cme_list, c("V1", "V2", "V3", "V4", "V5"), c("ticker", "exchange", "name", "months", "quandl.code"))
  unlink(temp)
  
#   temp <- tempfile()
#   ice_list_url <- "http://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Futures/ICE.csv"
#   download.file(ice_list_url, temp)
#   ice_list <- data.table(read.csv(temp, header = T))
#   setnames(ice_list, c("Symbol", "Exchange", "Name", "Months", "Quandl.Code"), c("ticker", "exchange", "name", "months", "quandl.code"))
#   unlink(temp)
#   
#   temp <- tempfile()
#   eurex_list_url <- "http://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Futures/EUREX.csv"
#   download.file(eurex_list_url, temp)
#   eurex_list <- data.table(read.csv(temp, header = T))
#   setnames(eurex_list, c("Symbol", "Exchange", "Name", "Months", "Quandl.Code"), c("ticker", "exchange", "name", "months", "quandl.code"))
#   unlink(temp)
  
  raw_futures_list <- rbind(cme_list)
#   raw_futures_list <- rbind(cme_list, ice_list, eurex_list)
  raw_futures_list[] <- lapply(raw_futures_list, as.character)
  setorderv(raw_futures_list, c("exchange", "ticker"))
  return(raw_futures_list)
  
  
#   cme_list_url <- "http://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Futures/CMEGroup.csv"
#   ice_list_url <- "http://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Futures/ICE.csv"
#   eurex_list_url <- "http://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Futures/EUREX.csv"
#   
#   cme_list <- data.table(read.csv(cme_list_url, header = F))
#   setnames(cme_list, c("V1", "V2", "V3", "V4", "V5"), c("ticker", "exchange", "name", "months", "quandl.code"))
#   ice_list <- data.table(read.csv(ice_list_url, header = T))
#   setnames(ice_list, c("Symbol", "Exchange", "Name", "Months", "Quandl.Code"), c("ticker", "exchange", "name", "months", "quandl.code"))
#   eurex_list <- data.table(read.csv(eurex_list_url, header = T))
#   setnames(eurex_list, c("Symbol", "Exchange", "Name", "Months", "Quandl.Code"), c("ticker", "exchange", "name", "months", "quandl.code"))
#   
#   raw_futures_list <- rbind(cme_list, ice_list, eurex_list)
#   raw_futures_list[] <- lapply(raw_futures_list, as.character)
#   setorderv(raw_futures_list, c("exchange", "ticker"))
#   return(raw_futures_list)
}

# quandl_get_list <- function(){
#   equity_list_url <- "https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/WIKI_tickers.csv"
#   futures_list_url <- "https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Futures/continuous.csv"
#   
#   #FX_list
#   fx_list_url_1 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=1&auth_token=GFHcsSjXc_LFxoW_xobL"
#   fx_list_url_2 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=2&auth_token=GFHcsSjXc_LFxoW_xobL"
#   fx_list_url_3 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=3&auth_token=GFHcsSjXc_LFxoW_xobL"
#   fx_list_url_4 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=4&auth_token=GFHcsSjXc_LFxoW_xobL"
#   fx_list_url_5 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=5&auth_token=GFHcsSjXc_LFxoW_xobL"
#   fx_list_url_6 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=6&auth_token=GFHcsSjXc_LFxoW_xobL"
#   fx_list_url_7 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=7&auth_token=GFHcsSjXc_LFxoW_xobL"
#   fx_list_url_8 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=8&auth_token=GFHcsSjXc_LFxoW_xobL"
#   fx_list_url_9 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=9&auth_token=GFHcsSjXc_LFxoW_xobL"
#   fx_list_1 <- data.table(read.csv(fx_list_url_1, header = F))
#   fx_list_2 <- data.table(read.csv(fx_list_url_2, header = F))
#   fx_list_3 <- data.table(read.csv(fx_list_url_3, header = F))
#   fx_list_4 <- data.table(read.csv(fx_list_url_4, header = F))
#   fx_list_5 <- data.table(read.csv(fx_list_url_5, header = F))
#   fx_list_6 <- data.table(read.csv(fx_list_url_6, header = F))
#   fx_list_7 <- data.table(read.csv(fx_list_url_7, header = F))
#   fx_list_8 <- data.table(read.csv(fx_list_url_8, header = F))
#   fx_list_9 <- data.table(read.csv(fx_list_url_9, header = F))
#   fx_list <- rbind(fx_list_1,fx_list_2,fx_list_3,fx_list_4,fx_list_5,fx_list_6,fx_list_7,fx_list_8,fx_list_9)
#   setnames(fx_list, c("V1", "V2", "V3", "V4", "V5", "V6"), c("quandl.code", "name", "start_date", "end_date", "frequency", "last_updated"))
#   fx_list <- fx_list[, c("quandl.code", "name"), with = F]
#   fx_list[] <- lapply(fx_list, as.character)
#   fx_list$name <- strsplit(fx_list$quandl.code, split = "/")
#   fx_list$name <- lapply(fx_list$name, tail, 1)
#   fx_list$name <- as.character(fx_list$name)
#   setorderv(fx_list, "name")
#   fx_list <<- unique(fx_list)
#   
#   equity_list <- data.table(read.csv(equity_list_url, header = T))
#   equity_list[] <- lapply(equity_list, as.character)
#   equity_list$ticker <- strsplit(equity_list$quandl.code, split = "/")
#   equity_list$ticker <- lapply(equity_list$ticker, tail, 1)
#   equity_list$ticker <- as.character(equity_list$ticker)
#   equity_list <<- equity_list
#   
#   futures_list <- data.table(read.csv(futures_list_url, header = T)) 
#   futures_list[] <- lapply(futures_list, as.character)
#   futures_list <- futures_list[Exchange %in% c("CME", "ICE", "EUREX")]
#   futures_list <<- futures_list
# }


quandl_equity_list <- function(){
  temp <- tempfile()
  equity_list_url <- "http://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/WIKI_tickers.csv"
  download.file(equity_list_url, temp)
  equity_list <- data.table(read.csv(temp, header = T))
  equity_list[] <- lapply(equity_list, as.character)
  equity_list$ticker <- strsplit(equity_list$quandl.code, split = "/")
  equity_list$ticker <- lapply(equity_list$ticker, tail, 1)
  equity_list$ticker <- as.character(equity_list$ticker)
  return(equity_list)
}

quandl_futures_list <- function(){
  temp <- tempfile()
  futures_list_url <- "http://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Futures/continuous.csv"
  download.file(futures_list_url, temp)
  futures_list <- data.table(read.csv(temp, header = T)) 
  futures_list[] <- lapply(futures_list, as.character)
  futures_list <- futures_list[Exchange %in% c("CME", "ICE", "EUREX")]
  return(futures_list)
}

quandl_fx_list <- function(){
  fx_list_url_1 <- "http://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=1&auth_token=GFHcsSjXc_LFxoW_xobL"
  fx_list_url_2 <- "http://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=2&auth_token=GFHcsSjXc_LFxoW_xobL"
  fx_list_url_3 <- "http://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=3&auth_token=GFHcsSjXc_LFxoW_xobL"
  fx_list_url_4 <- "http://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=4&auth_token=GFHcsSjXc_LFxoW_xobL"
  fx_list_url_5 <- "http://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=5&auth_token=GFHcsSjXc_LFxoW_xobL"
  fx_list_url_6 <- "http://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=6&auth_token=GFHcsSjXc_LFxoW_xobL"
  fx_list_url_7 <- "http://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=7&auth_token=GFHcsSjXc_LFxoW_xobL"
  fx_list_url_8 <- "http://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=8&auth_token=GFHcsSjXc_LFxoW_xobL"
  fx_list_url_9 <- "http://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=9&auth_token=GFHcsSjXc_LFxoW_xobL"
  fx_list_1 <- data.table(read.csv(fx_list_url_1, header = F))
  fx_list_2 <- data.table(read.csv(fx_list_url_2, header = F))
  fx_list_3 <- data.table(read.csv(fx_list_url_3, header = F))
  fx_list_4 <- data.table(read.csv(fx_list_url_4, header = F))
  fx_list_5 <- data.table(read.csv(fx_list_url_5, header = F))
  fx_list_6 <- data.table(read.csv(fx_list_url_6, header = F))
  fx_list_7 <- data.table(read.csv(fx_list_url_7, header = F))
  fx_list_8 <- data.table(read.csv(fx_list_url_8, header = F))
  fx_list_9 <- data.table(read.csv(fx_list_url_9, header = F))
  fx_list <- rbind(fx_list_1,fx_list_2,fx_list_3,fx_list_4,fx_list_5,fx_list_6,fx_list_7,fx_list_8,fx_list_9)
  setnames(fx_list, c("V1", "V2", "V3", "V4", "V5", "V6"), c("quandl.code", "name", "start_date", "end_date", "frequency", "last_updated"))
  fx_list <- fx_list[, c("quandl.code", "name"), with = F]
  fx_list[] <- lapply(fx_list, as.character)
  fx_list$name <- strsplit(fx_list$quandl.code, split = "/")
  fx_list$name <- lapply(fx_list$name, tail, 1)
  fx_list$name <- as.character(fx_list$name)
  setorderv(fx_list, "name")
  return(unique(fx_list))
  
}

# quandl_get_list <- function(){
#   equity_list_url <- "https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/WIKI_tickers.csv"
#   futures_list_url <- "https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Futures/continuous.csv"
#   
#   #FX_list
# #   fx_list_url_1 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=1&auth_token=GFHcsSjXc_LFxoW_xobL"
# #   fx_list_url_2 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=2&auth_token=GFHcsSjXc_LFxoW_xobL"
# #   fx_list_url_3 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=3&auth_token=GFHcsSjXc_LFxoW_xobL"
# #   fx_list_url_4 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=4&auth_token=GFHcsSjXc_LFxoW_xobL"
# #   fx_list_url_5 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=5&auth_token=GFHcsSjXc_LFxoW_xobL"
# #   fx_list_url_6 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=6&auth_token=GFHcsSjXc_LFxoW_xobL"
# #   fx_list_url_7 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=7&auth_token=GFHcsSjXc_LFxoW_xobL"
# #   fx_list_url_8 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=8&auth_token=GFHcsSjXc_LFxoW_xobL"
# #   fx_list_url_9 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=9&auth_token=GFHcsSjXc_LFxoW_xobL"
# #   fx_list_1 <- data.table(read.csv(fx_list_url_1, header = F))
# #   fx_list_2 <- data.table(read.csv(fx_list_url_2, header = F))
# #   fx_list_3 <- data.table(read.csv(fx_list_url_3, header = F))
# #   fx_list_4 <- data.table(read.csv(fx_list_url_4, header = F))
# #   fx_list_5 <- data.table(read.csv(fx_list_url_5, header = F))
# #   fx_list_6 <- data.table(read.csv(fx_list_url_6, header = F))
# #   fx_list_7 <- data.table(read.csv(fx_list_url_7, header = F))
# #   fx_list_8 <- data.table(read.csv(fx_list_url_8, header = F))
# #   fx_list_9 <- data.table(read.csv(fx_list_url_9, header = F))
# #   fx_list <- rbind(fx_list_1,fx_list_2,fx_list_3,fx_list_4,fx_list_5,fx_list_6,fx_list_7,fx_list_8,fx_list_9)
#   fx_list_url_1 <- "https://www.quandl.com/api/v2/datasets.csv?query=*&source_code=CURRFX&per_page=300&page=1&auth_token=GFHcsSjXc_LFxoW_xobL"
#   fx_list <- data.table(read.csv(fx_list_url_1, header = F))
#   setnames(fx_list, c("V1", "V2", "V3", "V4", "V5", "V6"), c("quandl.code", "name", "start_date", "end_date", "frequency", "last_updated"))
#   fx_list <- fx_list[, c("quandl.code", "name"), with = F]
#   fx_list[] <- lapply(fx_list, as.character)
#   fx_list$name <- strsplit(fx_list$quandl.code, split = "/")
#   fx_list$name <- lapply(fx_list$name, tail, 1)
#   fx_list$name <- as.character(fx_list$name)
#   setorderv(fx_list, "name")
#   fx_list <<- unique(fx_list)
#   
#   equity_list <- data.table(read.csv(equity_list_url, header = T))
#   equity_list[] <- lapply(equity_list, as.character)
#   equity_list$ticker <- strsplit(equity_list$quandl.code, split = "/")
#   equity_list$ticker <- lapply(equity_list$ticker, tail, 1)
#   equity_list$ticker <- as.character(equity_list$ticker)
#   equity_list <<- equity_list
#   
#   futures_list <- data.table(read.csv(futures_list_url, header = T)) 
#   futures_list[] <- lapply(futures_list, as.character)
#   futures_list <- futures_list[Exchange %in% c("CME", "ICE", "EUREX")]
#   futures_list <<- futures_list
# }

quandl_raw_search_results <- function(keyword){
  result <- try(data.table(do.call("rbind",Quandl.search(query = keyword, silent = T))), silent = T)
  return(result)
}

quandl_raw_download_results <- function(id){
  result <- Quandl(id, authcode = quandl_key)
  return(result)
}