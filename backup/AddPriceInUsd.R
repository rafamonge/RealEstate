library(dplyr)
library(quantmod)

AddPriceInUSD <- function(df){
  from <- c("CRC", "USD")
  symbol=c("¢", "$")
  symbol <- enc2utf8 (symbol)
  to <- c("USD", "USD")
  
  currencyConvertion <- getQuote(paste0(from, to, "=X"))  
  
  getExchangeRate <- function(currency){
     currencyConvertion[match(currency, symbol),2]
  }
  
  getConvertedValue <- function(price, currency){
      price * getExchangeRate(currency)
  }
  
  df %>%  mutate(PriceInUSD = getConvertedValue(Price, Currency))
    
}


