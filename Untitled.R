library(httr)
library("rstudioapi")
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(scales)
library(useful)
library(dygraphs)
library(forecast)
library(olsrr)
library(zoo)
library(moments)


path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


url <- "https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX9D_History.csv"

#access to API - VIX
response_VIX <- GET(url)
base_uri_VIX <- "https://cdn.cboe.com"
endpoint_VIX <- "/api/global/us_indices/daily_prices/VIX9D_History.csv"
resource_uri_VIX <- paste0(base_uri_VIX, endpoint_VIX)
response_data_VIX <- GET(resource_uri_VIX, query = query_params)


#loading data: SP500 
#source of data: https://www.wsj.com/market-data/quotes/index/SPX/historical-prices
SP500 <- read.csv("S&P.csv", stringsAsFactors = F)

#loading data: SX5Euro
#source of data: https://www.wsj.com/market-data/quotes/index/XX/SX5E/historical-prices
SX5Euro <- read.csv("SX5Euro.csv", stringsAsFactors = F)

#loading data: FTSE China A50
#data source: https://www.wsj.com/market-data/quotes/index/XX/XIN9/historical-prices
FTSE <- read.csv("FTSEChina.csv", stringsAsFactors = F)

#loading data: Dow Jones
#data source: https://www.wsj.com/market-data/quotes/index/DJIA/historical-prices
DJ <- read.csv("DowJones.csv", stringsAsFactors = F)

#loading data: NASDAQ
#data source: https://www.nasdaq.com/market-activity/index/spx/historical
NASDAQ <-read.csv("NASDAQ.csv", stringsAsFactors = F)

#loading data: NIKKEI 225
#data source: https://www.wsj.com/market-data/quotes/index/JP/NIK/historical-prices
NIKKEI <- read.csv("NIKKEI225.csv", stringsAsFactors = F)

#loading data: FTSE 100
#data source: https://www.wsj.com/market-data/quotes/index/UK/UKX/historical-prices
FTSE100 <-read.csv("FTSE100.csv", stringsAsFactors = F)


#converts character value as a date


date_convert = function(date_input) {

  if(is.Date(as.Date(date_input, "%Y-%m-%d")))
  {
    return(as.Date(date, "%Y-%m-%d"))
  }
  else if (is.Date(as.Date(date_input,"%m/%d/%y"))) 
  { 
    return(mdy(date))
  }
}

SP500$Date <- date_convert(SP500$Date)

as.Date(SP500$Date,"%m/%d/%y")


ymd(NIKKEI$Data)