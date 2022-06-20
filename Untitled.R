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

#access to API NASDAQ
#url <- "https://www.nasdaq.com/market-activity/index/spx/historical"
#response_NASDAQ <- GET(url)
#base_uri_NASDAQ <- "https://www.nasdaq.com"
#endpoint_NASDAQ <- "market-activity/index/spx/historical"
#resource_uri_NASDAQ <- paste0(base_uri_NASDAQ, endpoint_NASDAQ)
#response_data_NASDAQ <- GET(resource_uri_NASDAQ, query = query_params)

#loading data: SP500 
#source of data: https://www.wsj.com/market-data/quotes/index/SPX/historical-prices
SP500 <- read.csv("S&P.csv", stringsAsFactors = F)
