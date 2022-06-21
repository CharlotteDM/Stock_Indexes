library(httr)
library("rstudioapi")
library(ggplot2)
library(plotly)
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
library(quantmod) ?
library(tseries)
library(timetk)
#install.packages("timetk")

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


url <- "https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX9D_History.csv"

#access to API - VIX
#response_VIX <- GET(url)
#base_uri_VIX <- "https://cdn.cboe.com"
#endpoint_VIX <- "/api/global/us_indices/daily_prices/VIX9D_History.csv"
#resource_uri_VIX <- paste0(base_uri_VIX, endpoint_VIX)
#response_data_VIX <- GET(resource_uri_VIX, query = query_params)


#loading data: VIX
#source of data: https://www.wsj.com/market-data/quotes/index/VIX/historical-prices
VIX <- read.csv("VIX.csv", stringsAsFactors = F)

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
#data source: https://www.wsj.com/market-data/quotes/index/NASDAQ/historical-prices
NASDAQ <-read.csv("NASDAQ.csv", stringsAsFactors = F)

#loading data: NIKKEI 225
#data source: https://www.wsj.com/market-data/quotes/index/JP/NIK/historical-prices
NIKKEI <- read.csv("NIKKEI225.csv", stringsAsFactors = F)

#loading data: FTSE 100
#data source: https://www.wsj.com/market-data/quotes/index/UK/UKX/historical-prices
FTSE100 <-read.csv("FTSE100.csv", stringsAsFactors = F)


#converts character value as a date
VIX$Date <- mdy(VIX$Date) 
DJ$Date <- mdy(DJ$Date) 
FTSE$Date <- mdy(FTSE$Date) 
FTSE100$Date <- mdy(FTSE100$Date) 
NASDAQ$Date <- mdy(NASDAQ$Date) 
NIKKEI$Date <- mdy(NIKKEI$Date) 
SP500$Date <- mdy(SP500$Date) 
SX5Euro$Date <- mdy(SX5Euro$Date) 

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


#analysis



#?????chart_Series(SX5Euro$Close, name = "EURO STOXX 50 Index 02.2020-06.2022")

### ------- plot for all od indexes
#new data frame

#1 attempt
all_indexes <- left_join(VIX, DJ, by='Date') %>%
  left_join(., FTSE, by='Date') %>%
  left_join(., FTSE100, by='Date') %>%
  left_join(., NASDAQ, by='Date') %>%
  left_join(., NIKKEI, by='Date') %>%
  left_join(., SP500, by='Date') %>%
  left_join(., SX5Euro, by='Date')

#2attempt
max_length <- max(c(length(VIX$Close),length(VIX$Date),length(DJ$Close), length(FTSE$Close),
                    length(FTSE100$Close), length(NASDAQ$Close), length(NIKKEI$Close),
                    length(SP500$Close), length(SX5Euro$Close)))    # finding out maximum length because we have unequal lenghts 

length(NIKKEI$Close) <- length(FTSE$Close)





#changing columns from df's into separate df
VIX_Close <- as.data.frame(VIX$Close)
VIX_Date <- as.data.frame(VIX$Date)
DJ_Close <- as.data.frame(DJ$Close)
FTSE_Close <- as.data.frame(FTSE$Close)
FTSE100_Close <- as.data.frame(FTSE100$Close)
NASDAQ_Close <- as.data.frame(NASDAQ$Close)
NIKKEI_Close <- as.data.frame(NIKKEI$Close)
SP500_Close <- as.data.frame(SP500$Close)
SX5Euro_Close <- as.data.frame(SX5Euro$Close)


all_indexes <- smartbind(VIX_Close, 
                              VIX_Date, 
                              DJ_Close,
                              FTSE_Close,
                              FTSE100_Close,
                              NASDAQ_Close,
                              NIKKEI_Close, 
                              SP500_Close,
                              SX5Euro_Close, fill = NA)
?smartbind




all_indx <- data.frame(matrix(unlist(all_indexes), nrow=length(all_indexes), byrow=TRUE))


allindexes <- data.frame(VIX = c(1, 5, NA), DJ = c(5, NA),
                         FTSE = c(5, NA),
                         FTSE100 = c (5, NA),
                         NASDAQ = c(5, NA),
                         NIKKEI = c(5, NA), 
                         SP500 = c(5, NA),
                         SX5Euro = c(5, NA), all = T)


#3attempt
all_indexes <- data.frame(stock = "VIX", Date = as.Date(row.names(as.data.frame(VIX))), value = VIX$Close)
DFJPM <- data.frame(stock = "JPM", Date = as.Date(row.names(as.data.frame(JPM))), value = JPM$JPM.Close)
plotting <- rbind(DFSQ, DFJPM)


plot_all <- 
  ggplot(allindexes, aes(x=Date))+
  geom_line(aes(y=VIX),color="blue")+
  geom_line(aes(y=DJ),color="violet")+
  geom_line(aes(y=FTSE),color="pink")+
  geom_line(aes(y=FTSE100),color="coral") +
  geom_line(aes(y=NASDAQ),color="greenyellow")+
  geom_line(aes(y=NIKKEI),color="firebrick1")+
  geom_line(aes(y=SP500),color="darkorange")+
  geom_line(aes(y=SX5Euro),color="green")

#time series for SX5Euro
tsSX5Euro <- ts(SX5Euro$Close, start=c(2022, 1), freq=12)
plotTSX5Euro <- plot_time_series(tsSX5Euro, 'EURO STOXX 50 Index')


#autocovariance function
acf(tsSX5Euro)

#partial autocovariance function
pacf(tsSX5Euro)

#determining the optimal number of differentiations 
plot(diff(tsSX5Euro))
diff(tsSX5Euro)
ndiffs(tsSX5Euro,test="kpss",alpha=0.05)

#using the auto.arima function
best_tsSX5Euro <- auto.arima(x = tsSX5Euro)
best_tsSX5Euro

acf(best_tsSX5Euro$residuals)
pacf(best_tsSX5Euro$residuals)

#coefficients ARIMA model for components AR and MA
coef(best_tsSX5Euro)

#prediction based on the ARIMA model forecasting for 100 days with the standard error
predict(best_tsSX5Euro, n.ahead = 100, se.fit = T)
theForecastSX5Euro <- forecast(object = best_tsSX5Euro, h = 100)
plot(theForecast) 