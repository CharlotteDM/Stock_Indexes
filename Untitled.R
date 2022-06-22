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
## ----new data frame
# ---changes name of columns in df's
colnames(VIX)[1] <- "Date"
colnames(VIX)[2] <- "VIX_Open"
colnames(VIX)[3] <- "VIX_High"
colnames(VIX)[4] <- "VIX_Low"
colnames(VIX)[5] <- "VIX_Close"
colnames(DJ)[1] <- "Date"
colnames(DJ)[2] <- "DJ_Open"
colnames(DJ)[3] <- "DJ_High"
colnames(DJ)[4] <- "DJ_Low"
colnames(DJ)[5] <- "DJ_Close"
colnames(FTSE)[1] <- "Date"
colnames(FTSE)[2] <- "FTSE_Open"
colnames(FTSE)[3] <- "FTSE_High"
colnames(FTSE)[4] <- "FTSE_Low"
colnames(FTSE)[5] <- "FTSE_Close"
colnames(FTSE100)[1] <- "Date"
colnames(FTSE100)[2] <- "FTSE100_Open"
colnames(FTSE100)[3] <- "FTSE100_High"
colnames(FTSE100)[4] <- "FTSE100_Low"
colnames(FTSE100)[5] <- "FTSE100_Close"
colnames(NASDAQ)[1] <- "Date"
colnames(NASDAQ)[2] <- "NASDAQ_Open"
colnames(NASDAQ)[3] <- "NASDAQ_High"
colnames(NASDAQ)[4] <- "NASDAQ_Low"
colnames(NASDAQ)[5] <- "NASDAQ_Close"
colnames(NIKKEI)[1] <- "Date"
colnames(NIKKEI)[2] <- "NIKKEI_Open"
colnames(NIKKEI)[3] <- "NIKKEI_High"
colnames(NIKKEI)[4] <- "NIKKEI_Low"
colnames(NIKKEI)[5] <- "NIKKEI_Close"
colnames(SP500)[1] <- "Date"
colnames(SP500)[2] <- "SP500_Open"
colnames(SP500)[3] <- "SP500_High"
colnames(SP500)[4] <- "SP500_Low"
colnames(SP500)[5] <- "SP500_Close"
colnames(SX5Euro)[1] <- "Date"
colnames(SX5Euro)[2] <- "SX5Euro_Open"
colnames(SX5Euro)[3] <- "SX5Euro_High"
colnames(SX5Euro)[4] <- "SX55uro_Low"
colnames(SX5Euro)[5] <- "SX5Euro_Close"

# ---joining df's
df_list <- list(DJ, FTSE, FTSE100, NASDAQ, NIKKEI, SP500, SX5Euro, VIX)
all_indexes <- df_list %>% reduce(full_join, by="Date")

## ----plot for all indexes

plot_all <- 
  ggplot(all_indexes, aes(x=Date))+
  geom_line(aes(y=VIX_Close),color="blue")+
  geom_line(aes(y=DJ_Close),color="violet")+
  geom_line(aes(y=FTSE_Close),color="pink")+
  geom_line(aes(y=FTSE100_Close),color="coral") +
  geom_line(aes(y=NASDAQ_Close),color="greenyellow")+
  geom_line(aes(y=NIKKEI_Close),color="firebrick1")+
  geom_line(aes(y=SP500_Close),color="darkorange")+
  geom_line(aes(y=SX5Euro_Close),color="green")

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