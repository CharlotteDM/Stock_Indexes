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
library(quantmod) 
library(tseries)
library(timetk)
#install.packages("rlang")
library(rlang)
#install.packages("highcharter")
library(highcharter)
#install.packages("ggfortify")
library(ggfortify)
#install.packages("coefplot")
library(coefplot)
library(boot)
library(GGally)

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


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

#loading data: GDP_Euro_quart
#source of data: https://ec.europa.eu/eurostat/databrowser/view/NAIDQ_10_GDP/default/table?lang=en
GDP_Euro_quart <- read.csv("GDP_EURO_2020_2022.csv", stringsAsFactors = F)



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

# ---joins df's
df_list <- list(DJ, FTSE, FTSE100, NASDAQ, NIKKEI, SP500, SX5Euro, VIX)
all_indexes <- df_list %>% reduce(full_join, by="Date")

## ----plot for all indexes
# ---defines colors
colors_indexes <- c("VIX_Close" = "blue", "DJ_Close" = "violet", "FTSE_Close" = "pink", "FTSE100_Close" = "coral",
            "NASDAQ_Close" = "greenyellow", "NIKKEI_Close" = "firebrick1", "SP500_Close" = "darkorange",
            "SX5Euro_Close" = "deepskyblue")
# ---makes plot
plot_all <- 
  ggplot(all_indexes, aes(x = Date)) +
  geom_line(aes(y = VIX_Close, color = "VIX_Close")) +
  geom_line(aes(y = DJ_Close, color = "DJ_Close")) +
  geom_line(aes(y = FTSE_Close,color = "FTSE_Close")) +
  geom_line(aes(y = FTSE100_Close, color = "FTSE100_Close")) +
  geom_line(aes(y = NASDAQ_Close, color = "NASDAQ_Close")) +
  geom_line(aes(y = NIKKEI_Close, color = "NIKKEI_Close")) +
  geom_line(aes(y = SP500_Close, color = "SP500_Close")) +
  geom_line(aes(y = SX5Euro_Close, color = "SX5Euro_Close")) +
  labs(
    title = "Stock Indexes",
    subtitle = "02.2020-06.2022",
    caption = "(based on data from: https://www.wsj.com/market-data/)",
    x = "Date",
    y = "Stock Indexes",
    color = "Legend") +
  scale_color_manual(values = colors_indexes) +
  theme(
    plot.title = element_text(color="royalblue4", size=14, face="bold"),
    axis.title.x = element_text(color="steelblue2", size=14, face="bold"),
    axis.title.y = element_text(color="steelblue2", size=14, face="bold")) 


## ----interactive chart for all indexes
all_ind_plotly <- ggplotly(plot_all)

### -------- descriptive statistics - function

descr_stat = function(all_indexes) {
  
  #get indexes to use in desc statistics
  names_list = colnames(all_indexes)
 
  
  for (col_name in names_list) {
    summary_val <- summary(all_indexes[[col_name]])
    var_val <- var(all_indexes[[col_name]], na.rm = T)
    sd_val<- sd(all_indexes[[col_name]], na.rm = T)
    skew_val <- skewness(all_indexes[[col_name]], na.rm = T)
    kurt_val <- kurtosis(all_indexes[[col_name]], na.rm = T)
    cat("\n","Summary: ", col_name,"\n")
    print(summary_val)
    cat("\n","var: ", var_val,"\n","sd: ", sd_val,"\n","skewness: ", skew_val,"\n","kurtosis: ", kurt_val,"\n")
  }
}

descr_stat(all_indexes)




## ----interactive plot for SX5 Euro

sx5e <- all_indexes %>%
  hchart(type = "line", color = "red", 
         hcaes(x = Date, y = SX5Euro_Close)) %>%
  hc_credits(enabled = TRUE, 
             text = "Sources: https://www.wsj.com/market-data",
             style = list(fontSize = "10px")) %>%
  hc_title (text = "The EURO STOXX 50 Index") %>%
  hc_tooltip(valueDecimals = 4,
             pointFormat = "Value of EURO STOXX 50 Index: {point.y}") 


### ------- time series for one of the stock index
#time series for SX5Euro
tsSX5Euro <- ts(SX5Euro$SX5Euro_Close, start=c(2020, 2), end=c(2022, 6), freq=12)
class(SX5Euro$SX5Euro_Close)

plotSX5Euro <- plot_time_series(SX5Euro, SX5Euro$Date, SX5Euro_Close, 
                                .title = "Time Series Plot for SX5Euro",
                                .x_lab = "Date", .y_lab = "Value")


#decomposition of additive time series for SX5Euro
dcmp <- decompose(as.ts(tsSX5Euro))
plot(dcmp)


#Augmented Dickey-Fuller Test for stationarity
adf.test(tsSX5Euro) #is not stationary

#autocovariance function
acf(tsSX5Euro)

#partial autocovariance function
pacf(tsSX5Euro)

#determining the optimal number of differentiations 
ndiffs(tsSX5Euro,test="kpss",alpha=0.05)

#using the auto.arima function
best_tsSX5Euro <- auto.arima(x = tsSX5Euro)
best_tsSX5Euro

acf(best_tsSX5Euro$residuals)
pacf(best_tsSX5Euro$residuals)

#coefficients ARIMA model for components AR and MA
coef(best_tsSX5Euro)

#### -------- builds a model
fit_tsSX5Euro <- Arima(tsSX5Euro, order = c(1,0,0),
             include.drift = TRUE)
summary(fit_tsSX5Euro)


#plots for residuals for model ARIMA (3,1,3)
plot_resid <- ggtsdiag(fit_tsSX5Euro) +
  theme(panel.background = element_rect(fill = "lavender"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray28"),
        axis.line.x = element_line(colour="gray28"))

residFit_SX5Euro <- ggplot(data=fit_tsSX5Euro, aes(residuals(fit_tsSX5Euro))) +
  geom_histogram(aes(y =..density..),  
                 binwidth = 6,
                 col="midnightblue", fill="skyblue") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "lavender"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray28"),
        axis.line.x = element_line(colour="gray28")) +
  ggtitle("EURO STOCK 50 - ARIMA Model Residuals")



#### -------prediction
#prediction based on the ARIMA model forecasting for 24 months with the standard error
predict(best_tsSX5Euro, n.ahead = 24, se.fit = T)
theForecastSX5Euro <- forecast(object = best_tsSX5Euro, h = 24)
plot(theForecastSX5Euro) 

#prediction based on the naive method for SX5 Euro
naive_mod <- naive(tsSX5Euro)
summary(naive_mod)

#forecast -  naive method 
n <- 100 #number of period for forecasting
train <- head((SX5Euro$SX5Euro_Close), length(SX5Euro$SX5Euro_Close)-n) #splits data
test <- tail((SX5Euro$SX5Euro_Close), n) #splits data
frc2_SX5Euro_nm <- naive(train, h=10)
naive_plot <- autoplot(frc2_SX5Euro_nm) +
  autolayer(ts(test, start=length(train)), series = "Test Data")

#forecast - Holt's Trend Method
holt_mod <- holt(tsSX5Euro, h = 24)
summary(holt_mod)



#### ------multiple regression model for EURO STOXX 50 Index 
model1 <- lm(SX5Euro_Close ~ DJ_Close + FTSE_Close + FTSE100_Close + NASDAQ_Close + 
              NIKKEI_Close + SP500_Close + VIX_Close, data = all_indexes)
summary(model1)
plot(model1)


allmodels <- ols_step_all_possible(model1) #residuals vs fitted
ols_step_both_aic(model1) #stepwise summary


ols_test_normality(model1) #test K-S: p > 0,05 that is, there is no reason to reject the null hypothesis and the distribution is normal
ols_plot_resid_hist(model1) #histogram
ols_plot_resid_qq(model1) #a quantile-quantile plot shows the residual distribution and outliers
cook <- ols_plot_cooksd_bar(model1) #establishing outliers 


#### ----- comparison of models
#with function multiplot
model2 <-  lm(SX5Euro_Close ~ DJ_Close +  FTSE100_Close + NASDAQ_Close + 
            SP500_Close + VIX_Close, data = all_indexes)
model3 <-  lm(SX5Euro_Close ~ VIX_Close + NASDAQ_Close + SP500_Close, data = all_indexes)
model4 <-  lm(SX5Euro_Close ~ VIX_Close + FTSE100_Close, data = all_indexes)
model5 <- lm(SX5Euro_Close ~ NIKKEI_Close + FTSE_Close, data = all_indexes)
model6 <- lm(SX5Euro_Close ~ DJ_Close, data = all_indexes)

multiplot_models <- multiplot(model1, model2, model3, model4, model5, model6, 
                       pointSize = 2) #the graph of the coefficients for the various models 

#ANOVA
all_indexes_nona <- na.omit(all_indexes) #we shoul delete all NA observations
model1nona <- lm(SX5Euro_Close ~ DJ_Close + FTSE_Close + FTSE100_Close + NASDAQ_Close + 
               NIKKEI_Close + SP500_Close + VIX_Close, data = all_indexes_nona)
model2nona <-  lm(SX5Euro_Close ~ DJ_Close +  FTSE100_Close + NASDAQ_Close + 
                SP500_Close + VIX_Close, data = all_indexes_nona)
model3nona <-  lm(SX5Euro_Close ~ VIX_Close + NASDAQ_Close + SP500_Close, data = all_indexes_nona)
model4nona <-  lm(SX5Euro_Close ~ VIX_Close + FTSE100_Close, data = all_indexes_nona)
model5nona <- lm(SX5Euro_Close ~ NIKKEI_Close + FTSE_Close, data = all_indexes_nona)
model6nona <- lm(SX5Euro_Close ~ DJ_Close, data = all_indexes_nona)
anova(model1nona, model2nona, model3nona, model4nona, model5nona, model6nona) #first model is the best

#Bayesian Information Criterion
BIC(model1, model2, model3, model4, model5, model6) #the best is first model
#Akaike Information Criterion
AIC(model1nona, model2nona, model3nona, model4nona, model5nona, model6nona) #the best is first model

#Cross validation with generalized linear models
modelG1nona<- glm(SX5Euro_Close ~ DJ_Close + FTSE_Close + FTSE100_Close + NASDAQ_Close + 
                   NIKKEI_Close + SP500_Close + VIX_Close, data = all_indexes_nona)
modelG2nona <-  glm(SX5Euro_Close ~ DJ_Close +  FTSE100_Close + NASDAQ_Close + 
                    SP500_Close + VIX_Close, data = all_indexes_nona)
modelG3nona <-  glm(SX5Euro_Close ~ VIX_Close + NASDAQ_Close + SP500_Close, data = all_indexes_nona)
modelG4nona <-  glm(SX5Euro_Close ~ VIX_Close + FTSE100_Close, data = all_indexes_nona)
modelG5nona <- glm(SX5Euro_Close ~ NIKKEI_Close + FTSE_Close, data = all_indexes_nona)
modelG6nona <- glm(SX5Euro_Close ~ DJ_Close, data = all_indexes_nona)

identical(coef(model1nona), coef(modelG1nona))

modelCV1 <- cv.glm(all_indexes_nona, modelG1nona, K = 5)
modelCV2 <- cv.glm(all_indexes_nona, modelG2nona, K = 5)
modelCV3 <- cv.glm(all_indexes_nona, modelG3nona, K = 5)
modelCV4 <- cv.glm(all_indexes_nona, modelG4nona, K = 5)
modelCV5 <- cv.glm(all_indexes_nona, modelG5nona, K = 5)
modelCV6 <- cv.glm(all_indexes_nona, modelG6nona, K = 5)


models_results <- as.data.frame(rbind(modelCV1$delta, modelCV2$delta, modelCV3$delta,
                                      modelCV4$delta, modelCV5$delta, modelCV6$delta))
names(models_results) <- c("Error", "Adjusted Error")
models_results$model_name <- sprintf("modelG%s", 1:6)
#first model is the best


#### ------- makes quarterly data
all <- arrange(all_indexes, Date)
all$qdate <- as.yearqtr(all$Date)
all_qtrly <- all %>%
  group_by(qdate) %>%
  summarise_all(mean, na.rm = T)

#### ------- takes only closing value
all_close <- all_qtrly %>%
  select("qdate", "DJ_Close", "FTSE_Close", "FTSE100_Close", "NASDAQ_Close", "NIKKEI_Close", 
         "SP500_Close", "SX5Euro_Close", "VIX_Close")

#### ------- joins data frame
TIME_PERIOD <- c("2020-Q1", "2020-Q2", "2020-Q3", "2020-Q4", "2021-Q1", "2021-Q2", "2021-Q3", "2021-Q4", "2022-Q1", "NA") 
all_close$TIME_PERIOD <- TIME_PERIOD 
all_close_gdp <- left_join(all_close, GDP_Euro_quart, by = c("TIME_PERIOD"="TIME_PERIOD"))

#### -----correlation
cor(all_close_gdp$SX5Euro_Close, all_close_gdp$OBS_VALUE, use = "complete.obs")


#### ----linear regression model for EURO STOXX 50 Index and another variables
regr <- lm(SX5Euro_Close ~ OBS_VALUE, data = all_close_gdp)
summary(regr)


