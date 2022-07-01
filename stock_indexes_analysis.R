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

### ------- time series for one of the stock index
#time series for SX5Euro
tsSX5Euro <- ts(SX5Euro$SX5Euro_Close, start=c(2022, 1), freq=12)
class(SX5Euro$SX5Euro_Close)

plotSX5Euro <- plot_time_series(SX5Euro, SX5Euro$Date, SX5Euro_Close, 
                                .title = "Time Series Plot for SX5Euro",
                                .x_lab = "Date", .y_lab = "Value")



#Augmented Dickey-Fuller Test for stationarity
adf.test(tsSX5Euro) #is not stationary

#autocovariance function
acf(tsSX5Euro)

#partial autocovariance function
pacf(tsSX5Euro)

#determining the optimal number of differentiations 
ndiffs(tsSX5Euro,test="kpss",alpha=0.05)
diff(tsSX5Euro, differences = 1)
plot(diff(tsSX5Euro, 1))

#using the auto.arima function
best_tsSX5Euro <- auto.arima(x = tsSX5Euro)
best_tsSX5Euro

acf(best_tsSX5Euro$residuals)
pacf(best_tsSX5Euro$residuals)

#coefficients ARIMA model for components AR and MA
coef(best_tsSX5Euro)

#### -------- builds a model
fit_tsSX5Euro <- Arima(tsSX5Euro, order = c(3,1,3),
             include.drift = TRUE)
summary(fit_tsSX5Euro)


#plots for residuals for model ARIMA (3,1,3)
ggtsdiag(fit_tsSX5Euro) +
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
  ggtitle("SX5 Euro Index - ARIMA Model Residuals")



#### -------prediction
#prediction based on the ARIMA model forecasting for 100 days with the standard error
predict(best_tsSX5Euro, n.ahead = 100, se.fit = T)
theForecastSX5Euro <- forecast(object = best_tsSX5Euro, h = 100)
plot(theForecastSX5Euro) 

#decomposition of additive time series for SX5Euro
dcmp <- decompose(as.ts(tsSX5Euro))
plot(dcmp)

#prediction based on the naive method for SX5 Euro
frc_SX5Euro_nm <- c(NA, SX5Euro$SX5Euro_Close[-length(SX5Euro$SX5Euro_Close)])
mean(abs((SX5Euro$SX5Euro_Close-frc_SX5Euro_nm)/SX5Euro$SX5Euro_Close), na.rm=T) * 100 #MAPE
mean(abs(SX5Euro$SX5Euro_Close-frc_SX5Euro_nm), na.rm=T) #absolute mean error

#plot based on naive method for SX5 Euro
#naive_plot <- plot(SX5Euro$SX5Euro_Close, type='l', col = 'red', main='Actual vs. Forecasted',
    # xlab='Period of time', ylab='SX5 Euro value') + lines(frc_SX5Euro_nm, type='l', col = 'green') + legend('topright', legend=c('Actual', 'Forecasted'),
    #   col=c('red', 'green'), lty=1)

#forecast from naive method 
n <- 100 #number of period for forecasting
train <- head((SX5Euro$SX5Euro_Close), length(SX5Euro$SX5Euro_Close)-n) #splits data
test <- tail((SX5Euro$SX5Euro_Close), n) #splits data
frc2_SX5Euro_nm <- naive(train, h=10)
autoplot(frc2_SX5Euro_nm) +
  autolayer(ts(test, start=length(train)), series = "Test Data")



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


#2attempt
highchart(type="stock") %>% 
  hc_add_series(all_indexes$SX5Euro_Close, type = "line",
                color = "red") %>% 
  hc_title(text="<b>SX5 Euro</b>") #work but we have problems with date


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


multiplot <- multiplot(model1, model2, model3, model4, model5, model6, 
                       pointSize = 2) #the graph of the coefficients for the various models showed that none of the analyzed variables had a significant impact on the SX5 Euro

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
#### ----linear regression model for EURO STOXX 50 Index 
