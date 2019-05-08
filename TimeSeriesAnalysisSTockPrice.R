#Load libraries 

library("ggplot2")
library("dplyr")
library("tidyr")
library("data.table")
library('corrplot')
library('gridExtra')
library('forecast')
library('tseries')
library('TSA')
library('tibble')
library('TTR')


#Read Data

s_data <-read.csv('all_stocks_5yr.csv')
summary(s_data)

#string data

str(s_data)

#Data Cleaning

s_data[is.na(s_data)]<-0
s_data$Date <-as.Date(s_data$Date,format="%y-%m-%d")
summary(s_data)

#Univariate distributions

options(repr.plot.width=12, repr.plot.height=12)

p1 = ggplot(s_data, aes(Open)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p2 = ggplot(s_data, aes(High)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p3 = ggplot(s_data, aes(Low)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p4 = ggplot(s_data, aes(Close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)


#time series analysis

tmp <- filter(s_data, High > 100) 
sample(tmp$Name, 10)

#individual stock
i_stock <-filter(s_data, Name == "PSA")
str(i_stock)

#create time series
##create daily Date object
#tsclean() is a convenient method for outlier removal and inputing missing values
#ts() is used to create time-series objects
## Create a daily Date object
inds <- seq(as.Date("2012-08-13"), as.Date("2017-08-11"), by = "day")

create_ts <- function(col_idx){
  ## Create a time series object
  i_ts <- as.numeric(i_stock[,col_idx]) %>%
    tsclean(replace.missing = TRUE, lambda = NULL) %>%
    ts(start = c(2012, as.numeric(format(inds[1], "%j"))),
       frequency = 365.5)
  return(i_ts)
}

i_ts = create_ts(which(colnames(i_stock) == "High"))
plot.ts(i_ts, xlab = "Time", ylab = "High value", main = "Time Series", col = "red")


#Stationarity
# we will use “Dickey-Fuller test” to determine stationarity.

adf.test(i_stock[,which(colnames(i_stock) == "High")], alternative = "stationary", k = 0)

## 
##  Augmented Dickey-Fuller Test
## 
## data:  i_stock[, which(colnames(i_stock) == "High")]
## Dickey-Fuller = -1.0257, Lag order = 0, p-value = 0.9346
## alternative hypothesis: stationary


#decomposing timer series
i_tscomponents <- decompose(i_ts)
plot(i_tscomponents,col = "red")


#Differecing time series

i_tsdiff1 <- diff(i_ts,differences = 1)
plot.ts(i_tsdiff1,col="red")

#select ARIMA Model
#autocorrelation correlogram
acf(i_tsdiff1,lag.max = 60)
#autocorrelation values
acf(i_tsdiff1,lag.max = 60,plot = FALSE)


# plot a partial correlogram
pacf(i_tsdiff1,lag.max = 60)
#partial autocorrelation values
pacf(i_tsdiff1,lag.max = 60,plot = FALSE)


#Fitting ARIMA model
i_tsarima <- auto.arima(i_ts,max.p = 3,max.q = 3,max.d = 3)
i_tsarima

#forecast using ARIMA model

i_tsforecast <- forecast(i_tsarima,h = 60)
plot(i_tsforecast, col="red")

#make time plot of forecast errors
plot.ts(i_tsforecast$residuals)

#make a histogram
ggplot(data.frame(residuals=i_tsforecast$residuals),aes(residuals))+geom_histogram(bins = 50,aes(y = ..density..), col="red",fill="red",alpha=0.3)+geom_density()


























