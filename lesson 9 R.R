library(readr)
library(dplyr)
library(lubridate)
library(tseries)
#install.packages('forecast')
library(forecast)

df <-  read_csv('https://raw.githubusercontent.com/HumayDS/Digital-Data-Analytics/main/ts_data.csv')

df$date<- as.Date(df$InvoiceDate)
df$month<- month(df$date,label = TRUE)
df$week<- week(df$date)
df$year<-year (df$date)

df %>%  summary()

df %>% glimpse()


### Removing outlier  with IQR

Q1 <-  quantile(df$Revenue , probs = 0.25)

Q3 <-  quantile(df$Revenue , probs = 0.75)

IQR = Q3 - Q1
upper_bound <-  Q3 + 1.5* IQR

lower_bound <-  Q1 - 1.5*IQR

outlier_ind <- which(df$Revenue < lower_bound | df$Revenue > upper_bound)

outlier_ind

df <-  df[-outlier_ind,]


df %>%  summary()


##Create data for monthly, yearly, dauily
for_monthly<- df %>% group_by(month,year) %>% 
  summarise(sales= sum(Quantity,na.rm = TRUE)) %>% arrange(year,month)

for_weekly<-df %>% group_by(week,year) %>% 
  summarise(sales= sum(Quantity,na.rm = TRUE)) %>% arrange(year,week)

for_daily<- df %>% group_by(date,year) %>% 
  summarise(sales= sum(Quantity,na.rm = TRUE)) %>% arrange(year,date)


#Create time series dataset

#Create time series dataset

time_series_monthly<- ts(for_monthly[,3],start = c(2009,12),frequency = 12)
autoplot(time_series_monthly)

#install.packages('ggplot2')
library(ggplot2)
install.packages("ggfortify")
#library(ggfortify)
autoplot(time_series_monthly)
install.packages('forecast')
#library(forecast)
#### Seasonality and AUTO-correlations
#Seasonality is a pattern in data that repeats at regular 
#time intervals because of calendar-related or systematic effects.
#Seasonality = regular, predictable fluctuations that occur at the same time every cycle.


ggseasonplot(time_series_monthly)


### Auto_corelations

#ggAcf(time_series_monthly,lag.max = 24)

## Decomposition -- season, trend , remainder 
decompsition<- stl(time_series_monthly[,1],s.window = "periodic")

decompsition

autoplot(decompsition)


###trend
#trend shows whether values are increasing, decreasing, 
#or staying stable across a long period.
trend_series<-trendcycle(decompsition)

autoplot(trend_series)

### seasonality

seasonal_series<- seasonal(decompsition)


autoplot(seasonal_series)


#Analyse by month 
ggseasonplot(
  seasonal_series,
  year.labels = FALSE
)



###remainder
##Data cannot explain in mathmatical terms stays in residual
rem_series<-remainder(decompsition)

autoplot(rem_series)


ggseasonplot(
  rem_series,
  year.labels = TRUE,
  year.labels.left = TRUE
)

### stregnth of seasonality and trend

trend_stregnth<- max(0,1-((var(rem_series)/(var(rem_series+trend_series)))))

trend_stregnth
#0.11 means the trend is very weak.
#Only about 11% of the variation in the data 
#is explained by a long-term trend.
#Most of the changes come from seasonality or random 
#fluctuations, not from a steady increase or decrease over time.



seas_stregnth<- max(0,1-((var(rem_series)/(var(rem_series+seasonal_series)))))
seas_stregnth
#0.92 means the seasonal pattern is very strong
#About 92% of the variation in the series is explained by seasonality
#install.packages('hts')

#library(hts)

time_series_monthly

length(time_series_monthly) *0.8

train<- window(time_series_monthly,end=c(2011,7))

train
length(train)
length(time_series_monthly)

#Stasionarity

#Stationarity means that a time series has stable statistical properties over time.
#the mean is constant

#the variance is constant

#there is no long-term trend or changing seasonality



#Dickey fuller test for stationarity
##If p value < 0.05 ts is stationary
#Increasing sales with seasonality → non-stationary
library(tseries)
adf.test(train)


#Arima (requires stationary data )
#ARIMA (AutoRegressive Integrated Moving Average) is a time series forecasting model.

#AR (AutoRegressive) → uses past values

#I (Integrated) → makes the series stationary using differencing

#MA (Moving Average) → uses past forecast errors


arima_model<- auto.arima(train)
arima_model
forecast(arima_model,h=5) 
forecast(arima_model,h=5) %>% accuracy(time_series_monthly)


## myarima MA moving average
#required p-d-q


# 1 past value, 2 levels of differencing, 3 past forecast errors

myarima<- arima(train , order = c(1,2,3))
forecast(myarima,h=5) 
time_series_monthly
forecast(myarima,h=5) %>% accuracy(time_series_monthly)

autoplot(forecast(myarima,h=5))


SARIMA<- auto.arima(train,xreg = fourier(train,K=6),seasonal = TRUE)
forecast(SARIMA,xreg = fourier(train,K=6,h=5))

forecast_harmonic<- forecast(SARIMA,xreg = fourier(train,K=6,h=5))

plot(forecast_harmonic,
     main = "Forecast with SARIMA + Fourier Terms",
     xlab = "Time",
     ylab = "Value")
forecast_harmonic %>% accuracy(time_series_monthly)

forecast_harmonic %>% accuracy(time_series_monthly)  
write.csv(forecast_harmonic, 'forecast_harmonic.csv')




#An ARIMA(1,2,3) model indicates the following:

#Autoregressive (AR) term (p = 1): This means that the model includes one lagged observation from the time series to predict the current value. In other words, the current value of the time series is assumed to be linearly dependent on the value observed one time step ago.

#Differencing (d = 2): This implies that the time series has been differenced twice to achieve stationarity. Differencing is the process of computing the differences between consecutive observations to remove trends and seasonality. In this case, differencing has been applied twice to stabilize the mean and variance of the series.

#Moving Average (MA) term (q = 3): This indicates that the model includes three lagged forecast errors to predict the current value. In simpler terms, it suggests that the current value of the time series is influenced by the average of the errors from the three previous forecasts.

#So, in summary, an ARIMA(1,2,3) model uses one lagged observation, applies differencing twice, and includes three lagged forecast errors to make predictions about the future values of the time series.



