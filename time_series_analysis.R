#import data set 
df <- read.csv("D:/R files/R_file/mayur sir assignment/DailyDelhiClimateTrain.csv")
#import libraries
library(tidyverse)
library(ggplot2)
library(tseries)
library(timeSeries)
library(plotly)
library(ggfortify)
library(forecast)
#check for null values
sum(is.na(df))
#time series plot
fig <- plot_ly(df, x =~date , y=~meantemp , type = "scatter",
               mode = "lines") %>% 
  layout(plot_bgcolor = "#46bd85", xaxis = list(rangeslider = list(visible = T))
         ,title="recorded tempreture")
fig

#density plot
fig_1<-plot_ly(df , type="scatter", mode="lines", fill="tozeroy",
               x=~date , y=~meantemp) %>% layout(title = "recorded tampreture")
fig_1


#create time series
df_new<-ts(df$meantemp , frequency = 12)
cycle(df_new)


#plot the time series 
autoplot(df_new)+ggtitle("time series")+xlab("years")+ylab("")

autoplot(decompose(df_new))

seasonplot(df_new , col = rainbow(12) , year.labels = TRUE )
#simple exponential model
simple_expo <- HoltWinters(df_new , beta = FALSE , gamma = FALSE)
accuracy(ts(simple_expo))

#double exponential model
double_expo <-HoltWinters(df_new , beta = FALSE)
accuracy(ts(double_expo))

#tripple exponential model 
tripple_expo <-HoltWinters(df_new)
accuracy(ts(tripple_expo))

#predict the next 30 values
forecast(tripple_expo ,30)
forecast(double_expo , 30)
forecast(simple_expo , 30)

#plot prediction
autoplot(tripple_expo)
autoplot(double_expo)
autoplot(simple_expo)

plot(forecast(simple_expo , 120))
plot(forecast(double_expo , 130))
plot(forecast(tripple_expo ,130))

#Automated forecasting using an exponential model
aem<-ets(df_new)
accuracy(aem)
forecast(aem , 30)
autoplot(aem)
plot(forecast(aem , 30))

#Automated forecasting using an ARIMA mode
auto_arima<-auto.arima(df_new)
accuracy(auto_arima)
forecast(auto_arima ,30)
plot(forecast(auto_arima ,130))

#SARIMA
sarima <- stlm(y = df_new , method = "arima")
summary(sarima$model)
forecast(sarima , 5)
accuracy(sarima)
plot(forecast(sarima , 120))

adf.test(df_new)
