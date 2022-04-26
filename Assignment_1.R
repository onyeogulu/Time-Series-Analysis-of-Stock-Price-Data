#====================================================================================================================
# install packages for the project
library(ggplot2)
library(ggfortify)
library(zoo)
library(reshape2)
library(seasonal)
library(readxl)
library(forecast)
#====================================================================================================================
#read in the data set
Tochukwu <- read_excel("Tochukwu.xlsx")
head(Tochukwu)

# create a time series plot for the data
tsdata = ts(Tochukwu, start = c(2006,1),frequency = 12)
plot.ts(tsdata,xlab="Time (Month)",ylab="Monthly Income",pch=19,xant="n")

# create new column for time (t)
Tochukwu["t"]<-seq(1,nrow(Tochukwu))
#create new column for time squre (t^2)
Tochukwu["t2"]<-Tochukwu$t**2
#create new column for time squre (t^3)
Tochukwu["t3"]<-Tochukwu$t**3
#create new column for time squre (t^4)
Tochukwu["t4"]<-Tochukwu$t**4

View(Tochukwu)
#====================================================================================================================

#trend analysis for the time series datset

# Trend analysis using simple linear regression model
trend1=lm(Mo~t, data=Tochukwu)
summary(trend1)
# prediction of the liner model
pred_1 <- predict(trend1)
#residual f the regression model
resd <- residuals(trend1)
#plot of residual
plot(resd,type = "l",ylab = "Residuals")
#add a horizontal line at 0 
abline(0,0, col="red")

#plot the linear model and time series data
plot(Tochukwu$t,Tochukwu$Mo, type = "l",xlab = "Time (Month)", ylab = "Monthly Income")
lines(Tochukwu$t,pred_1,col="red")

# Trend analysis using quadratic polynominal regression model
trend2=lm(Mo~t+t2, data=Tochukwu)
summary(trend2)
# prediction of the polynomial model
pred_2 <- predict(trend2)
#residual for the polynomial regression model
resd_1 <- residuals(trend2)
#plot of residual from the polynomial regression model
plot(resd_1,type = "l",ylab = "Residuals")
#add a horizontal line at 0 
abline(0,0, col="red")

#plot the quadratic polynomial model and time series data
plot(Tochukwu$t,Tochukwu$Mo, type = "l",xlab = "Time (Month)", ylab = "Monthly Income")
lines(Tochukwu$t,pred_2,col="red")

# Trend analysis using cubic polynominal regression model
trend3=lm(Mo~t+t2+t3, data=Tochukwu)
summary(trend3)
# prediction of the polynomial model
pred_3 <- predict(trend3)
#residual for the polynomial regression model
resd_2 <- residuals(trend3)
#plot of residual from the polynomial regression model
plot(resd_2,type = "l",ylab = "Residuals")
#add a horizontal line at 0 
abline(0,0, col="red")

#plot the cubic polynomial model and time series data
plot(Tochukwu$t,Tochukwu$Mo, type = "l",xlab = "Time (Month)", ylab = "Monthly Income")
lines(Tochukwu$t,pred_3,col="red")

# Trend analysis using quatic polynominal regression model
trend4=lm(Mo~t+t2+t3+t4, data=Tochukwu)
summary(trend4)
# prediction of the polynomial model
pred_4 <- predict(trend4)
#residual for the polynomial regression model
resd_3 <- residuals(trend4)
#plot of residual from the polynomial regression model
plot(resd_3,type = "l",ylab = "Residuals")
#add a horizontal line at 0 
abline(0,0, col="red")

#plot the cubic polynomial model and time series data
plot(Tochukwu$t,Tochukwu$Mo, type = "l",xlab = "Time (Month)", ylab = "Monthly Income")
lines(Tochukwu$t,pred_4,col="red")
#====================================================================================================================
# Moving average smoothing technique
tsm <- ma(tsdata,order=12)
plot(tsdata,xlab="Time (Year)",ylab="Monthly Rainfall",col = "black")
lines(tsm,col="red")
legend(x = "topleft",legend=c("Original series", "Moving average smoothed series"), fill = c("black","red"))

# Holt-Winter Smoothig technique
# Holt-Winters additive method
fit1 <- hw(tsdata,seasonal="additive")
summary (fit1)
# Holt-Winters multiplicative method
fit2 <- hw(tsdata,seasonal="multiplicative")
summary (fit2)

# plot of Holt-Winters additive smoothing and the original series
plot.ts(tsdata,xlab="Time (Year)",ylab="Monthly Rainfall",col = "black")
lines(fit1$fitted, col = "red")
legend(x = "topleft",legend=c("Original series", "HW additive smoothed series"), fill = c("black","red"))

# plot of Holt-Winters multiplicative smoothing and the original series
plot.ts(tsdata,,xlab="Time (Year)",ylab="Monthly Rainfall",col = "black")
lines(fit2$fitted, col = "red")
legend(x = "topleft",legend=c("Original series", "HW multiplicative smoothed series"), fill = c("black","red"))
#====================================================================================================================
# decomposition of the time series data 
m <- seas(tsdata, na.action = na.omit)
# summary of the decompose data
summary(m)
# plot of the decomposed data
autoplot(m)

# plot of adjusted series and the original series
plot.ts(tsdata,xlab="Time (Year)",ylab="Monthly Rainfall",col = "black")
lines(final(m), col = "red")
legend(x = "topleft",legend=c("Original Series", "Seasonally Ajusted Series"), fill = c("black","red"))

# plot of adjusted series and the original series
autoplot(tsdata, series="Data") + 
  autolayer(seasadj(m), series = "Seasonally Adjusted") + 
  xlab("Time (month)") + ylab("Monthly Income") + 
  scale_color_manual(values = c("black","red"),breaks = c("Data", "Seasonally Adjusted"))

# extracting the adjusted series 
ajusted_series<-seasadj(m)

# coverting the adjusted series to a dataframe
df <- data.frame(date=as.Date(index(ajusted_series),origin = "2006-01-01"), Monthly_Income = melt(ajusted_series)$value)

# create new column for time (t) in the adjusted series dataframe
df["t"]<-seq(1,nrow(df))
View(df)

# Trend analysis using simple linear regression model on the adjusted time series data
adjusted_trend=lm(Monthly_Income~t, data=df)
# summary of the linear model on the adjusted time series data
summary(adjusted_trend)
# prediction of the liner model
adjusted_pred <- predict(adjusted_trend)

#residual of the linear regression model
adjusted_resd <- residuals(adjusted_trend)

#plot of residual
plot(adjusted_resd,type = "l",ylab = "Residuals")
#add a horizontal line at 0 
abline(0,0, col="red")

#plot the linear model and adjuste time series data
plot(df$t,df$Monthly_Income, type = "l",xlab = "Time (Month)", ylab = "Monthly Income")
lines(df$t,adjusted_pred,col="red")
legend(x = "topleft",legend=c("Ajusted Series", "Linear Model"), fill = c("black","red"))

# creat a new data set to predict one year ahead
t<-seq(193,204)
df_2<-data.frame(t)
df_2

# using the linear model to predict on the new data and saving it as a column in the dataframe
df_2["Monthly_income_forecast"] <- predict(adjusted_trend,df_2)

# creat a new colum on the multiplicative seasonal index of the time series data
df_2["Multipicative_seasonal_factor"]<-decompose(tsdata, type = "multiplicative")$seasonal[1:12]

# creatig the adjusted forecast for the model
df_2["Adjusted_Monthly_income_forecast"] <- df_2$Monthly_income_forecast*df_2$Multipicative_seasonal_factor

# plot of the monthly income forecast and adjusted monthly icome forecats for in the next one year 
ggplot(df_2, aes(x=t, y = value, color = Series)) + 
  geom_line(aes(y =Monthly_income_forecast , col = "Monthly income forecast")) + 
  geom_line(aes(y = Adjusted_Monthly_income_forecast, col = "Adjusted monthly income forecast"))+
  xlab("Time")+ylab("Monthly Income")

View(df_2)

#plot the original series, adjuste series, trend forecast and adjusted forecast data
plot(Tochukwu$t,Tochukwu$Mo, type = "l",xlab = "Time (Month)", ylab = "Monthly Income",xlim=c(0,210),ylim=c(80000,270000))
lines(df$t,ajusted_series,col="red")
lines(df_2$t,df_2$Monthly_income_forecast, col="blue")
lines(df_2$t,df_2$Adjusted_Monthly_income_forecast, col="grey")
legend(x = "topleft",legend=c("Original Series", "Seasonal Ajusted series", "Trend Forecast","Adjusted Forecast"), fill = c("black","red","blue","grey"))

#============================================================================================================================================================

# TIME SERIES USING STOCHASTIC APPROACHE (SARIMA MODEL)
#read in the data set
Tochukwu_1 <- read_excel("Tochukwu.xlsx")
head(Tochukwu_1)

# log of the monthly income in other to decrease variance in the series
Tochukwu_1$Mo<- log(Tochukwu_1$Mo)

# create a time series plot for the log data
tsdata_1 = ts(Tochukwu_1, start = c(2006,1),frequency = 12)
plot.ts(tsdata_1,xlab="Time (Month)",ylab=" Log(Monthly Income)",pch=19,xant="n")

# acf and pcf of the log series 
autoplot(Acf(tsdata_1))
autoplot(Pacf(tsdata_1))

# first difference of the series 
tsdata_1_diff_1<- diff(tsdata_1)
plot.ts(tsdata_1_diff_1,xlab="Time (Month)",ylab=" Log(Monthly Income)_1",pch=19,xant="n")
# acf and pacf plot
autoplot(Acf(tsdata_1_diff_1))
autoplot(Pacf(tsdata_1_diff_1))

# second difference of the series at lag 12
tsdata_1_diff_1_12<- diff(tsdata_1_diff_1, lag = 12)
plot.ts(tsdata_1_diff_1_12,xlab="Time (Month)",ylab=" Log(Monthly Income)_1",pch=19,xant="n")
# acf and pacf plot
autoplot(Acf(tsdata_1_diff_1_12))
autoplot(Pacf(tsdata_1_diff_1_12))

# Model 1
fit1 <- Arima(tsdata_1, order=c(0,1,1), seasonal=c(0,1,2), include.constant = FALSE)
summary(fit1)
checkresiduals(fit1)

# Model 2
fit2 <- Arima(tsdata_1, order=c(2,1,0), seasonal=c(0,1,2), include.constant = FALSE)
summary(fit2)
checkresiduals(fit2)

# Model 3
fit3 <- Arima(tsdata_1, order=c(2,1,1), seasonal=c(0,1,2), include.constant = FALSE)
summary(fit3)
checkresiduals(fit3)

# forecast one year ahead 
forecast_1<-forecast(fit2, 12)[4]
data<- t(data.frame(matrix(unlist(forecast_1), nrow = length(forecast_1),
                         byrow = TRUE)))
# convert forecast to a data frame
data<- data.frame(data)
# take exponential of the forecast to compare with forecast from classical approach
data<-exp(data$data)

# merge forecast from classical approch with forecast from sarima model to data frame
both_forecast<- data.frame(sarima_forecast=data, linear_trend_forecast=df_2$Adjusted_Monthly_income_forecast)


#plot the sarima forecast and linear trend forecast
plot(both_forecast$sarima_forecast,type = "l",xlab = "Time (Month)", ylab = "Monthly Income",ylim=c(180000,270000))
lines(both_forecast$linear_trend_forecast,col="red")
legend(x = "bottomright",legend=c("SARIMA Forecast", "Linear Trend Forecast"), fill = c("black","red"))
