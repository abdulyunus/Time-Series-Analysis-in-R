
# Time series Analysis R codes on Air passengers data

# This function is use to install and load the required packages
gc()
rm(list = ls(all = TRUE))

packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

# We need the blow packages for time series analysis
packages(tseries)
packages(forecast)


# Import the data for the time series Analysis #

data("AirPassengers")
class(AirPassengers)

#This is the start of the time series 
start(AirPassengers)

#This is the end of the time series
end(AirPassengers)

#This tell us the intervals
frequency(AirPassengers)

#Summary of the data
summary(AirPassengers)

# Ploting the time series

plot(AirPassengers, main = "Air Passengers Data")

#fitting the line

abline(reg = lm(AirPassengers ~ time(AirPassengers)))

#This will print the data for all the cycle (Frequency)
cycle(AirPassengers)


# Lets check if the series is stationary
# SInce we are using auto.arima function, we don't need to test for the stationary and no need to find out P and q values.

adf.test(AirPassengers, alternative = 'stationary', k = 0)

# if we check the p value of the test, it is less than 0.05, which means, the series is stationary.


plot(log(AirPassengers))

plot(diff(log(AirPassengers)), main = "Air Passenger numbers from 1949 to 1961", ylab = "Air Passengers", xlab = "Time")


# ARIMA - *Autoregressive Moving Average Integration* - seeing the past values and predicting the future values
# We will be using auto.arima function to check the best model for our data.

model = auto.arima(AirPassengers, ic = 'aic', trace = TRUE)  # Passing tace = TRUE will help us to get all the 
summary(model)

# It select the model which has lower AIC value

# lets plot the residuals 

plot(ts(model$residuals))


acf(ts(model$residuals))
pacf(ts(model$residuals))


# Use model to forecast for next 10 years

pred_plot_ts = forecast(model, level = c(95), h = 10*12)
# In case, we want to see the forecasted result, remove the # from the below line. 
#pred_plot_ts
plot(pred_plot_ts)



# These things are optional
acf(AirPassengers)
pacf(AirPassengers)

# Finally, lets do the model diagnosis
tsdiag(model)
