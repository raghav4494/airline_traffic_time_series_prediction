# Loading Original Dataframe
data("AirPassengers")

# To count the number of time periods per instance
frequency(AirPassengers)

# Year vs Travellers count - plot
plot(AirPassengers)

# Converting dataset in the way, mean and variance becomes constant
# Log Function - Make Variance even
# Differentiation - Make mean constatn throughout series 

plot(diff(log(AirPassengers)))

# Building model - ARIMA Model

# ARIMA - Auto Regressive Integration Moving Average

# To find value of q
# Auto corelation Function
acf(diff(log(AirPassengers)))
q = 1

# To find value of p
# Partial Auto Corelation Function
pacf(diff(log(AirPassengers)))
p = 0

# Building our model
# p = 0, d = 1, q = 1
# d = 1 denotes, data has to be differentiated once
arima_model = arima(log(AirPassengers), c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))

# Predicting what will be the passenger's count for the next 10 years with our model 
test_model = predict(arima_model, n.ahead = 10*12)

# Converting log value to decimal values - as we have used log function
test_values = 2.718^test_model$pred

# Plot the input data and prediction in single plot
ts.plot(AirPassengers,test_values, log = "y", lty=c(1,3))

# To check how our model performs 
# Original Dataset contains values for years 1949 to 1960
# To test our model, training data is going to have values between 1949 and 1959
filtered_data = ts(AirPassengers, frequency = 12, start = c(1949,1), end = c(1959,12))

# Fit our model with the filtered data
test_arima_model = arima(log(filtered_data), c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))

# Predicting for the year 1960
predict_1960 = predict(test_arima_model, n.ahead = 1 * 12)

predict_1960_values = 2.718^predict_1960$pred

# Round off to nearby integer
predict_1960_values = round(predict_1960_values, digits = 0)

# original Values
original_1960 = tail(AirPassengers, 12)

#               sample Result for the year 1960 

#          Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#Our model 419 399 466 454 473 547 622 630 526 462 406 452
#Original  417 391 419 461 472 535 622 606 508 461 390 432
