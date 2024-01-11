## Load required libraries
if (!require(pacman)) {
    install.packages("pacman")
}

pacman::p_load(
    fpp2, tidyverse, janitor,
    xts, astsa
)

## Load the data files
data(goog)
data(ausbeer)

## Explore the data
head(goog)
start(goog)
end(goog)
frequency(goog)
deltat(goog)

head(ausbeer)

## Plot the data
plot(goog)
autoplot(goog)

# ggseasonplot(goog)
# ggsubseriesplot(goog)
gglagplot(goog)

ts.plot(diff(goog))
acf(goog, plot = TRUE)
ggAcf(goog)
ggAcf(diff(goog))
ggAcf(diff(log(goog)))

Box.test(diff(goog), type = "Ljung-Box")

## Generate white noise process
wn_series <- arima.sim(
    model = list(order = c(0, 0, 0)),
    n = 500
)

head(wn_series)
autoplot(wn_series) + labs(title = "White Noise Process")
ggAcf(wn_series)
gglagplot(wn_series)
Box.test(wn_series, type = "Ljung")

## Read in the exercise data ----
my_data <- readxl::read_excel("exercise1.xlsx")
head(my_data)
my_data <- ts(my_data,
    start = c(1981, 1),
    frequency = 4
)

start(my_data)
end(my_data)
frequency(my_data)
head(my_data)

## Plot the datasets
ts.plot(my_data[, "Sales"])
ggseasonplot(my_data[, "Sales"])
ggseasonplot(my_data[, "Sales"], polar = TRUE)
ggsubseriesplot(my_data[, "Sales"])
gglagplot(my_data[, "Sales"])

## slice beer from 1992 index
beer <- window(ausbeer, start = 1992)
autoplot(beer)
ggAcf(beer)
ggseasonplot(beer)
ggseasonplot(beer, polar = TRUE)
gglagplot(beer)

## Time series patterns
## Seasonal
## Cyclic
## Trended
forecast_google <- naive(goog, h = 300)
autoplot(forecast_google)
summary(forecast_google)
snaive(goog)
frequency(my_data)

## creating a training and testing set
start(my_data)
end(my_data)

my_data_training <- window(my_data, end = c(2003, 4))
length(my_data_training)
my_data_testing <- window(my_data, start = c(2004, 1))
length(my_data_testing)


prediction = naive(my_data_training[, "Sales"], h = 8)
accuracy(prediction, my_data_testing[, "Sales"])
tail(my_data_training)
myCV_forecast <- tsCV(my_data_training[, "Sales"], forecastfunction = meanf, h = 8)
colMeans(myCV_forecast, na.rm = TRUE)
accuracy(

    colMeans(myCV_forecast, na.rm = TRUE),
    my_data_testing[, "Sales"]

)
