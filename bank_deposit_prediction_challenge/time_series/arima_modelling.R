## Load libraries ----
library(xts)
library(astsa)

## load data ----
data("sp500w")
data("soi")
data("AirPassengers")
data("djia")

## View the data ----
head(sp500w)
head(soi)
head(AirPassengers)
head(djia)
## Plot the data 
## Assumptions errors
## Independent
## Normal 
## Homoscedastic
plot(sp500w)
plot(soi)
plot(AirPassengers)
plot(djia$Close)
