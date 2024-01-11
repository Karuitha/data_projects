## Libraries
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(quantmod)
library(Quandl)


## Make a matrix 
my_raw_data <- matrix(1:16, nrow = 4)
my_raw_data


## Make a vector of dates
my_dates <- seq(as.Date("2021-12-01"), length = 4, by = "days")
my_dates

## Make an xts object
my_xts_data <- xts(my_raw_data, order.by = my_dates)
my_xts_data

## subsetiing
my_xts_data[2016/2016-12-02, 1]
coredata(my_xts_data)
index(my_xts_data)


as.Date("1977-09-17") + 0:30


## Load the sunspots dataset
data("sunspots")
head(sunspots)
class(sunspots) ## This is a ts object. 

sunspots_xts <- as.xts(sunspots)
head(sunspots_xts)


## Perfomance analytics edhec data for time based querries
data("edhec", package = "PerformanceAnalytics")
head(edhec)
class(edhec)


edhec["1997-01-01/1998-12-31", 1]
edhec["1997-02/05", 1]
edhec["199702/"]

## First and last 
first(edhec[, "Funds of Funds"], "12 weeks")
??xts::xts

edhec_97_01 <- edhec["1997/2001"]

ep = endpoints(edhec_97_01, on = "years")
ep

period.apply(edhec_97_01[, "Convertible Arbitrage"], INDEX = ep, FUN = mean)
apply.weekly(edhec_97_01[, "Convertible Arbitrage"], FUN = mean)


??to.period
to.period(edhec[, "Convertible Arbitrage"], period = "months", name = "arb")

my_seq_dates <- seq(as.Date("2022-01-01"), length = 50, by = "years")
my_seq_dates


another_seq_dates <- as.Date("2022-01-01") + 0:400
another_seq_dates

format(Sys.Date(), format = "%A %B %d, %Y")
?strptime

names(edhec)


last(edhec[, "Convertible Arbitrage"], "1 month")
?merge
?diff
?
?split 
?rollapply
help("OlsonNames")
tzone(edhec)
Sys.setenv(TZ = "Africa/Nairobi")


getSymbols("AAPL")
head(AAPL)
index(AAPL)
coredata(AAPL)
periodicity(AAPL)
to.yearly(AAPL)
?to.period
tzone(AAPL)
tclass(AAPL)
tformat(AAPL) <- "%b %d, %Y"
head(AAPL)
nmonths(AAPL)
nquarters(AAPL)


#############################################
getSymbols(Symbols = "^DJI", auto.assign = TRUE)
tail(DJI)

getSymbols("GBP/CAD", src = "oanda")

Quandl::Quandl(code = "FRED/DEXUSEU")
quantmod::oanda.currencies

getSymbols("USD/KES", from = Sys.Date() - 180, to = Sys.Date(), src = "oanda")
head(USDKES)
?plot
plot(USDKES, main = "USD-Kenya Shilling Exchange Rates, Last 180 Days",
     
     xlab = "Date", ylab = "Rate", sub = "John Karuitha")


## Real GDP Kenya
getSymbols("KENNGDPRPCPCPPPT", src = "FRED")
plot(KENNGDPRPCPCPPPT)
Quandl(code = "FRED/GDP") -> here
head(here)


## Create a new environment to load data
my_env <- new.env()

getSymbols(c("QQQ", "SPY"), auto.assign = TRUE, env = my_env)

head(my_env$QQQ)
head(my_env$SPY)
hi_list <- lapply(my_env, Hi)
merged_list <- do.call(merge, hi_list)
merged_list


## Data from FRED
getSymbols("NAEXKP01CAQ189S", src = "FRED")
head(NAEXKP01CAQ189S)
plot(NAEXKP01CAQ189S)

Quandl(code = "FRED/NAEXKP01CAQ189S")
