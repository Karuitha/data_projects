## Load libraries
library(quantmod)
library(Quandl)
library(xts)

## Load google data from quantmod
data_env <- new.env()
getSymbols(c("GOOG", "AAPL"), env = data_env, auto.assign = TRUE)

my_adjusted_prices <- lapply(data_env, Ad)

final_prices <- do.call(merge, my_adjusted_prices)
head(final_prices)

getSymbols(Symbols = c("GOOG", "AAPL"), auto.assign = TRUE)

## Explore the data 
nyears(GOOG)
nyears(AAPL)

head(GOOG)
Hi(GOOG["2010/"])

## Plot the data
plot(Hi(GOOG), main = "Google Stock Price, 2007- ", col = "red")
lines(Lo(GOOG), col = "blue")
lines(Ad(GOOG), col = "yellow")

## Join google and apple
goog_label <- xts(rep("GOOG", times = nrow(GOOG)), order.by = index(GOOG))
??merge


GOOG + merge(goog_label, index(GOOG))
nrow(GOOG)
nrow(goog_label)
hello <- merge(GOOG, goog_label)
hello
hello$goog_label <- "GOOG"

merged_data <- xts(cbind(coredata(GOOG), coredata(goog_label)), 
    
    order.by = index(GOOG))
head(merged_data)

## My data 
## Compare daily 10 year treasuries constant maturity date 
## With USA GDP 
getSymbols(Symbols = c("DGS10", "GDP"), src = "FRED")
head(DGS10)
head(GDP)


## Convert the treasuries to quarterly
?apply.quarterly
DGS10_quarterly <-  apply.quarterly(DGS10, median, na.rm = TRUE)
index(GDP) <- as.yearqtr(index(GDP))
index(DGS10_quarterly) <- as.yearqtr(index(DGS10_quarterly))

merge(GDP, DGS10_quarterly)


date_times <- seq(from = as.POSIXct("2016-01-16 08:00"),
                  
                  to = as.POSIXct("2016-01-17 18:00"),
                  
                  by = "2 hours")
head(date_times)

regular_xts <- xts(, order.by = date_times)


adjustOHLC(GOOG)
adjustOHLC(AAPL)
getDividends("GOOG")
getSplits("AAPL")
Ad(AAPL)


isTRUE(c(TRUE, TRUE, TRUE, TRUE))
isFALSE(c(FALSE, FALSE, FALSE))
?isTRUE
isTRUE("TRUE")
