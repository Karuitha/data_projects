## Load libraries ----
library(plm)
library(punitroots)
data("OECDunemp")

## View the data structure ----
str(OECDunemp)
class(OECDunemp)
head(OECDunemp)
nrow(OECDunemp)
ncol(OECDunemp)

## Do the stationarity tests ----

