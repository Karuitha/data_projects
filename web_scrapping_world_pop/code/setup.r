
library(knitr)
library(rmdformats)
library(prettydoc)
library(tidyverse)
library(rvest)
library(kableExtra)

## Global options
options(max.print="75")
opts_chunk$set(ech=TRUE,
               cache=TRUE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE)
opts_knit$set(width=75)

