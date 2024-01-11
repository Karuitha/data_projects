## Load required packages ----
if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load(tidyverse, cronR)

## Check list on cron jobs
cronR::cron_ls()

## Create a cron job to scrap website
my_ls <- cron_rscript("/home/johnkaruitha/scrap_tyson.R")
