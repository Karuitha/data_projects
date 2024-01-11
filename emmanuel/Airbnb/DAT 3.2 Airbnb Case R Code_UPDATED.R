# --------------------------------------------
#Importing data from Excel file
# Load package manager
if(!require(pacman)){
    install.packages('pacman')
}

#Load packages
pacman::p_load(readxl, tidyverse,
               GGally,performance,
               broom)

## Specify precision for digits (3 decimal places)
options(digits = 3)

## Disable scientific notation in output
options(scipen=999)

# -------------------------------------------------------
## Import the data
Data_file_1 <- read_excel("DAT 3.2 Airbnb Case Data File.xlsx")
glimpse(Data_file_1)

# ------------------------------------------------------
#Part 1 - Customer

#Run regression
fit <- glm(Choice ~ Email_25 +	Email_Taxi + Gmail + yahoo + Edu + AlaskaFF + Add_Ore +	Add_Eug +	Age	+ Tickets +	RoundTrip, family=binomial(link='logit'), data = Data_file_1)

summary(fit)

## Get detailed regression output ----
## easy way ------
broom::tidy(fit)
broom::glance(fit)
broom::augment(fit)

#Exporting the data to CSV file
fit |>
    broom::tidy() |>
    write_csv("Customer.csv")

# -------------------------------------------------------------
#Part 2 - Host

#Importing data set
Data_file_2 <- read_excel("DAT 3.2 Airbnb Case Data File.xlsx",
                          sheet = "Host")
glimpse(Data_file_2)

#Run regression
fit2 <- glm(Choice ~ Length + Type_Hm + Guests + ZestimateK + ListPrice + Rating + Rev_AbB + Loc_1 + Loc_2 + Loc_3 + Loc_4 + Loc_5, family=binomial(link='logit'), data = Data_file_2)
summary(fit2)


#Getting the coefficients, standard errors, t-values and p-values for all regression
## The easy way
broom::tidy(fit2)
broom::glance(fit2)
broom::augment(fit2)

#Exporting the data to CSV file
fit2 |>
    broom::tidy() |>
    write.csv("Host.csv")


