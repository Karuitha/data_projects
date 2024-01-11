# --------------------------------------------
#Importing data from Excel file
# Load package manager
if(!require(pacman)){
    install.packages('pacman')
}

#Load packages
pacman::p_load(readxl, tidyverse, GGally)

## Specify precision for digits (3 decimal places)
options(digits = 3)

## Disable scientific notation in output
options(scipen=999)

#Importing data from Excel file
#Load package and importing data
Data_file_1 <- read_excel("TKL Case Data File_UPDATED.xlsx")
View(Data_file_1)

#Part 1 - Results of Acquisition Choice Model

#Run regression
fit <- glm(Acquired ~ Price + Warranty + Delivery_Time + Sales_Support + Industry_Group_Ele + Industry_Group_Cons + Firm_Size + Buying_Center, family=binomial(link='logit'), data = Data_file_1)
summary(fit)

#Getting the co-efficients, standrad errors, t-values and p-values for all regression
broom::tidy(fit)
broom::glance(fit)
broom::augment(fit)

#Exporting the data to CSV file
fit |>
    broom::tidy() |>
    write.csv("TKL_Acquisition.csv")


#Part 2 - Results of Choice Choice Model

#Importing data set
Data_file_2 <- read_excel("TKL Case Data File_UPDATED.xlsx",
                          sheet = "Expansion")
View(Data_file_2)

#Run regression
fit2 <- glm(Expansion ~ Price + Warranty + Delivery_Time + Sales_Support + Industry_Group_Ele + Industry_Group_Cons + Firm_Size + Buying_Center, family=binomial(link='logit'), data = Data_file_2)
summary(fit2)

#Getting the co-efficient, standard errors, t-values and p-values for all regression
broom::tidy(fit2)
broom::glance(fit2)
broom::augment(fit2)

#Exporting the data to CSV file
fit2 |>
    broom::tidy() |>
    write.csv("TKL_Expansion.csv")
