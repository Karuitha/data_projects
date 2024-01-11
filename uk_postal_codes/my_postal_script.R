#############################################################
## validating London postcodes
## load required packages ----
library(tidyverse)
#library(PostcodesioR)

############################################################
## read in the data ----
input_file <- read_csv("InputFile.csv", col_names = "postcodes")

london_post_codes <- read_csv("LondonPostcodes.csv", col_names = "postcodes")

###########################################################
## Explore the input data ----
head(input_file)
tail(input_file)
class(input_file)

length(unique(input_file$postcodes))

##############################################################
## Explore the london postal codes data
head(london_post_codes)
tail(london_post_codes)
dim(london_post_codes)
class(london_post_codes)

length(unique(london_post_codes$postcodes)) ## all are unique entries

###############################################################
## Format of valid UK postal codes
formats <- c("AN NAA", "ANN NAA", "AAN NAA",
                      "AANN NAA", "ANA NAA", "AANA NAA")

##############################################################
## matching regex for a valid UK postal code 

## Status postal codes in input files

input_file$verdict <- sapply(input_file$postcodes, 
                             
                             
                      str_detect, "^[A-Z]{1,2}[1-9]{1,2}[A-Z]?\\s[1-9][A-Z][A-Z]$")

#######################################################################################
## Number of valid postal codes in input file = 926 0ut of 1500
sum(input_file$verdict)

#######################################################################################

## Status of postal codes in london postal codes
london_post_codes$verdict <- sapply(london_post_codes$postcodes, 
                                    
                             str_detect, "^[A-Z]{1,2}[1-9]{1,2}[A-Z]?\\s[1-9][A-Z][A-Z]$")

######################################################################################
## Number of valid postal codes in London postal codes 281,794
sum(london_post_codes$verdict)

#####################################################################################
##**********************************************************************************
### ISSUE 1: Actual London postcodes that are also included in LondonPostcodes.csv
## valid postal codes in input file
valid_input_file_post_codes <- input_file %>% 
  
  filter(verdict == TRUE)

## valid postal codes in london postal codes file
################
valid_london_post_codes <- london_post_codes %>% 
  
  filter(verdict == TRUE)

####
## There are 426 genuine postcodes in inputs that are also in london postcodes files
output1 <- valid_input_file_post_codes %>% 
  
  filter(postcodes %in% valid_london_post_codes$postcodes)

output1

###write csv
write.csv(output1, "output1.csv")


#########################################################################################
## *************************************************************************************
## ISSUE 2
## Values that adhere to one of the formats for a valid UK postcode but which are NOT in 
## the LondonPostcodes.csv file

output2 <- setdiff(valid_input_file_post_codes$postcodes, valid_london_post_codes$postcodes)

length(output2)

output2

### Write csv
write.csv(output2, "output2.csv")

########################################################################################
##*************************************************************************************
### ISSUE 3 Values that do not adhere to a valid UK postcode format." (in the input file)

output3 <- input_file %>% 
  
  filter(verdict == FALSE)


length(output3$postcodes) ##574

output3

## Write csv
write.csv(output3, "output3.csv")

########################################################################################
##************************************************************************************
## Solutions using the tidyverse

