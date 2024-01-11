## Vector with duplicates
library(tidyverse)
my_vector <- c(1, 1, 2, 4, 4, 5, 6, 7, 7, 7, 9)

## NULL vector to hold unique values
my_unique_vector <- c()

## For loop to remove duplicates (Can also use unique fn)
for(element in my_vector){
    
    ## Capture values not already in unique vector
    if(!element %in% my_unique_vector){
        
        my_unique_vector <- append(my_unique_vector, element)
        
    }
}
