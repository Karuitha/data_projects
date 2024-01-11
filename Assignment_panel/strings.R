## Strings ----
library(tidyverse)
## These are equivalent
string1a <- "This is a string."
string1b <- "This is a string."

string1a == string1b
##############################
## These are NOT equivalent ----
string2a <- "My name is 'John'"
string2b <- 'My name is "John"'
string2a == string2b
charToRaw(string2a)
charToRaw(string2b)

## View a string
str_view(string1a)
str_view(string1b)
## Use string view to  view part 
str_view(string2a, "John")
str_view(string2a, "J.")
