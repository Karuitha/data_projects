#################################################
set.seed(2, sample.kind = "Rounding")
marks <- round(rnorm(100, mean = 60, sd = 10), 0)
#################################################

max(marks)
min(marks)

################################################
for(i in 1:length(marks)){
  if(marks[i] < 40){
    print("Fail")
  } else if(marks[i] >= 40 & marks[i] < 50){
    print("D")
  } else if(marks[i] >= 50 & marks[i] < 60){
    print("C")
  } else if(marks[i] >= 60 & marks[i] < 70){
    print("B")
  } else {
    print("A")
  }
}


#################################################
for(i in marks){
  if(marks < 40){
   print("Fail")
  } else if(i >= 40 & i < 50){
    print("D")
  } else if(i >= 50 & i < 60){
    print("C")
  } else if(i >= 60 & i < 70){
    print("B")
  } else {
    print("A")
  }
}
##################################################
library(tidyverse)
library(lubridate)
##################################################

name <- readline(prompt = "Please type in your name: ")

home_area <- readline(prompt = "Please type in your county of origin: ")

age_years <- readline(prompt = "Please type in your age: ")

print(paste("My name is", name, "from", home_area, "and I am", age_years, "years old"))
###################################################
print(R.version.string)
###################################################