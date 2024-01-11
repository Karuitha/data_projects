## Tweeter data ----
## Using hashes allows filter data 
library(twitteR)
library(rtweet)
library(httpuv)
## This setup allows one to authenticate twitter account. Run it just once. 
# auth_setup_default()
## Twitter API more open and accessible
??search_tweets ## Extracts tweets based on a hash tag
??stream_tweets ## Samples tweets
search_tweets(q = "#ruto")

??get_timeline ## Extracts tweets based on timelines. 
get_timeline("@kingzz2005")

## Get fans of Manchester United
manu_tweets <- search_tweets("@ManUtd", n = 5000)
sort(table(manu_tweets$id), decreasing = TRUE)

my_tweeter_data <- lookup_users(c("DavidNdii", "ahmednasirlaw", 
                                  
                                  "DonaldBKipkorir", "RailaOdinga", 
                                  
                                  "williamruto"))
my_tweeter_data$followers_count

library(tidyverse)
library(glue)
counter = 0
for(i in 1:10){
  if(i %% 2 == 0){
    counter = counter + 1
    print(i)
  }
}

print(glue("We have {counter} even numbers in this range"))


fizz_buzz <- function(number){
  
  if(number %% 3 == 0 & number %% 5 == 0){
    return("Fizz Buzz")
  } else if(number %% 3 == 0){
    return("Fizz")
  } else if(number %% 5 == 0){
    return("Buzz")
  } else {
    return("Dumb")
  }
  
}

fizz_buzz(15)
