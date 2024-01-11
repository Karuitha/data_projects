library(tidyverse)
library(glue)

secret_number = sample(0:9, size = 1, replace = TRUE)

guess_limit = 3

guess_count = 0

while(guess_count < guess_limit){
        
        guess = readline("Enter number: ")
        
        guess_count = guess_count + 1
        
        guess_remain = guess_limit - guess_count
        
        if(guess == secret_number){
                
                print(glue("You got it right on the {guess_count} attempt."))
                
                break
        }else{
                
                print(glue("You failed. You have {guess_remain} attempts remaining. Please try again."))
                
        }
} 
