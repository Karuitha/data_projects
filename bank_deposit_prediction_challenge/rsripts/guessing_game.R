if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, glue)
maximum_attempts = 3
attempt_number = 0

secret_number = sample(0:9, size = 1, replace = TRUE)

while(attempt_number < maximum_attempts){
  
  my_guess = readline("Enter your guess: ") |> as.numeric()
  
  attempt_number = attempt_number + 1
  
  if(attempt_number >= maximum_attempts){
    
    print(glue::glue("You have exhausted all {attempt_number} attempts. Please start again"))
    
    break
    
  }else if(my_guess == secret_number){
    
    print(glue::glue("You got it right on attempt number {attempt_number}"))
    
    break
    
  } else{
    
    print(glue::glue("Please try again. You have {maximum_attempts - attempt_number} attempts left"))
    
  }
  
}
