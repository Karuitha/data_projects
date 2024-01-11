library(glue)
secret_number = sample(1:10, 1, replace = TRUE)

print(secret_number)

trial_number = 0

number_of_trials = 3

lives = 30


while(trial_number < number_of_trials){
  
  my_guess = as.integer(readline(prompt = "Enter a number: "))
  
  trial_number = trial_number + 1
  
  if(my_guess == secret_number){
    
    lives = lives + 10
    
    print(glue("You won on the {trial_number} attempt. You have {lives} lives left"))
    
    break
    
  }else{
    
    lives = lives - 10
    
    if(trial_number < 3){
      
      print("You failed, try again!! You have {lives} lives left.")
      
    }else{
      
      print(glue("You failed in all {trial_number} attempts"))
      
    }
    
  }
  
}



## car game  ----