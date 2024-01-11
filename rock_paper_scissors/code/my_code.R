## In this project, we develop the create the rock-Spock-paper-lizard-scissors game. 

## Load required packages 
library(tidyverse)
library(glue)

## Suppress warnings 
options(warn=-1) ## Source: https://intellipaat.com/community/5013/how-to-suppress-warnings-globally-in-an-r-script

## In this assignment, I create the rock-Spock-paper-lizard-scissors game. 

## 0 — rock; 1 — Spock; 2 — paper; 3 — lizard; 4 — scissors

## The game pits one person against the computer. 
## The player will choose one of rock; Spock; paper; lizard; scissors. 
## The computer also simultaneously chooses one of these levels. 
## Each level wins against the two preceeding levels. 
## For instance, paper wins against rock and Spock. 
## Likewise, each level loses against the two proceeding levels. 
## For instance paper loses against lizard and scissors. 
## Note that scissors and rock are adjacent to complete the cycle. 

## @knitr 
## Create a function to convert a name 
## (rock; Spock; paper; lizard; scissors) to number (0-4).
name_to_number <- function(name){
  if(name == "rock"){
    return(0)
  } else if(name == "Spock"){
    return(1)
  } else if(name == "paper"){
    return(2)
  } else if(name == "lizard"){
    return(3)
  } else if(name == "scissors"){
    return(4)
  } else{
    return("Error!! Your choice is out of range: 
           Please choose one of: rock, Spock, paper, lizard and scissors.")
  }
}

## Create a function to convert a number (0-4) to a name (rock; Spock; paper; lizard; 4. scissors)
number_to_name <- function(number){
  if(number == 0){
    return("rock")
  } else if(number == 1){
    return("Spock")
  } else if(number == 2){
    return("paper")
  } else if(number == 3){
    return("lizard")
  } else if(number == 4){
    return("scissors")
  } #else{
    #return("Error!! Your choice is out of range: 
           #Please choose one of: rock, Spock, paper, lizard and scissors.")
  #}
}

number_to_name(4)


## Capture player choice and run the game 
game_function <- function(){
  
  ## Input the player choice 
  player_choice = readline("Input your choice: ")
  
  ## In this section, I take care of possible mis-spelings
  ## and white spaces in the choices
  if(player_choice == "spock"){
    player_choice = player_choice %>% 
      str_to_title() %>% 
      str_trim()
  } else{
    player_choice = player_choice %>% 
      str_to_lower() %>% 
      str_trim()
  }
  
  
  ## In this section, I print out the player choice 
  if(player_choice %in% c("rock", "Spock", "paper", "lizard", "scissors")){
    print(glue("Player chooses {player_choice}"))
  } 
  
  ## Convert player choice into a number
  player_number = name_to_number(player_choice)
  
  ## Computer generates a number
  comp_number = sample(x = 0:4, size = 1, replace = TRUE)
  
  ## Print out the computer choice
  comp_choice = number_to_name(comp_number)
  
  print(glue("Computer chooses {comp_choice}"))
  
  ## Decide who wins between player and computer
  if(is.na(as.numeric(player_number))){
    print("Error!! Your choice is out of range:
          
          Please choose one of: rock, Spock, paper, lizard and scissors.")
    
  } else if(comp_number == player_number){
    print("Computer and player tie!")
  } else if((comp_number - player_number)%% 5 %in% c(1, 2)){
    print("Computer wins!")
  } else{
    print("Player wins!")
  }
}

game_function()
