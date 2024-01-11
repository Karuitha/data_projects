library(tidyverse)
upper_case <- LETTERS
lower_case <- letters

word = "John Karuitha"

word <- word %>% 
    str_extract_all(boundary("character")) %>% 
    as.character() %>% 
    str_replace_all('[:punct:] | " "', "")

up = vector()
lw = vector()

for(letter in word){
    if(letter %in% upper_case){
        up = append(up, letter, after = length(up))
    }
}

nchar(up)
nchar(lw)
