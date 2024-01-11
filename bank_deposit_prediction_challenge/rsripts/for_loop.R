library(glue)

my_numbers <- function(a, b){
for(i in a:b){
  tens = floor(i / 10)
  ones = i %% 10
  print(glue("for {i}, the tens value is {tens} value while the ones value is {ones}"))
}
  }

my_numbers(0, 100)

for(i in seq(99, 1, -2)){
  if(i %% 2 == 0){
    next
  }
  print(i)
}



participants <- c("Paul", "Mary", "Ngure")

letter <- "Dear {participants},

It was a pleasure meeting and talking to you earlier today. 

As agreed, please send me an email with your CV.

My email address is written below.

Feel free to contact me for additional information or clarification.

Regards,
John Karuitha,
Lecturer in Finance,
Karatina University,
P.O. Box 1957 00100,
Karatina, Kenya.

+254 736 917 717
jkaruitha@karu.ac.ke

"


for(i in participants){
  print(glue(letter))
}

install.packages("tidytable")






