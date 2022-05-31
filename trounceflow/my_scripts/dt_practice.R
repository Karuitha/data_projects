library(data.table)
library(tidyverse)

x <- data.table(
        
        z = sample(LETTERS[1:3], 10, replace = TRUE),
        
        w = sample(1:5, 10, replace = TRUE)
)

x


x[z > 'A', list(mean_w = mean(w)), by = z]


##Data for joins 
set.seed(42, sample.kind = "Rounding")

big <- data.table( 
        id = LETTERS[2:11],
        a = sample(101:105, 10, replace = TRUE),
        b = sample(200:300, 10)
)


small <- data.table(
        id = LETTERS[1:5],
        y = sample(1:5, 5, replace = TRUE),
        z = sample(10:20, 5) 
)


head(big)
head(small)


big[small, on = list(id)]
small[big, on = .(id)]
small[big, on = .(id), nomatch = 0]





data(mtcars)
head(mtcars)
mtcars <- mtcars %>% as.data.table()
system.time(mtcars %>% 
                    
                    filter(gear == 4) %>% 
                    
                    summarise(mean_mpg = mean(mpg)))


system.time(mtcars[gear == 4, .(mean_mpg = mean(mpg))])
