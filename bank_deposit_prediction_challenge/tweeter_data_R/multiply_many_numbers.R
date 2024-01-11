

multiply_many_numbers <- function(numbers) {
    
    total = 1 
    
    for(number in numbers){
        
        total = total * number
    }
    
    return(total)
}

multiply_many_numbers(c(1, 2, 3))
multiply_many_numbers(c(3, 8, 12, 18))
multiply_many_numbers(1:10)
multiply_many_numbers(seq(10, 100, by = 2))
