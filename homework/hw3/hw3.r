library(Matrix) 
library(tidyverse) 

## Name the function
Fun1<- function(x){
  return(x^3 + 3 * x^2 - 2*x + 4) 
}

## Define the x variables 
x <- seq(-4, 1, length = 500) 



## Bisection Method Function
Bisection <- function(fun, ## Call the f(x) functions
                      
                      a, ## left point for bisection
                      
                      b, ## right point for bisection
                      
                      tol = 1e-6, ## tolerance between a and b 
                      
                      maxN = 200 ## we want to have a max on iterations
                          ## if no solution we don't want a
                          ## data leak
                      ){
  ## Check and make sure x* is between a and b 
  fa <- fun(a) 
  fb <- fun(b) 
  if (fa * fb > 0){
    stop('The solution is not between the bounds [a,b]. 
         Widen the search')
  }
  
  ## Check if c = x* 
  c <- (a + b) / 2
  fc <- fun(c) 
  if ( abs(fc) < tol | abs(a - b) < tol){
    return(c)
  }
  
  ## Main loop 
  for( i in 1:maxN){ ## We don't want the loop to be infinite
    if(fc * fa < 0){
      ## Left side
      b <- c
      fb <- fc
    }
    else{
      ## Right side
      a <- c 
      fa <- fc
    }
    if ( abs(fc) < tol | abs(a - b) < tol){
      message(sprintf('Solution achieved in %d iterations', i))
      message(sprintf('\tx = %5.4f', c)) 
      message(sprintf('\tf(x*) = %5.4f', fc)) 
      return(c)
    }
    c <- (a + b) / 2 
    fc <- fun(c) 
  }
  message(print('No Solution found')) 
  
}

Bisection(Fun1, min(x), max(x)) 

answers <- tibble(x = x,
                  f_x = Fun1(x)) 
ggplot(answers, aes(x = x, y = f_x)) + geom_line() 
