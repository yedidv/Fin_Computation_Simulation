library(tidyverse) 

Newton <- function(fun, df, x0, tol = 1e-6, maxN = 200){
  ## Check for convergence up front 
  
  if(abs(fun(x0)) < tot){
    message('Guessed correct, x0 = x*!') 
    return(x0) }
  
  for(i in 1:maxN){
    f0 <- fun(x0) 
    df0 <- df(x0 ) 
    x1 <- x0 - f0 / df0 
    if(abs(fun(x0)) < tot){
      message('Guessed correct, x0 = x*!') 
      return(x0) 
    }
    
    
  }
  print('max iterations reached') 
}



Fun1<- function(x){
  return(x^3 + 3 * x^2 - 2*x + 4) 
}
    
