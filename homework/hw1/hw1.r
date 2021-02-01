# Vijay Yedidi- Homework 1 
setwd("~/Desktop/msba/spring2021/simulation_computation/homework/hw1")
## Call all the needed packages
source('funs.r')
library(tidyverse) 
library(reshape2) 

##### Question 2##### 
## Write a function that outputs the call and put option values given the inputs 

D_1 <- function(s_0, k, r, sigma, time){
  
  ## Function to find d_1
  
  # Breaking the formula up into smaller parts 
  a <- log(s_0 / k) 
  b <- time * (r + ((sigma ^2 ) / 2))
  c <- sigma * sqrt(time) 
  
  #Put the parts of the equation together 
  d_1 <- (a + b) / (c) 
  return(d_1) 
}

d_1 <- D_1(s_0 = 50, k = 50, r = 0.05, sigma = 0.4, time = 1) 
print(d_1) 

D_2 <- function(s_0, k, r, sigma, time){
  
  ## Function to find d_2 
  
  # Breaking up the formula up into smaller parts 
  a <- log(s_0 / k) 
  b <- time * (r - ((sigma ^2 ) / 2)) 
  c <- sigma * sqrt(time) 
  
  # Put the parts of the equation together 
  d_2 <- (a + b) / (c) 
  return(d_2) 
}

d_2 <- D_2(s_0 = 50, k = 50, r = 0.05, sigma = 0.4, time = 1) 

Call_Option <- function(k, s_0, d_1, r, time, d_2){
  
  ## Calculate the value of the call option 
  
  ## Breaking up the formula into smaller parts 
  a <- s_0 * pnorm(d_1)
  b <- k * exp(-r * time) * pnorm(d_2)
  c <- a - b
  return(c)     
}

print(Call_Option(k = 50, s_0 = 50, d_1 = d_1, r = 0.05, time = 1, d_2 = d_2)) 

Put_Option <- function(k, s_0, d_1, r, time, d_2){
  
  ## Calculate the value of the put option 
  
  ## Breaking up the formula into smaller parts 
  a <- s_0 * pnorm(-d_1) 
  b <- k * exp(-r * time) * pnorm(-d_2) 
  return( -a + b) 
}

print(Put_Option(k = 50, s_0 = 50, d_1 = d_1, r = 0.05, time = 1, d_2 = d_2) ) 

Black_Scholes_Merton <- function(k, s_0, r, time, sigma){
  ### Use the previously created functions to create the Black Scholes Model
  
  ## Find the values for d_1 and d_2 
  d_1 <- D_1(s_0, k, r, sigma, time)
  d_2 <- D_2(s_0, k, r, sigma, time)
  
  ## Find the values for the call and put options 
  c <- Call_Option(k, s_0, d_1, r, time, d_2) 
  p <- Put_Option(k, s_0, d_1, r, time, d_2) 
  
  ## Return the call and put option values as a list 
  return(list('call' = c, 
              'put' = p))
}

## Use the Black Scholes Merton formula 
q_1_values <- Black_Scholes_Merton(k = 50, s_0 = 50, r = 0.05, time = 1, sigma = 0.4)

paste0('The price of the call option is: ', q_1_values$call) 
paste0('The price of the put option is: ', q_1_values$put) 




