#Sample size calculation


# sample size calculation should be based on survey question
# If stratified sampling is desired calcutate the sample size for each strata and sum them up
# Alternatively proportionate the total sample size by strata sizes

# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","openxlsx"))
sample_size <- function(t,p,d,N){
  
  #t is the critical value from t table for infinite degrees of freedom(i.e population size)
  #P is the proportion of individuals in the category C
  #Q is the proportion of individuals in the category C"
  #d is the margin of error
  q = 1-p
  
  n_null = ((t^2)*(p*q))/(d^2)
  
  samp_frac = n_null/N
  
  if(N<8000 & samp_frac > 0.05){
    #finite population correction
    #Cochran (1977) recommends that if the population size is smaller than 8000
    #and sampling fraction(n/N) is larger than .05 finite population correction should be applied to sample size calculation
    n = n_null/(1+((n_null-1)/N))
    
    return(n)
  }else{
    return(n_null)
  }
  
  
    

}

sample_size(t = 1.96,p = .5,d = .04,N = 42000)
