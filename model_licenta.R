### MODELING POTENTIAL IMPORTED POLIO CASES ###

## MODEL FOR PREVALENCE 

# p0 = prevalence the previous year t-1
# miu = 1/average life expectancy
# gamma = 1/average duration of infection
# LAMBDA = reported number of cases in year t
# S = number of susceptible individuals in year t
# r = ratio of asymptomatic to symptomatic cases

gamma <- 1/(28/365) # 4 weeks duration of illness
r <-  200

imported_cases <- function(c0, miu, LAMBDA,population, coverage, travelers) {

# PREVALENCE IN TRAVELERS
  
  S <- population*(1-coverage) # percentage of susceptible individuals
  p0 <- c0/S
  p <- p0*exp(-(miu+gamma)) + r*LAMBDA*((1-exp(-(miu+gamma)))/(S*(miu+gamma)))
  
  
# EXPECTED POLIO CASES IN TRAVELERS
 
  cases <- p*(1 - coverage)*travelers
  
  return(p)
    
}

