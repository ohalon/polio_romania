### SIR POLIO MODEL

library(deSolve)
library(ggplot2)

# SIR MODEL

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta*I*S
    dI <- beta*I*S - gamma*I
    dR <- gamma*I
    return(list(c(dS,dI,dR)))
  })
}

# PARAMETERS

parameters <- c(
    beta = 0.000337,
    gamma = 1/28
    )


# INITIAL CONDITIONS

start_conditions <- c(S = 500 , I = 1, R = 9500)
time_interval <- seq(0,1000)

# RUN MODEL

values <- ode(
    y = start_conditions,
    times = time_interval,
    func = sir_equations,
    parms = parameters
    
)

# CHANGE TO DATAFRAME 

values <- as.data.frame(values)

# PLOT

with(values, {
  plot(time, S, type = "l", col = "blue", lwd = 2, # plot susceptibles
       xlab = "Timp (zile)",
       ylab = "Numar de indivizi",
       #ylim = c(0,10000),
       main = "Evolutia epidemiei - acoperire 95%")
  lines(time, I, col = "red", lwd = 2) # plot infected
  lines(time, R, col = "green", lwd = 2) # plot recovered
})

legend("right", c("susceptibili", "infectati", "vindecati"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")