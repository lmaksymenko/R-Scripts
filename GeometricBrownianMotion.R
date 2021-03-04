
simGBM <- function(S = 100, DTintervals = 252, mu = 0.1, sig = 0.2, numSims = 1){
  """
  Generates a Geometric Brownian Motion
  
  Inputs:
  S = starting value
  DTintervals = the number of intervals to generate the bm
  mu = mu variable
  sig = sigma variable
  numSims = number of simulations to run
  
  Returns:
  A dataframe
  
  Index | t/Simulation | Sim1 ... Sim<numSims> +1
    0
    .
    .
    .
<DTintervals +1>

the first row is just the starting val
the t/Simulation column is the value of dt for that row
Sim<numSims> is the simulation number
  """
  dt <- 1/DTintervals#not sure if this is the correct way to do this
  
  dtIndex <- c(0)
  simColNames <- paste("Sim", 1:numSims, sep="")
  
  for (i in 1:DTintervals){
    dtIndex <- c(dtIndex, i/DTintervals)
  }
  
  simulations <- as.data.frame(matrix(0, ncol = numSims + 1, nrow = DTintervals + 1))
  simulations[, 1] <- dtIndex
  names(simulations) <-c("t / Simulation", simColNames) 
  
  for (j in 1:numSims + 1){
    sim <- c(S)
    for(i in 1:DTintervals) {
      dR <- rnorm(1,dt*(mu - 0.5*sig^2), sig*sqrt(dt) )
      S_dt <- sim[i]*exp(dR)
      sim <- c(sim,S_dt) 
    }
   simulations[,j] <- sim
  }
  return(simulations)
}


#test <- simGBM(DTintervals = 52, numSims = 50)
#test
