#optimized geometric brownian motion generator


simGBM <- function(S = 100, DTintervals = 252, mu = 0.1, sig = 0.2, numSims = 1){
  "
  Generates a Geometric Brownian Motion
  
  Inputs:
  S = starting value
  DTintervals = the number of intervals to generate the bm
  mu = mu variable
  sig = sigma variable
  numSims = number of simulations to run
  
  Returns:
  A matrix
  
  Index | t/Simulation | Sim1 ... Sim<numSims> +1
    0
    .
    .
    .
<DTintervals +1>

the first row is just the starting val
the t/Simulation column is the value of dt for that row
Sim<numSims> is the simulation number
  "
  dt <- 1/DTintervals#not sure if this is the correct way to do this
  
  #create a matrix of brownian motion values and calculate the asset price
  initialValMatrix <- matrix(S, nrow = DTintervals, ncol = numSims)
  gbmVals <- matrix( rnorm(DTintervals*numSims, dt*(mu - 0.5*sig^2), sig*sqrt(dt)), nrow = DTintervals, ncol = numSims) 
  gbmVals <- apply(gbmVals, 2, cumsum)
  sims <- initialValMatrix *exp(gbmVals)
  
  #add the row of initial values
  inVals <- c(rep(S, numSims))
  sims <- rbind(inVals,sims )
  
  #create the t x axis
  intervals <- c(0:DTintervals)
  ones <- c(rep(1, DTintervals + 1))
  dtQuantiles <- intervals * ones
  sims<-cbind(dtQuantiles, sims)
 
  #create the x axis names
  colnames(sims) <- c("t / Simulation", paste("Sim", 1:numSims, sep=""))
  return(sims)
}


for (i in 1:10){
  ptm <- proc.time()
  test <- simGBM(DTintervals = 52, numSims = 1000000) # elapsed 205.90
  print(proc.time() - ptm)
}
print('done')

