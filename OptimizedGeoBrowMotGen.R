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
  Matrix of floats
  
  Index | t/Simulation | Sim1 ... Sim<numSims> +1
    0
    .
    .
    .
<DTintervals +1>

first row is the headers
second row is starting value
remaining rows are the asset prices for each time point specified
t/Simulation column is the value of dt for that row
Sim<numSims> is the simulation number
  "
  dt <- 1/DTintervals#not sure if this is the correct way to do this
  
  ##create a matrix of brownian motion values and calculate the asset price##
  #create matrix of initial values the same size as the random val matrix
  initialValMatrix <- matrix(S, nrow = DTintervals, ncol = numSims) 
  #create of random brownian motion numbers(we must use DTintervals*numSims, else matrix will reuse values)
  gbmVals <- matrix( rnorm(DTintervals*numSims, dt*(mu - 0.5*sig^2), sig*sqrt(dt)), nrow = DTintervals, ncol = numSims) 
  #take the cumulative sum of the random numbers for each col in the random num matrix (to avoid looping and mulitplying on the prev value)
  gbmVals <- apply(gbmVals, 2, cumsum)
  #raise the matrix of initial values to the matrix of random vals to get the price for each period
  sims <- initialValMatrix *exp(gbmVals)
  
  #add the row of initial values
  inVals <- c(rep(S, numSims))
  sims <- rbind(inVals,sims )
  
  #create the t val x axis (can be omitted for pure simulation)
    intervals <- c(0:DTintervals)
    ones <- c(rep(1, DTintervals + 1))
    dtQuantiles <- intervals * ones
    sims<-cbind(dtQuantiles, sims)
  #create the header names (can be omitted for pure simulation)
    colnames(sims) <- c("t / Simulation", paste("Sim", 1:numSims, sep=""))
  
  return(sims)
}


#optimized 2#  ##use this function its shaves another second##

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
   
   gbmVals <- matrix( rnorm(DTintervals*numSims, dt*(mu - 0.5*sig^2), sig*sqrt(dt)), nrow = DTintervals, ncol = numSims) #takes the most time
   gbmVals <- apply(gbmVals, 2, cumsum)
   sims <- (matrix(S, nrow = DTintervals, ncol = numSims)) *exp(gbmVals)
   
   #add the row of initial values
   sims <- rbind(c(rep(S, numSims)),sims )
   
   #create the t x axis
   sims <- cbind(c(0:DTintervals) * c(rep(1, DTintervals + 1)), sims)
   
   #create the x axis names
   colnames(sims) <- c("t / Simulation", paste("Sim", 1:numSims, sep=""))
   return(sims)
}


#optimized 3 ##use this if you dont need lables##

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
  Matrix
  
the first row is just the starting val
each col is the individual simulation
  "
   dt <- 1/DTintervals#not sure if this is the correct way to do this
   gbmVals <- matrix( rnorm(DTintervals*numSims, dt*(mu - 0.5*sig^2), sig*sqrt(dt)), nrow = DTintervals, ncol = numSims) 
   gbmVals <- apply(gbmVals, 2, cumsum)
   sims <- (matrix(S, nrow = DTintervals, ncol = numSims)) *exp(gbmVals)
   #add the row of initial values
   sims <- rbind(c(rep(S, numSims)),sims )
   return(sims)
}

#speed test
for (i in 1:10){
  ptm <- proc.time()
  test <- simGBM(DTintervals = 52, numSims = 1000000)
  print(proc.time() - ptm)
}
print('done')

