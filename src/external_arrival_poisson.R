#External arrival of jobs following a Poisson process
external_arrival_poisson <- function(N, lambda){
  #Interarrivals are exponential
  interarrivals <- rexp(N, lambda)
  arrival <- cbind(1:N, cumsum(interarrivals))
  return(arrival)
}
#Output is matrix. First row id, second row arrival time.