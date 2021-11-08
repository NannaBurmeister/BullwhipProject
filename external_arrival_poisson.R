external_arrival_poisson <- function(N, lambda){
  interarrivals <- rexp(N, lambda)
  arrival <- cbind(1:N, cumsum(interarrivals))
  return(arrival)
}
