external_arrival_poisson <- function(N, lambda){
  arrival <- cbind(1:N, cumsum(rexp(N, lambda)))
  return(arrival)
}
