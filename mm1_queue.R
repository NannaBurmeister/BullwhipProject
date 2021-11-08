## M/M/1 queue

simulate_mm1_queue <- function(arrival, mu) {

  
  number_of_arrivals <- length(arrival[,1])
  
  service <- rexp(number_of_arrivals, mu)
  
  #Time spend in node
  enter_service_time <- rep(number_of_arrivals, 0)
  leave_service_time <- rep(number_of_arrivals, 0)
  total_time <- rep(number_of_arrivals, 0)
  
  for (i in 1:number_of_arrivals){
    if (i == 1 || leave_service_time[i-1] < arrival[i,2]){
      #first job
      enter_service_time[i] <- arrival[i,2]
    } else {
      #queue
      enter_service_time[i] <- leave_service_time[i-1]
    }
    leave_service_time[i] <- enter_service_time[i] + service[i]
    total_time[i] <- leave_service_time[i] - arrival[i,2]
  }
  return(
    list("id" = arrival[,1],
         "total_time" = total_time,
         "enter_service_time" = enter_service_time,
         "leave_service_time" = leave_service_time
    )
  )
}

#Verify simulation
#arrivals <- cbind(1:1000000, cumsum(rexp(1000000, 0.4)))
#output <- simulate_mm1_queue(arrivals, 0.5)
#sojourn_time_sim <- mean(output$total_time)
#sojourn_time_theoritical <- (1/(mu_I-alpha))
#x <- 1:N

#plot(x, cumsum(output$total_time)/x, type = "l", lty = 1)
#abline(h=sojourn_time_theoritical, col="red")
