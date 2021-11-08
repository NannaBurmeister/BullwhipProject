## M/M/1 queue

simulate_mm1_queue <- function(arrival, mu) {
  #Service time
  service_time <- function(mu) {
    temp <- rexp(1,mu)
    return(temp)
  }
  
  service <- rep(length(arrival[,1]), 0)
  for (i in 1:length(arrival[,1])){
    service[i] <- service_time(mu)
  }
  
  #Time spend in node
  enter_service_time <- matrix(0,length(arrival[,1]), 2)
  leave_service_time <- rep(length(arrival[,1]), 0)
  total_time <- rep(length(arrival[,1]), 0)
  for (i in 1:length(arrival[,1])){
    if (i==1){
      #first job
      enter_service_time[1,] <- arrival[1,]
      leave_service_time[1] <- enter_service_time[1,2] + service[1]
    } else if (leave_service_time[i-1] < arrival[i,2]){
      #no queue
      enter_service_time[i,] <- arrival[i,]
      leave_service_time[i] <- enter_service_time[i,2] + service[i]
    } else {
      #queue
      enter_service_time[i,2] <- leave_service_time[i-1]
      enter_service_time[i,1] <- arrival[i,1]
      leave_service_time[i] <- enter_service_time[i,2] + service[i]
    }
    total_time[i]<- leave_service_time[i]-arrival[i,2]
  }
  return(
    list("id" = enter_service_time[,1],
         "total_time" = total_time,
         "enter_service_time" = enter_service_time[,2],
         "leave_service_time" = leave_service_time
    )
  )
}

#Verify simulation
#sojourn_time_sim <- mean(output$total_time)
#sojourn_time_theoritical <- (1/(mu_I-alpha))
#x <- 1:N

#plot(x, cumsum(output$total_time)/x, type = "l", lty = 1)
#abline(h=sojourn_time_theoritical, col="red")
