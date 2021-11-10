#Load functions and packages
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/mm1_queue.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/external_arrival_poisson.R")
library(tidyverse)

## Opennodek) Jackson Network

generate_leadtimes_supplier <- function(N, C, K, M, alpha, mu_I, mu_D1, mu_D2, mu_D3, mu_O) {
  
  ##Node I
  
  #Generate external poisson arrival time
  arrival <- external_arrival_poisson(N,alpha)
  
  #Now simulate M/M/1 queue at node I
  node_I <- simulate_mm1_queue(arrival, mu_I)
  
  ##Department 1
  #Generate interarrival
  arrival_D1 <- matrix(NA,N,3)
  for (i in 1:N){
    #Chosen at random
    k <- sample(1:C,1)
    arrival_D1[i,3] <- k
    arrival_D1[i,1] <- node_I$id[i]
    arrival_D1[i,2] <- node_I$leave_service_time[i]
  }
  
  arrival_D1 <- as.tibble(arrival_D1)
  
  #Now simulate M/M/1 queues at each node in department 1
  D1 <- map(1:6, function(k) {
    arrival_node_k <- arrival_D1 %>% filter(V3 == k) %>% select(-V3) %>% as.matrix()
    sim_output <- simulate_mm1_queue(arrival_node_k, mu_D1)
    sim_output <- cbind(sim_output$id, sim_output$leave_service_time)
    return(sim_output)
  })
  
  D1 <- do.call(rbind, D1)
  
  arrival_D2 <- D1[order(D1[,2]),]
  arrival_D2 <- cbind(arrival_D2,rep(0,N))
  
  for (i in 1:N){
    #Chosen at random
    k <- sample(1:K,1)
    arrival_D2[i,3] <- k
  }
  
  arrival_D2 <- as.tibble(arrival_D2)
  
  #Now simulate M/M/1 queues at each node in department 2
  D2 <- map(1:6, function(k) {
    arrival_node_k <- arrival_D2 %>% filter(V3 == k) %>% select(-V3) %>% as.matrix()
    sim_output <- simulate_mm1_queue(arrival_node_k, mu_D2)
    sim_output <- cbind(sim_output$id, sim_output$leave_service_time)
    return(sim_output)
  })
  
  D2 <- do.call(rbind, D2)
  arrival_D3 <- D2[order(D2[,2]),]
  arrival_D3 <- cbind(arrival_D3,rep(0,N))
  
  for (i in 1:N){
    #Chosen at random
    k <- sample(1:M,1)
    arrival_D3[i,3] <- k
  }
  
  arrival_D3 <- as.tibble(arrival_D3)
  
  #Now simulate M/M/1 queues at each node in department 2
  D3 <- map(1:6, function(k) {
    arrival_node_k <- arrival_D3 %>% filter(V3 == k) %>% select(-V3) %>% as.matrix()
    sim_output <- simulate_mm1_queue(arrival_node_k, mu_D3)
    sim_output <- cbind(sim_output$id, sim_output$leave_service_time)
    return(sim_output)
  })
  
  D3 <- do.call(rbind, D3)
  arrival_O <- D3[order(D3[,2]),]
  arrival_O <- arrival_O[order(arrival_O[,2]),]
  
  #Simulate
  node_O <- simulate_mm1_queue(arrival_O,mu_O)
  
  #Calculate sojourn times
  arrival_I <- data.frame(arrival)
  colnames(arrival_I) <- c("id", "arrival")
  leave_O <- data.frame(node_O$id, node_O$leave_service_time)
  colnames(leave_O) <- c("id", "leave_service_time")
  total_time <- inner_join(arrival_I,leave_O, by="id")
  sojourn_time <- total_time$leave_service_time - total_time$arrival
  total_time <- data.frame(total_time,sojourn_time)
  
  #Total sojourn time
  sojourn_time_sim <- mean(total_time$sojourn_time)
  
  #Theoretical sojourn time
  theoretical_sojourn_time <-  ((1/(mu_I-alpha)) 
                                + C*(1/C)*(1/(mu_D1-((1/C)*(alpha))))
                                + K*(1/K)*(1/(mu_D2-((1/K)*(alpha))))
                                + M*(1/M)*(1/(mu_D3-((1/M)*(alpha))))
                                + (1/(mu_O-alpha)))
  
  #Determine if system is stable
  if ((alpha/mu_I) < 1 & ((alpha/C)/mu_D1) < 1 & ((alpha/K)/mu_D2) < 1 & 
      ((alpha/M)/mu_D3) < 1 & (alpha/mu_O) < 1){
    stable <- "YES"
  } else {
    stable <- "NO"
  }
  
  return(list("total_time" = total_time, "sojourn_time_sim" = sojourn_time_sim, 
              "theoretical_sojourn_time" = theoretical_sojourn_time, "stable" = stable))
}

