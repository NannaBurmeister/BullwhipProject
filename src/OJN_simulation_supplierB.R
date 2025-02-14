#Load functions and packages
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/mm1_queue.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/external_arrival_poisson.R")
library(tidyverse)

## Jackson Network

generate_leadtimes_supplierB <- function(N, C, K, M, alpha, mu_I, mu_D1, mu_D2, mu_D3, mu_O, pIO){
  
  ##Node I
  
  #Generate external poisson arrival time
  arrival <- external_arrival_poisson(N,alpha)
  
  
  #Now simulate M/M/1 queue at node I
  node_I <- simulate_mm1_queue(arrival, mu_I)
  
  
  ##Department 1
  #Generate interarrival
  arrival_D1 <- matrix(NA,N,3)
  arrival_O_temp <- matrix(NA,N,2)
  for (i in 1:N){
    #Chosen at random
    p <- runif(1)
    if (p > pIO) {
      k <- sample(1:C,1)
      arrival_D1[i,3] <- k
      arrival_D1[i,1] <- node_I$id[i]
      arrival_D1[i,2] <- node_I$leave_service_time[i] 
    }
    else {
      arrival_O_temp[i,1] <- node_I$id[i]
      arrival_O_temp[i,2] <- node_I$leave_service_time[i]
    }
  }
  
  #Remove rows with NA
  arrival_D1 <- arrival_D1[complete.cases(arrival_D1),] 
  arrival_O_temp <- arrival_O_temp[complete.cases(arrival_O_temp),]
  
  arrival_D1 <- as.tibble(arrival_D1)
  
  D1 <- map(1:C, function(k) {
    arrival_node_k <- arrival_D1 %>% filter(V3 == k) %>% select(-V3) %>% as.matrix()
    sim_output <- simulate_mm1_queue(arrival_node_k, mu_D1)
    sim_output <- cbind(sim_output$id, sim_output$leave_service_time, sim_output$total_time)
    return(sim_output)
  })
  
  D1 <- do.call(rbind, D1)
  colnames(D1) <- c("id","leave_service_time","total_time")
  
  arrival_D2 <- D1[order(D1[,2]),]
  arrival_D2 <- cbind(arrival_D2,rep(0,length(arrival_D2[,2])))
  
  for (i in 1:length(arrival_D2[,2])){
    #Chosen at random
    k <- sample(1:K,1)
    arrival_D2[i,4] <- k
  }
  
  arrival_D2 <- as.tibble(arrival_D2)
  
  #Now simulate M/M/1 queues at each node in department 2
  D2 <- map(1:K, function(k) {
    arrival_node_k <- arrival_D2 %>% filter(V4 == k) %>% select(-V4) %>% as.matrix()
    sim_output <- simulate_mm1_queue(arrival_node_k, mu_D2)
    sim_output <- cbind(sim_output$id, sim_output$leave_service_time, sim_output$total_time)
    return(sim_output)
  })
  
  D2 <- do.call(rbind, D2)
  colnames(D2) <- c("id","leave_service_time","total_time")
  
  arrival_D3 <- D2[order(D2[,2]),]
  arrival_D3 <- cbind(arrival_D3,rep(0,length(arrival_D3[,2])))
  
  for (i in 1:length(arrival_D3[,2])){
    #Chosen at random
    k <- sample(1:M,1)
    arrival_D3[i,4] <- k
  }
  
  arrival_D3 <- as.tibble(arrival_D3)
  
  #Now simulate M/M/1 queues at each node in department 2
  D3 <- map(1:M, function(k) {
    arrival_node_k <- arrival_D3 %>% filter(V4 == k) %>% select(-V4) %>% as.matrix()
    sim_output <- simulate_mm1_queue(arrival_node_k, mu_D3)
    sim_output <- cbind(sim_output$id, sim_output$leave_service_time, sim_output$total_time)
    return(sim_output)
  })
  
  D3 <- do.call(rbind, D3)
  colnames(D3) <- c("id","leave_service_time","total_time")
  
  temp <- as_tibble(D3) %>% select(-total_time)
  colnames(arrival_O_temp) <- c("id","leave_service_time")
  
  arrival_O <- as.matrix(rbind(temp, arrival_O_temp))
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
  theoretical_sojourn_time <- ((1/(mu_I-alpha)) 
                               + C*((1-pIO)/C)*(1/(mu_D1-(((1-pIO)/C)*(alpha))))
                               + K*((1-pIO)/K)*(1/(mu_D2-(((1-pIO)/K)*(alpha))))
                               + M*((1-pIO)/M)*(1/(mu_D3-(((1-pIO)/M)*(alpha))))
                               + (1/(mu_O-alpha)))
  
  #Determine if system is stable
  if ((alpha/mu_I) < 1 & (1-pIO)*((alpha/C)/mu_D1) < 1 & (1-pIO)*((alpha/K)/mu_D2) < 1 & 
      (1-pIO)*((alpha/M)/mu_D3) < 1 & (alpha/mu_O) < 1){
    stable <- "YES"
  } else {
    stable <- "NO"
  }
  
  node_I <- data.frame(node_I$id, node_I$total_time)
  colnames(node_I) <- c("id", "total_time")
  D1 <- data.frame(D1[,1], D1[,3])
  colnames(D1) <- c("id", "total_time")
  D2 <- data.frame(D2[,1], D2[,3])
  colnames(D2) <- c("id", "total_time")
  D3 <- data.frame(D3[,1], D3[,3])
  colnames(D3) <- c("id", "total_time")
  node_O <- data.frame(node_O$id, node_O$total_time)
  colnames(node_O) <- c("id", "total_time")
    
  node_sojourn_time <- left_join(node_I, D1, by="id")
  node_sojourn_time <- left_join(node_sojourn_time, D2, by="id")
  node_sojourn_time <- left_join(node_sojourn_time, D3, by="id")
  node_sojourn_time <- left_join(node_sojourn_time, node_O, by="id")
  
  node_sojourn_time <- arrange(node_sojourn_time, "id")
  
  return(list("total_time" = total_time, "sojourn_time_sim" = sojourn_time_sim, 
              "theoretical_sojourn_time" = theoretical_sojourn_time, "stable" = stable,
              "node_sojourn_time"=node_sojourn_time))
}
