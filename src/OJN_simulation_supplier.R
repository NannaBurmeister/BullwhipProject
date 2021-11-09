


#Load functions and packages
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/mm1_queue.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/external_arrival_poisson.R")
library(tidyverse)

## Open Jackson Network

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
  
  #Split into vectors
  arrival_D1_1 <- arrival_D1 %>% filter(V3 == 1) %>% select(-V3) %>% as.matrix()
  arrival_D1_2 <- arrival_D1 %>% filter(V3 == 2) %>% select(-V3) %>% as.matrix()
  arrival_D1_3 <- arrival_D1 %>% filter(V3 == 3) %>% select(-V3) %>% as.matrix()
  arrival_D1_4 <- arrival_D1 %>% filter(V3 == 4) %>% select(-V3) %>% as.matrix()
  arrival_D1_5 <- arrival_D1 %>% filter(V3 == 5) %>% select(-V3) %>% as.matrix()
  arrival_D1_6 <- arrival_D1 %>% filter(V3 == 6) %>% select(-V3) %>% as.matrix()
  
  
  #Now simulate M/M/1 queues at each node in department 1
  D1_1 <- simulate_mm1_queue(arrival_D1_1, mu_D1)
  D1_2 <- simulate_mm1_queue(arrival_D1_2, mu_D1)
  D1_3 <- simulate_mm1_queue(arrival_D1_3, mu_D1)
  D1_4 <- simulate_mm1_queue(arrival_D1_4, mu_D1)
  D1_5 <- simulate_mm1_queue(arrival_D1_5, mu_D1)
  D1_6 <- simulate_mm1_queue(arrival_D1_6, mu_D1)
  
  
  ##Department 2
  #Vector with id and interarrival for D2
  D1_1 <- cbind(D1_1$id, D1_1$leave_service_time)
  D1_2 <- cbind(D1_2$id, D1_2$leave_service_time)
  D1_3 <- cbind(D1_3$id, D1_3$leave_service_time)
  D1_4 <- cbind(D1_4$id, D1_4$leave_service_time)
  D1_5 <- cbind(D1_5$id, D1_5$leave_service_time)
  D1_6 <- cbind(D1_6$id, D1_6$leave_service_time)
  
  arrival_D2 <- rbind(D1_1, D1_2, D1_3, D1_4, D1_5, D1_6)
  arrival_D2 <- arrival_D2[order(arrival_D2[,2]),]
  arrival_D2 <- cbind(arrival_D2,rep(0,N))
  
  for (i in 1:N){
    #Chosen at random
    k <- sample(1:K,1)
    arrival_D2[i,3] <- k
  }
  
  arrival_D2 <- as.tibble(arrival_D2)
  
  #Split into vectors
  arrival_D2_1 <- arrival_D2 %>% filter(V3 == 1) %>% select(-V3) %>% as.matrix()
  arrival_D2_2 <- arrival_D2 %>% filter(V3 == 2) %>% select(-V3) %>% as.matrix()
  arrival_D2_3 <- arrival_D2 %>% filter(V3 == 3) %>% select(-V3) %>% as.matrix()
  arrival_D2_4 <- arrival_D2 %>% filter(V3 == 4) %>% select(-V3) %>% as.matrix()
  arrival_D2_5 <- arrival_D2 %>% filter(V3 == 5) %>% select(-V3) %>% as.matrix()
  arrival_D2_6 <- arrival_D2 %>% filter(V3 == 6) %>% select(-V3) %>% as.matrix()
  
  
  #Now simulate M/M/1 queues at each node in department 1
  D2_1 <- simulate_mm1_queue(arrival_D2_1, mu_D2)
  D2_2 <- simulate_mm1_queue(arrival_D2_2, mu_D2)
  D2_3 <- simulate_mm1_queue(arrival_D2_3, mu_D2)
  D2_4 <- simulate_mm1_queue(arrival_D2_4, mu_D2)
  D2_5 <- simulate_mm1_queue(arrival_D2_5, mu_D2)
  D2_6 <- simulate_mm1_queue(arrival_D2_6, mu_D2)
  
  
  ##Department 3
  #Vector with id and interarrival for D3
  D2_1 <- cbind(D2_1$id, D2_1$leave_service_time)
  D2_2 <- cbind(D2_2$id, D2_2$leave_service_time)
  D2_3 <- cbind(D2_3$id, D2_3$leave_service_time)
  D2_4 <- cbind(D2_4$id, D2_4$leave_service_time)
  D2_5 <- cbind(D2_5$id, D2_5$leave_service_time)
  D2_6 <- cbind(D2_6$id, D2_6$leave_service_time)
  
  arrival_D3 <- rbind(D2_1, D2_2, D2_3, D2_4, D2_5, D2_6)
  arrival_D3 <- arrival_D3[order(arrival_D3[,2]),]
  arrival_D3 <- cbind(arrival_D3,rep(0,N))
  
  for (i in 1:N){
    #Chosen at random
    k <- sample(1:M,1)
    arrival_D3[i,3] <- k
  }
  
  arrival_D3 <- as.tibble(arrival_D3)
  
  #Split into vectors
  arrival_D3_1 <- arrival_D3 %>% filter(V3 == 1) %>% select(-V3) %>% as.matrix()
  arrival_D3_2 <- arrival_D3 %>% filter(V3 == 2) %>% select(-V3) %>% as.matrix()
  arrival_D3_3 <- arrival_D3 %>% filter(V3 == 3) %>% select(-V3) %>% as.matrix()
  arrival_D3_4 <- arrival_D3 %>% filter(V3 == 4) %>% select(-V3) %>% as.matrix()
  arrival_D3_5 <- arrival_D3 %>% filter(V3 == 5) %>% select(-V3) %>% as.matrix()
  arrival_D3_6 <- arrival_D3 %>% filter(V3 == 6) %>% select(-V3) %>% as.matrix()
  
  
  #Now simulate M/M/1 queues at each node in department 1
  D3_1 <- simulate_mm1_queue(arrival_D3_1, mu_D3)
  D3_2 <- simulate_mm1_queue(arrival_D3_2, mu_D3)
  D3_3 <- simulate_mm1_queue(arrival_D3_3, mu_D3)
  D3_4 <- simulate_mm1_queue(arrival_D3_4, mu_D3)
  D3_5 <- simulate_mm1_queue(arrival_D3_5, mu_D3)
  D3_6 <- simulate_mm1_queue(arrival_D3_6, mu_D3)
  
  ##Output node node
  #Vector with id and interarrival for output node
  D3_1 <- cbind(D3_1$id, D3_1$leave_service_time)
  D3_2 <- cbind(D3_2$id, D3_2$leave_service_time)
  D3_3 <- cbind(D3_3$id, D3_3$leave_service_time)
  D3_4 <- cbind(D3_4$id, D3_4$leave_service_time)
  D3_5 <- cbind(D3_5$id, D3_5$leave_service_time)
  D3_6 <- cbind(D3_6$id, D3_6$leave_service_time)
  
  arrival_O <- rbind(D3_1, D3_2, D3_3, D3_4, D3_5, D3_6)
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

