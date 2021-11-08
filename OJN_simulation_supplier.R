source("mm1_queue.R")
library(tidyverse)

## Jackson Network
set.seed(1) # Set seed for reproducibility
N <- 10000  # number of events to simulate;
C <- 6          # number of nodes Department 1
K <- 6          # number of nodes Department 2
M <- 6          # number of nodes Department 3
alpha <- 0.1    # external arrival rate
mu_I <- 0.2       # Input node (I) service rate 
mu_D1 <- 0.1      # Deparment 1 (D_i^1) service rate
mu_D2 <- 0.2      # Deparment 1 (D_i^1) service rate
mu_D3 <- 0.3      # Deparment 1 (D_i^1) service rate
mu_O <- 0.5       # Output node (O) service rate

##Node I

#Generate external poisson arrival time
arrival_time <- function(lambda) {
  temp <- rexp(1,lambda)
  return(temp)
}

#Vector of interarrival times
arrival <- rep(N, 0)
for (i in 1:N){
  if (i==1) {
    arrival[i] <- arrival_time(alpha)
  } 
  else {
    arrival[i] <- arrival[i-1] + arrival_time(alpha)
  }
}
id <- 1:N
arrival <- cbind(id, arrival)


#Now simulate M/M/1 queue at node I
node_I <- simulate_mm1_queue(arrival, mu_I)
mean_I <- mean(node_I$total_time)

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

#Means
mean_D1 <- (mean(D1_1$total_time) + mean(D1_2$total_time) + mean(D1_3$total_time)
            + mean(D1_4$total_time) + mean(D1_5$total_time) + mean(D1_6$total_time))/C

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

#Means
mean_D2 <- (mean(D2_1$total_time) + mean(D2_2$total_time) + mean(D2_3$total_time)
            + mean(D2_4$total_time) + mean(D2_5$total_time) + mean(D2_6$total_time))/K

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

#Means
mean_D3 <- (mean(D3_1$total_time) + mean(D3_2$total_time) + mean(D3_3$total_time)
          + mean(D3_4$total_time) + mean(D3_5$total_time) + mean(D3_6$total_time))/M

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

#Mean
mean_O <- mean(node_O$total_time)

#Calculate sojourn times
arrival_I <- data.frame(arrival)
leave_O <- data.frame(node_O$id, node_O$leave_service_time)
colnames(leave_O) <- c("id", "leave_service_time")
total_time <- inner_join(arrival_I,leave_O, by="id")
sojourn_time <- total_time$leave_service_time - total_time$arrival
total_time <- data.frame(total_time,sojourn_time)

#Total sojourn time
sojourn_time_sim <- mean(total_time$sojourn_time)

#Plot cummulative mean
theoretical_sojourn_time <- ((1/alpha)*((alpha/mu_I)/(1-(alpha/mu_I)))
                          + C*(1/(alpha))*(((alpha/C)/mu_D1)/(1-((alpha/C)/mu_D1)))
                          + K*(1/(alpha))*(((alpha/C)/mu_D2)/(1-((alpha/C)/mu_D2)))
                          + C*(1/(alpha))*(((alpha/C)/mu_D3)/(1-((alpha/C)/mu_D3)))
                          + (1/(alpha))*(((alpha)/mu_O)/(1-((alpha)/mu_O))))
x <- 1:N
plot(x, cumsum(total_time$sojourn_time)/x, type = "l", lty = 1)
abline(h = theoretical_sojourn_time, col="red")

