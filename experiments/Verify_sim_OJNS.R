library(tseries)
library(purrr)
library(tidyverse)
library(moments)
library(gridExtra)
#Parameters
N <- 365*10  # number of events to simulate;
C <- 6          # number of nodes Department 1
K <- 6          # number of nodes Department 2
M <- 6          # number of nodes Department 3
alpha <- 0.4    # external arrival rate
mu_I <- 1       # Input node (I) service rate 
mu_D1 <- 0.5     # Deparment 1 (D_i^1) service rate
mu_D2 <- 0.75      # Deparment 1 (D_i^1) service rate
mu_D3 <- 1      # Deparment 1 (D_i^1) service rate
mu_O <- 2      # Output node (O) service rate


#Arrivals
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/external_arrival_poisson.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/OJN_simulation_supplierA.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/OJN_simulation_supplierB.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/Inventory_model_retailer.R")

##Base model

#Run generator
supplierA_base <- generate_leadtimes_supplier(N, C, K, M, alpha, mu_I, mu_D1, mu_D2, mu_D3, mu_O)

#Convergence plot
#expanded mean
plot_baseA <- as.data.frame(cbind(1:N,supplierA_base$total_time$sojourn_time)) %>% mutate(moving_average = cumsum(V2)/V1) %>% ggplot(aes(x=V1, y=moving_average)) + 
  geom_line() + geom_hline(yintercept = supplierA_base$theoretical_sojourn_time, color = "red") +
  ggtitle("Stability plot supplier A") + xlab("Time in days") + ylab("Mean sojourn time")

#Mean of dist 
mean(supplierA_base$total_time$sojourn_time)

#Variance of dist
var(supplierA_base$total_time$sojourn_time)

#Kurtosis
kurtosis(supplierA_base$total_time$sojourn_time)

#Skewness
skewness(supplierA_base$total_time$sojourn_time)

#Convergence
i=1
while (sum((sum(supplierA_base$total_time$sojourn_time[1:i])/i)/(sum(supplierA_base$total_time$sojourn_time[1:(i+1)])/(i+1)) > 1.01 ) 
       | ((sum(supplierA_base$total_time$sojourn_time[1:i])/i)/(sum(supplierA_base$total_time$sojourn_time[1:(i+1)])/(i+1)) < 0.99) > 0){
  i = i+1
}

#Convergence bullwhip



alpha <- 0.4    # external arrival rate
mu_I <- 1      # Input node (I) service rate 
mu_D1 <- 0.5     # Deparment 1 (D_i^1) service rate
mu_D2 <- 0.75      # Deparment 1 (D_i^1) service rate
mu_D3 <- 1     # Deparment 1 (D_i^1) service rate
mu_O <- 0.6     # Output node (O) service rate



##Base model
pIO <- 0.9

#Run generator
supplierB_base <- generate_leadtimes_supplierB(N, C, K, M, alpha, mu_I, mu_D1, mu_D2, mu_D3, mu_O, pIO)

#Convergence plot
#expanded mean
plot_baseB <- as.data.frame(cbind(1:N,supplierB_base$total_time$sojourn_time)) %>% mutate(moving_average = cumsum(V2)/V1) %>% ggplot(aes(x=V1, y=moving_average)) + 
  geom_line() + geom_hline(yintercept = supplierB_base$theoretical_sojourn_time, color = "red") +
  ggtitle("Stability plot supplier B") + xlab("Time in days") + ylab("")


#Mean of dist 
mean(supplierB_base$total_time$sojourn_time)

#Variance of dist
var(supplierB_base$total_time$sojourn_time)

#Kurtosis
kurtosis(supplierB_base$total_time$sojourn_time)

#Skewness
skewness(supplierB_base$total_time$sojourn_time)

i=1
while (sum((sum(supplierB_base$total_time$sojourn_time[1:i])/i)/(sum(supplierB_base$total_time$sojourn_time[1:(i+1)])/(i+1)) > 1.01 ) 
       | ((sum(supplierB_base$total_time$sojourn_time[1:i])/i)/(sum(supplierB_base$total_time$sojourn_time[1:(i+1)])/(i+1)) < 0.99) > 0){
  i = i+1
}

gridExtra::grid.arrange(plot_baseA, plot_baseB, nrow = 1, top ="Cummulative mean of the lead times generated from the OJN")

#Plot distributions

supplierA_LT_dist <- supplierA_base$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1) + 
  ggtitle("Distribution of lead times generated from supplier A") + geom_vline(xintercept = mean(supplierA_base$total_time$sojourn_time), color="red")

supplierB_LT_dist <- supplierB_base$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1) + 
  ggtitle("Distribution of lead times generated from supplier B") + geom_vline(xintercept = mean(supplierB_base$total_time$sojourn_time), color="red") +
  ylab("")


gridExtra::grid.arrange(supplierA_LT_dist, supplierB_LT_dist, nrow = 1, top ="Distribution of the lead times generated from the OJN")

