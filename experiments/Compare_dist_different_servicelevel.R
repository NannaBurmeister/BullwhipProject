library(tseries)
library(purrr)
library(tidyverse)
library(grid)
#Parameters
N <- 20000  # number of events to simulate;
C <- 6          # number of nodes Department 1
K <- 6          # number of nodes Department 2
M <- 6          # number of nodes Department 3
alpha <- 0.4    # external arrival rate
mu_I <- 1      # Input node (I) service rate 
mu_D1 <- 0.5     # Deparment 1 (D_i^1) service rate
mu_D2 <- 0.75      # Deparment 1 (D_i^1) service rate
mu_D3 <- 1     # Deparment 1 (D_i^1) service rate
mu_O <- 0.6     # Output node (O) service rate


#Arrivals
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/external_arrival_poisson.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/OJN_simulation_supplierA.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/OJN_simulation_supplierB.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/Inventory_model_supplierB.R")

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




#Steady state bullwhip
N=365*20
demand <- runif(N, min = 4500, max = 5500)
inventory_base <- Inventory_model_retailer(60,5,0.9,demand = demand, leadtimes_retailer = sample(supplierB_base$total_time$sojourn_time,size = N))

x <- 1:N
sup_plot_base <- plot(x, inventory_base$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="Bullwhip Effect Measure", main="Base model")


#COnvergence
i=(60+3+round(max(supplierB_base$total_time$sojourn_time+0.5)))

while (sum((inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(supplierB_base$total_time$sojourn_time+0.5))-1)),1] > 1.01 ) 
           | (inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(supplierB_base$total_time$sojourn_time+0.5))-1)),1] < 0.99)) > 0){
  i = i+1
}

i

mean(na.omit(inventory_base$BM_retailer))

par(mfrow=c(1,3))
x <- 1:N
sup_plot_base <- plot(x, inventory_base$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="Bullwhip Effect Measure", main="Base model")

##Lower service level
pIO <- 0.75
#Run generator
supplierB_low_cap <- generate_leadtimes_supplierB(N, C, K, M, alpha, mu_I, mu_D1, mu_D2, mu_D3, mu_O, pIO)

#Mean of dist 
mean(supplierB_low_cap$total_time$sojourn_time)

#Variance of dist
var(supplierB_low_cap$total_time$sojourn_time)

#Kurtosis
kurtosis(supplierB_low_cap$total_time$sojourn_time)

#Skewness
skewness(supplierB_low_cap$total_time$sojourn_time)

i=(60+3+round(max(supplierB_base$total_time$sojourn_time+0.5)))
while (sum((supplierB_low_cap$total_time$sojourn_time[i]/supplierB_low_cap$total_time$sojourn_time[(i+1):N] < 1.01 )
           & (supplierB_low_cap$total_time$sojourn_time[i]/supplierB_low_cap$total_time$sojourn_time[(i+1):N] > 0.99)) > 0){
  i = i+1
}

inventory_low <- Inventory_model_retailer(60,5,demand = demand, 0.9, leadtimes_retailer = sample(supplierB_low_cap$total_time$sojourn_time,size = N))

#COnvergence
i=(60+3+round(max(supplierB_low_cap$total_time$sojourn_time+0.5)))

while (sum((inventory_low$BM_retailer[i,1]/inventory_low$BM_retailer[((i+1):(N-round(max(supplierB_low_cap$total_time$sojourn_time+0.5))-1)),1] > 1.01 ) 
           | (inventory_low$BM_retailer[i,1]/inventory_low$BM_retailer[((i+1):(N-round(max(supplierB_low_cap$total_time$sojourn_time+0.5))-1)),1] < 0.99)) > 0){
  i = i+1
}

i

mean(na.omit(inventory_low$BM_retailer[,1]))

sup_plot_Low_cap <- plot(x, inventory_low_cap$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="", main = "Low service level")

##Higher service level
pIO <- 0.99
#Run generator
supplierB_high_cap <- generate_leadtimes_supplierB(N, C, K, M, alpha, mu_I, mu_D1, mu_D2, mu_D3, mu_O, pIO)

#Mean of dist 
mean(supplierB_high_cap$total_time$sojourn_time)

#Variance of dist
var(supplierB_high_cap$total_time$sojourn_time)

#Kurtosis
kurtosis(supplierB_high_cap$total_time$sojourn_time)

#Skewness
skewness(supplierB_high_cap$total_time$sojourn_time)

#Steady state bullwhip
inventory_high_cap <- Inventory_model_retailer(60,5,0.9,demand = demand, leadtimes_retailer = sample(supplierB_high_cap$total_time$sojourn_time,size = N))


i=(60+3+round(max(supplierB_high_cap$total_time$sojourn_time+0.5)))

while (sum((inventory_high_cap$BM_retailer[i,1]/inventory_high_cap$BM_retailer[((i+1):(N-round(max(supplierB_high_cap$total_time$sojourn_time+0.5))-1)),1] > 1.01 ) 
           | (inventory_high_cap$BM_retailer[i,1]/inventory_high_cap$BM_retailer[((i+1):(N-round(max(supplierB_high_cap$total_time$sojourn_time+0.5))-1)),1] < 0.99)) > 0){
  i = i+1
}

i

mean(na.omit(inventory_high_cap$BM_retailer[,1]))

sup_plot_high_cap <- plot(x, inventory_high_cap$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="", main = "High service level")




plot_base <- supplierB_base$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1) + ggtitle("Base model") +
  xlab("Lead times")
plot_low_cap <- supplierB_low_cap$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1) + ggtitle("Low service level")+
  ylab("") + xlab("Lead times")
plot_high_cap <- supplierB_high_cap$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1) + ggtitle("High service level")+
  ylab("") + xlab("Lead times")

grid.arrange(plot_base, plot_low_cap, plot_high_cap,nrow = 1, top = "Lead times generated from supplier B")


