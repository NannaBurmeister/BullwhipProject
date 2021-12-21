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
par(mfrow=c(1,1))
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

i=1
while (sum((sum(supplierA_base$total_time$sojourn_time[1:i])/i)/(sum(supplierA_base$total_time$sojourn_time[1:(i+1)])/(i+1)) > 1.01 ) 
           | ((sum(supplierA_base$total_time$sojourn_time[1:i])/i)/(sum(supplierA_base$total_time$sojourn_time[1:(i+1)])/(i+1)) < 0.99) > 0){
  i = i+1
}


#Steady state bullwhip
N =10*365
demand <- runif(N, min = 4500, max = 5500)
inventory_base <- Inventory_model_retailer(5,5,demand = demand, leadtimes_retailer = sample(supplierA_base$total_time$sojourn_time,size = N))

x <- 1:N
sup_plot_base <- plot(x, inventory_base$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="Bullwhip Effect Measure", main="Base model")


#COnvergence
i=(5+3+round(max(supplierA_base$total_time$sojourn_time+0.5)))

while (sum((inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(supplierA_base$total_time$sojourn_time+0.5))-1)),1] > 1.01 ) 
       | (inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(supplierA_base$total_time$sojourn_time+0.5))-1)),1] < 0.99)) > 0){
  i = i+1
}

i

##high capacity
mu_D1 <- 1
#Run generator
supplier_high_cap <- generate_leadtimes_supplier(N, C, K, M, alpha, mu_I, mu_D1, mu_D2, mu_D3, mu_O)

#Mean of dist 
mean(supplier_high_cap$total_time$sojourn_time)

#Variance of dist
var(supplier_high_cap$total_time$sojourn_time)

#Kurtosis
kurtosis(supplier_high_cap$total_time$sojourn_time)

#Skewness
skewness(supplier_high_cap$total_time$sojourn_time)

#Steady state bullwhip
inventory_high_cap <- Inventory_model_retailer(5,5,demand = demand, leadtimes_retailer = sample(supplier_high_cap$total_time$sojourn_time,size = N))

sup_plot_high_cap <- plot(x, inventory_high_cap$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="", main = "High capacity D1")

i=(5+3+round(max(supplier_high_cap$total_time$sojourn_time+0.5)))

while (sum((inventory_high_cap$BM_retailer[i,1]/inventory_high_cap$BM_retailer[((i+1):(N-round(max(supplier_high_cap$total_time$sojourn_time+0.5))-1)),1] > 1.01 ) 
           | (inventory_high_cap$BM_retailer[i,1]/inventory_high_cap$BM_retailer[((i+1):(N-round(max(supplier_high_cap$total_time$sojourn_time+0.5))-1)),1] < 0.99)) > 0){
  i = i+1
}

i
#1


##low capacity
mu_D1 <- 0.25

N <- 365*10  # number of events to simulate;
demand <- runif(N, min = 4500, max = 5500)

#Run generator
supplier_low_cap <- generate_leadtimes_supplier(N, C, K, M, alpha, mu_I, mu_D1, mu_D2, mu_D3, mu_O)

#Mean of dist 
mean(supplier_low_cap$total_time$sojourn_time)

#Variance of dist
var(supplier_low_cap$total_time$sojourn_time)

#Kurtosis
kurtosis(supplier_low_cap$total_time$sojourn_time)

#Skewness
skewness(supplier_low_cap$total_time$sojourn_time)


#Steady state bullwhip
inventory_low_cap <- Inventory_model_retailer(5,5,demand = demand, leadtimes_retailer = sample(supplier_low_cap$total_time$sojourn_time,size = N))

sup_plot_low_cap <- plot(x, inventory_low_cap$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="", main = "Low capacity D1")

i=(5+3+ceiling(max(supplier_low_cap$total_time$sojourn_time)))
while (sum((inventory_low_cap$BM_retailer[i,1]/inventory_low_cap$BM_retailer[((i+1):(N-round(max(supplier_low_cap$total_time$sojourn_time+0.5))-1)),1] > 1.1 ) 
           | (inventory_low_cap$BM_retailer[i,1]/inventory_low_cap$BM_retailer[((i+1):(N-round(max(supplier_low_cap$total_time$sojourn_time+0.5))-1)),1] < 0.9)) > 0){
  i = i+1
}

i

plot_base <- supplierA_base$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1) + ggtitle("Base model")+
  xlab("Lead times")
plot_high_cap <- supplier_high_cap$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1) + ggtitle("High capacity D1")+
  ylab("") + xlab("Lead times")
plot_low_cap <- supplier_low_cap$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1) + ggtitle("Low capacity D1")+
  ylab("") + xlab("Lead times")

grid.arrange(plot_base, plot_high_cap, plot_low_cap,nrow = 1, top = "Lead times generated from supplier A")

grid.arrange(plot_baseA, sup_plot_high_cap, sup_plot_low_cap,nrow = 1, top = "Lead times generated from supplier A")

