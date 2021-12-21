### Node dependency analysis
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/OJN_simulation_supplier.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/OJN_simulation_manu.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/Inventory_model_retailer.R")
library(Hmisc)
library(moments)
library(ggplot2)
library(tidyr)
library(gridExtra)

#Supplier A

## Base model
#lambda/mu udnyttelsesgrad
Base_model <- generate_leadtimes_supplier(N=(3000), C = 6, K = 6, M = 6, alpha = 0.4,
                                                       mu_I = 1, mu_D1 = 0.5, mu_D2 = 0.75, mu_D3 = 1,
                                                       mu_O = 2)

Base_data <- data.frame(Base_model$node_sojourn_time, Base_model$total_time$sojourn_time)
Sojourn_time <- Base_model$node_sojourn_time
Service <- Base_model$node_service
Waiting_times <- Sojourn_time - Service
Waiting_times[Waiting_times<0] <- 0

colnames(Base_data) <- c("node_I","D1","D2","D3","node_O","total")
colnames(Waiting_times) <- c("node_I","D1","D2","D3","node_O")

temp <- as.matrix(Base_model$interarrivals)
cov <- rep(NA,5)
for(i in (1:5)){
  temp1 <- temp[,(i+1)]
  cov[i] <- sd(na.omit(temp1))/mean(na.omit(temp1))
}

temp2 <- as.matrix(Base_model$node_service)
cov_ser <- rep(NA,5)
for(i in (1:5)){
  temp1 <- temp2[,(i)]
  cov_ser[i] <- sd(na.omit(temp1))/mean(na.omit(temp1))
}



#cor(Waiting_times[19000:20000,])

rcorr(as.matrix(Waiting_times[2000:3000,]), type = "spearman")

#cor(Base_data[19000:20000,])

rcorr(as.matrix(Base_data[2000:3000,]),type = "spearman")

hist(Waiting_times$node_I, breaks = 100)

mean(Base_model$node_sojourn_time$node_O.total_time)

base_plot <- Base_model$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1)+ ggtitle("Base network")



# Distribution stats
#Mean of dist 
mean(Base_model$total_time$sojourn_time)

#Variance of dist
var(Base_model$total_time$sojourn_time)

#Kurtosis
kurtosis(Base_model$total_time$sojourn_time)

#Skewness
skewness(Base_model$total_time$sojourn_time)

# Bullwhip

#Steady state bullwhip
N =20*365
demand <- runif(N, min = 4500, max = 5500)
inventory_base <- Inventory_model_retailer(5,5,p = 0.9, demand = demand, leadtimes_retailer = sample(Base_model$total_time$sojourn_time,size = N))

x <- 1:N
plot(x, inventory_base$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="Bullwhip Effect Measure", main="Base model")


#Convergence
i=(5+3+round(max(Base_model$total_time$sojourn_time+0.5)))

while (sum((inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(Base_model$total_time$sojourn_time+0.5))-1)),1] > 1.01 ) 
           | (inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(Base_model$total_time$sojourn_time+0.5))-1)),1] < 0.99)) > 0){
  i = i+1
}

i

mean(na.omit(inventory_base$BM_retailer[,1]))

par(mfrow=c(2,2))
x <- 1:N
plot(x, inventory_base$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="Bullwhip Effect Measure", main="Base model")


#Distruption at node I
Dis_nodeI <- generate_leadtimes_supplier(N=(20000), C = 6, K = 6, M = 6,alpha = 0.4,
                                          mu_I = 0.5, mu_D1 = 0.5, mu_D2 = 0.75, mu_D3 = 1,
                                          mu_O = 2)
Dis_nodeI_data <- data.frame(Dis_nodeI$node_sojourn_time, Dis_nodeI$total_time$sojourn_time)

x <- 1:20000
plot(x, cumsum(Dis_nodeI$total_time$sojourn_time)/x, type = "l", lty = 1)
abline(h = Dis_nodeI$theoretical_sojourn_time, col="red")

colnames(Dis_nodeI_data) <- c("node_I","D1","D2","D3","node_O","total")
Sojourn_time <- Dis_nodeI$node_sojourn_time
Service <- Dis_nodeI$node_service
Waiting_times <- Sojourn_time - Service
Waiting_times[Waiting_times<0] <- 0
#cor(Waiting_times[19000:20000,])

colnames(Base_data) <- c("node_I","D1","D2","D3","node_O","total")
colnames(Waiting_times) <- c("node_I","D1","D2","D3","node_O")

rcorr(as.matrix(Waiting_times[9000:10000,]), type = "spearman")

#cor(Base_data[19000:20000,])

rcorr(as.matrix(Dis_nodeI_data[9000:10000,]),type = "spearman")



nodeI_plot <- Dis_nodeI$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1)+ ggtitle("Disruption node I")

#Input node flaske hals, udnyttelsesgraden ved input node højere end output nodens. 
#Mindre kødannelse ved output noden
#Generelt skiftes der fortegn med alle input nodes korrelationer med de resterende noder

# Distribution stats
#Mean of dist 
mean(Dis_nodeI$total_time$sojourn_time)

#Variance of dist
var(Dis_nodeI$total_time$sojourn_time)

#Kurtosis
kurtosis(Dis_nodeI$total_time$sojourn_time)

#Skewness
skewness(Dis_nodeI$total_time$sojourn_time)

# Bullwhip

#Steady state bullwhip
N =20*365
demand <- runif(N, min = 4500, max = 5500)
Dis_nodeI_base <- Inventory_model_retailer(5,5,p = 0.9, demand = demand, leadtimes_retailer = sample(Dis_nodeI$total_time$sojourn_time,size = N))

x <- 1:N
plot(x, Dis_nodeI_base$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="Bullwhip Effect Measure", main="Disruption at node I")


#COnvergence
i=(5+3+round(max(Dis_nodeI$total_time$sojourn_time+0.5)))

while (sum((inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(Dis_nodeI$total_time$sojourn_time+0.5))-1)),1] > 1.01 ) 
           | (inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(Dis_nodeI$total_time$sojourn_time+0.5))-1)),1] < 0.99)) > 0){
  i = i+1
}

i

mean(na.omit(inventory_base$BM_retailer[,1]))

#Disruption node O


Dis_nodeO <- generate_leadtimes_supplier(N=(20000), C = 6, K = 6, M = 6,alpha = 0.4,
                                         mu_I = 1, mu_D1 = 0.5, mu_D2 = 0.75, mu_D3 = 1,
                                         mu_O = 0.5)
Dis_nodeO_data <- data.frame(Dis_nodeO$node_sojourn_time, Dis_nodeO$total_time$sojourn_time)

colnames(Dis_nodeO_data) <- c("node_I","D1","D2","D3","node_O","total")

x <- 1:20000
plot(x, cumsum(Dis_nodeO$total_time$sojourn_time)/x, type = "l", lty = 1)
abline(h = Dis_nodeO$theoretical_sojourn_time, col="red")

colnames(Dis_nodeO_data) <- c("node_I","D1","D2","D3","node_O","total")
Sojourn_time <- Dis_nodeO$node_sojourn_time
Service <- Dis_nodeO$node_service
Waiting_times <- Sojourn_time - Service
Waiting_times[Waiting_times<0] <- 0
#cor(Waiting_times[19000:20000,])

colnames(Dis_nodeO_data) <- c("node_I","D1","D2","D3","node_O","total")
colnames(Waiting_times) <- c("node_I","D1","D2","D3","node_O")

rcorr(as.matrix(Waiting_times[15000:16000,]), type = "spearman")

#cor(Base_data[19000:20000,])

rcorr(as.matrix(Dis_nodeO_data[15000:16000,]),type = "spearman")


nodeO_plot <- Dis_nodeO$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1)+ ggtitle("Disruption node O")

# Distribution stats
#Mean of dist 
mean(Dis_nodeO$total_time$sojourn_time)

#Variance of dist
var(Dis_nodeO$total_time$sojourn_time)

#Kurtosis
kurtosis(Dis_nodeO$total_time$sojourn_time)

#Skewness
skewness(Dis_nodeO$total_time$sojourn_time)

# Bullwhip

#Steady state bullwhip
N =20*365
demand <- runif(N, min = 4500, max = 5500)
nodeO_base <- Inventory_model_retailer(5,5,p = 0.9, demand = demand, leadtimes_retailer = sample(Dis_nodeI$total_time$sojourn_time,size = N))

x <- 1:N
plot(x, nodeO_base$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="Bullwhip Effect Measure", main="Disruption at node O")



#COnvergence
i=(5+3+round(max(Dis_nodeI$total_time$sojourn_time+0.5)))

while (sum((inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(Dis_nodeI$total_time$sojourn_time+0.5))-1)),1] > 1.01 ) 
           | (inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(Dis_nodeI$total_time$sojourn_time+0.5))-1)),1] < 0.99)) > 0){
  i = i+1
}

i

mean(na.omit(inventory_base$BM_retailer[,1]))



#Distruption at depertment 1, only one station working


Dis_D1 <- generate_leadtimes_supplier(N=(30000), C = 1, K = 6, M = 6,alpha = 0.4,
                                          mu_I = 1, mu_D1 = 0.5, mu_D2 = 0.75, mu_D3 = 1,
                                          mu_O = 2)
Dis_D1_data <- data.frame(Dis_D1$node_sojourn_time, Dis_D1$total_time$sojourn_time)

colnames(Dis_D1_data) <- c("node_I","D1","D2","D3","node_O","total")

x <- 1:30000
plot(x, cumsum(Dis_D1$total_time$sojourn_time)/x, type = "l", lty = 1)
abline(h = Dis_D1$theoretical_sojourn_time, col="red")

colnames(Dis_D1_data) <- c("node_I","D1","D2","D3","node_O","total")
Sojourn_time <- Dis_D1$node_sojourn_time
Service <- Dis_D1$node_service
Waiting_times <- Sojourn_time - Service
Waiting_times[Waiting_times<0] <- 0
#cor(Waiting_times[19000:20000,])

colnames(Dis_D1_data) <- c("node_I","D1","D2","D3","node_O","total")
colnames(Waiting_times) <- c("node_I","D1","D2","D3","node_O")

rcorr(as.matrix(Waiting_times[17000:18000,]), type = "spearman")

#cor(Base_data[19000:20000,])

rcorr(as.matrix(Dis_D1_data[17000:18000,]),type = "spearman")




D1_plot <- Dis_D1$total_time %>% ggplot(aes(sojourn_time)) + geom_histogram(binwidth=1)+ ggtitle("Disruption D1")

# Distribution stats
#Mean of dist 
mean(Dis_D1$total_time$sojourn_time)

#Variance of dist
var(Dis_D1$total_time$sojourn_time)

#Kurtosis
kurtosis(Dis_D1$total_time$sojourn_time)

#Skewness
skewness(Dis_D1$total_time$sojourn_time)

# Bullwhip

#Steady state bullwhip
N =20*365
demand <- runif(N, min = 4500, max = 5500)
Dis_D1_base <- Inventory_model_retailer(5,5,p = 0.9, demand = demand, leadtimes_retailer = sample(Dis_D1$total_time$sojourn_time,size = N))

x <- 1:N
plot(x, Dis_D1_base$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="Bullwhip Effect Measure", main="Disruption at D1")


#COnvergence
i=(5+3+round(max(Dis_D1$total_time$sojourn_time+0.5)))

while (sum((inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(Dis_D1$total_time$sojourn_time+0.5))-1)),1] > 1.01 ) 
           | (inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(Dis_D1$total_time$sojourn_time+0.5))-1)),1] < 0.99)) > 0){
  i = i+1
}

i

mean(na.omit(inventory_base$BM_retailer[,1]))



#Extreme model

#Distruption at depertment 1, only one station working


Ex <- generate_leadtimes_supplier(N=(20000), C = 5, K = 6, M = 6,alpha = 0.4,
                                      mu_I = 1, mu_D1 = 0.25, mu_D2 = 0.37, mu_D3 = 0.5,
                                      mu_O = 1)
Ex_data <- data.frame(Ex$node_sojourn_time, Ex$total_time$sojourn_time)

colnames(Ex_data) <- c("node_I","D1","D2","D3","node_O","total")

cor(Ex_data)

rcorr(as.matrix(Ex_data))

# Distribution stats
#Mean of dist 
mean(Dis_D1$total_time$sojourn_time)

#Variance of dist
var(Dis_D1$total_time$sojourn_time)

#Kurtosis
kurtosis(Dis_D1$total_time$sojourn_time)

#Skewness
skewness(Dis_D1$total_time$sojourn_time)

# Bullwhip

#Steady state bullwhip
N =20*365
demand <- runif(N, min = 4500, max = 5500)
inventory_base <- Inventory_model_retailer(5,5,p = 0.9, demand = demand, leadtimes_retailer = sample(Dis_D1$total_time$sojourn_time,size = N))

x <- 1:N
plot(x, inventory_base$BM_retailer[,1], type = "l", lty = 1, xlab = "Time iterations", ylab="Bullwhip Effect Measure", main="Base model")


#COnvergence
i=(5+3+round(max(Dis_D1$total_time$sojourn_time+0.5)))

while (sum((inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(Dis_D1$total_time$sojourn_time+0.5))-1)),1] > 1.01 ) 
           | (inventory_base$BM_retailer[i,1]/inventory_base$BM_retailer[((i+1):(N-round(max(Dis_D1$total_time$sojourn_time+0.5))-1)),1] < 0.99)) > 0){
  i = i+1
}

i

mean(na.omit(inventory_base$BM_retailer[,1]))







#Supplier B
## Base model

Base_model_B <- generate_leadtimes_manufacturer(N=(20000), C = 6, K = 6, M = 6,alpha = 0.4,
                                          mu_I = 1, mu_D1 = 0.5, mu_D2 = 0.75, mu_D3 = 1,
                                          mu_O = 0.6, pIO = 0.9)
Base_data_B <- data.frame(Base_model_B$node_sojourn_time, Base_model_B$total_time$sojourn_time)

Base_data_B <- Base_data_B %>% select(-id)


colnames(Base_data_B) <- c("node_I","D1","D2","D3","node_O","total")

cor(Base_data_B, use = "pairwise.complete.obs")

rcorr(as.matrix(Base_data_B))

#Correlation calculated between each pair of variables using the complete pairs of
#observations of observations on those variables.

##Lower service level
LS_model_B <- generate_leadtimes_manufacturer(N=(20000), C = 6, K = 6, M = 6,alpha = 0.4,
                                                mu_I = 1, mu_D1 = 0.5, mu_D2 = 0.75, mu_D3 = 1,
                                                mu_O = 0.6, pIO = 0.75)
LS_data_B <- data.frame(LS_model_B$node_sojourn_time, LS_model_B$total_time$sojourn_time)

LS_data_B <- LS_data_B %>% select(-id)


colnames(LS_data_B) <- c("node_I","D1","D2","D3","node_O","total")

cor(LS_data_B, use = "pairwise.complete.obs")
rcorr(as.matrix(LS_data_B))


##Higher service level
HS_model_B <- generate_leadtimes_manufacturer(N=(20000), C = 6, K = 6, M = 6,alpha = 0.4,
                                              mu_I = 1, mu_D1 = 0.5, mu_D2 = 0.75, mu_D3 = 1,
                                              mu_O = 0.6, pIO = 0.99)
N=(20000)
C = 6
K = 6
M = 6
alpha = 0.4
mu_I = 1
mu_D1 = 0.5
mu_D2 = 0.75
mu_D3 = 1
mu_O = 0.6
pIO = 0.99

HS_data_B <- data.frame(HS_model_B$node_sojourn_time, HS_model_B$total_time$sojourn_time)

HS_data_B <- HS_data_B %>% select(-id)

colnames(HS_data_B) <- c("node_I","D1","D2","D3","node_O","total")

cor(HS_data_B, use = "pairwise.complete.obs")

rcorr(as.matrix(HS_data_B))

grid.arrange(base_plot, nodeI_plot, nodeO_plot, D1_plot, nrow = 2, top="Distributions from disruptions in base network")
