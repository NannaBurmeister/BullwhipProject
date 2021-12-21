source("C:/Users/Bruger/Documents/P8/Projekt/R/src/inventory_model_notseason_stochastic.R")
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/OJN_simulation_supplierA.R")

set.seed(1)

#Moving average parameter for demand
n <- 5
m <- 60
N= 365*20
#Generate stochastic demand
demand <- runif((365*20), min = 4500, max = 5500)

##Winter seasonality

#Generate lead times for supplier A
normal_LT <- generate_leadtimes_supplier(N=(20000), C = 6, K = 6, M = 6,alpha = 0.4,
                                         mu_I = 1, mu_D1 = 0.5, mu_D2 = 0.75, mu_D3 = 1,
                                         mu_O = 2)$total_time$sojourn_time
winter_LT <- generate_leadtimes_supplier(N=(20000), C = 6, K = 6, M = 6,alpha = 0.4,
                                         mu_I = 1, mu_D1 = 0.5, mu_D2 = 0.75, mu_D3 = 1,
                                         mu_O = 0.47)$total_time$sojourn_time
#Generate seasonal lead time
leadtimes_retailer <- rep(NA,length(demand))
season <- rep(NA,length(demand))
for (i in 1:(length(demand))) {
  if ((i >= 1 & i<=61)|(i >= 334 & i<=365)|(i >= 1+365 & i<=61+365)|(i >= 334+365 & i<=365+365)
      |(i >= 1+(365*2) & i<=61+(365*2))|(i >= 334+(365*2) & i<=365+(365*2))
      |(i >= 1+(365*3) & i<=61+(365*3))|(i >= 334+(365*3) & i<=365+(365*3))
      |(i >= 1+(365*4) & i<=61+(365*4))|(i >= 334+(365*4) & i<=365+(365*4))
      |(i >= 1+(365*5) & i<=61+(365*5))|(i >= 334+(365*5) & i<=365+(365*5))
      |(i >= 1+(365*6) & i<=61+(365*6))|(i >= 334+(365*6) & i<=365+(365*6))
      |(i >= 1+(365*7) & i<=61+(365*7))|(i >= 334+(365*7) & i<=365+(365*7))
      |(i >= 1+(365*8) & i<=61+(365*8))|(i >= 334+(365*8) & i<=365+(365*8))
      |(i >= 1+(365*9) & i<=61+(365*9))|(i >= 334+(365*9) & i<=365+(365*9))
      |(i >= 1+(365*10) & i<=61+(365*10))|(i >= 334+(365*10) & i<=365+(365*10))
      |(i >= 1+(365*11) & i<=61+(365*11))|(i >= 334+(365*11) & i<=365+(365*11))
      |(i >= 1+(365*12) & i<=61+(365*12))|(i >= 334+(365*12) & i<=365+(365*12))
      |(i >= 1+(365*13) & i<=61+(365*13))|(i >= 334+(365*13) & i<=365+(365*13))
      |(i >= 1+(365*14) & i<=61+(365*14))|(i >= 334+(365*14) & i<=365+(365*14))
      |(i >= 1+(365*15) & i<=61+(365*15))|(i >= 334+(365*15) & i<=365+(365*15))
      |(i >= 1+(365*16) & i<=61+(365*16))|(i >= 334+(365*16) & i<=365+(365*16))
      |(i >= 1+(365*17) & i<=61+(365*17))|(i >= 334+(365*17) & i<=365+(365*17))
      |(i >= 1+(365*18) & i<=61+(365*18))|(i >= 334+(365*18) & i<=365+(365*18))
      |(i >= 1+(365*19) & i<=61+(365*19))|(i >= 334+(365*19) & i<=365+(365*19))
      |(i >= 1+(365*20) & i<=61+(365*20))|(i >= 334+(365*20) & i<=365+(365*20))
  ){
    leadtimes_retailer[i] <- sample(winter_LT, 1)
    season[i] <- "winter"
  } else {
    leadtimes_retailer[i] <- sample(normal_LT, 1) 
    season[i] <- "normal"
  }
}


service_levels <- c(0.99, 0.95, 0.9, 0.8, 0.75)

#Moving average
output_notseason <- list()

for (i in 1:length(service_levels)){
  output_notseason[[i]] <- inventory_model_stochastic(n, m, service_levels[i], demand, leadtimes_retailer)
}

#NRMSE
(rmse(output[[3]]$leadtime_demand_retailer[1887:7099], (output[[3]]$leadtime_demand_retailer[1887:7099]+output[[3]]$errors[1887:7099]))/mean(output[[3]]$leadtime_demand_retailer[1887:7099]))

#SME
mean(((output[[3]]$errors[1887:7099])))/mean(output[[3]]$leadtime_demand_retailer[1887:7099])


data <- data.frame(output[[3]]$replenishment_orders_retailer[1888:7099], output[[3]]$season[1888:7099])
colnames(data) <- c("orders", "season")

ggplot(data, aes(x=season, y=orders)) + geom_jitter(alpha=0.05) + geom_violin(draw_quantiles = c(0.25,0.5,0.75), alpha = 0.1) + ylim(c(0,20000)) + ggtitle("Violin plot of the orders")

data %>% group_by(season) %>% dplyr::summarize(mean(orders))
data %>% group_by(season) %>% dplyr::summarize(median(orders))

data %>% group_by(season) %>% summarize(sum(orders < 0))





stat <- matrix(NA, 2,5)
rownames(stat) <- c("rmse", "mean_error")
for (i in 1:length(service_levels)){
  stat[1,i] <-sqrt(mean((na.omit(output_notseason[[i]]$errors))^2))
  stat[2,i] <- mean((na.omit(output_notseason[[i]]$errors)))
}

stat

#Long run

#Moving average

observed_leadtimes <- leadtimes_retailer + (1:N)


convergence_1 <- rep(NA, length(service_levels))
for (k in 1:length(service_levels)){
  i=((365)+max(n,m)+4)
  while (sum(((output_notseason[[k]]$BM_retailer[i]/output_notseason[[k]]$BM_retailer[(i+1):(length(demand)-max(leadtimes_retailer[observed_leadtimes<=length(demand)]))]) > 1.01 )
             | ((output_notseason[[k]]$BM_retailer[i]/output_notseason[[k]]$BM_retailer[(i+1):(length(demand)-max(leadtimes_retailer[observed_leadtimes<=length(demand)]))]) < 0.99)) > 0){
    i = i+1
  }
  convergence_1[k] <- i
}

# p/m 5%
convergence_5 <- rep(NA, length(service_levels))
for (k in 1:length(service_levels)){
  j=((365)+max(n,m)+4)
  while (sum(((output_notseason[[k]]$BM_retailer[j]/output_notseason[[k]]$BM_retailer[(j+1):(length(demand)-max(leadtimes_retailer[observed_leadtimes<=length(demand)]))]) > 1.05 )
             | ((output_notseason[[k]]$BM_retailer[j]/output_notseason[[k]]$BM_retailer[(j+1):(length(demand)-max(leadtimes_retailer[observed_leadtimes<=length(demand)]))]) < 0.95)) > 0){
    j = j+1
  }
  convergence_5[k] <- j
}

library(ggplot2)
x <- 1:length(demand)
BM_plots <- list()
for (i in 1: length(service_levels)) {
  data <- as.data.frame(cbind(x,output_notseason[[i]]$BM_retailer, output_notseason[[i]]$errors))
  colnames(data) <- c("x", "BM", "errors")
  BM_plots[[i]] <- data %>% ggplot(aes(x = as.numeric(x), y = as.numeric(BM))) + geom_line() +
    geom_vline(xintercept = convergence_1[i], color = "red") + 
    geom_vline(xintercept = convergence_5[i], color = "blue") +
    labs(x = "Time iterations in days", y = "Bullwhip effect measure")
#  data <- data[complete.cases(data), ]
#  error_plot[[i]] <- data %>% ggplot(aes(x=as.numeric(x), y = as.numeric(errors))) + geom_point() +
#    labs(x = "Time iterations in days", y = "Value") 
  
}

BM_plots[[1]] <- BM_plots[[1]] + ggtitle("Service level 0.99")
BM_plots[[2]] <- BM_plots[[2]] + ggtitle("Service level 0.95")
BM_plots[[3]] <- BM_plots[[3]] + ggtitle("Service level 0.90")
BM_plots[[4]] <- BM_plots[[4]] + ggtitle("Service level 0.80")
BM_plots[[5]] <- BM_plots[[5]] + ggtitle("Service level 0.75")

gridExtra::grid.arrange(BM_plots[[1]],BM_plots[[2]], BM_plots[[3]], BM_plots[[4]], BM_plots[[5]])


mean <- rep(NA, length(service_levels))
variance <- rep(NA, length(service_levels))
min <- rep(NA, length(service_levels))
max <- rep(NA, length(service_levels))


for (i in 1: length(service_levels)) {
  mean[i] <- mean(na.omit(output_notseason[[i]]$BM_retailer))
  variance[i] <- var(na.omit(output_notseason[[i]]$BM_retailer))
  min[i] <- min(na.omit(output_notseason[[i]]$BM_retailer))
  max[i] <- max(na.omit(output_notseason[[i]]$BM_retailer))
}

##Short run
#Yearly sliding window analysis for first 5 years

mean <- matrix(NA, length(service_levels),5)
colnames(mean) <- c("0.99", "0.95", "0.9", "0.8", "0.75")
rownames(mean) <- c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5")
variance <- matrix(NA, length(service_levels),5)
colnames(variance) <- c("0.99", "0.95", "0.9", "0.8", "0.75")
rownames(variance) <- c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5")
min <- matrix(NA, length(service_levels),5)
colnames(min) <- c("0.99", "0.95", "0.9", "0.8", "0.75")
rownames(min) <- c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5")
max <- matrix(NA, length(service_levels),5)
colnames(max) <- c("0.99", "0.95", "0.9", "0.8", "0.75")
rownames(max) <- c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5")

for (i in 1:length(service_levels)){
  for (j in 1:5){
    mean[j,i] <- mean(na.omit(output_notseason[[i]]$BM_yearly[(max(m,n)+4+(365*(j))):(max(n,m)+4+(365*(j+1)))]))
    variance[j,i] <- var(na.omit(output_notseason[[i]]$BM_yearly[(max(m,n)+4+(365*(j))):(max(n,m)+4+(365*(j+1)))]))
    min[j,i] <- min(na.omit(output_notseason[[i]]$BM_yearly[(max(m,n)+4+(365*(j))):(max(n,m)+4+(365*(j+1)))]))
    max[j,i] <- max(na.omit(output_notseason[[i]]$BM_yearly[(max(m,n)+4+(365*(j))):(max(n,m)+4+(365*(j+1)))]))
  }
}

var(na.omit(output_notseason[[1]]$errors))
var(na.omit(output_practic[[1]]$errors))
var(na.omit(output_season[[1]]$errors))
