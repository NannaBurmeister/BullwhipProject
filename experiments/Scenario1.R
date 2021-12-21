#Deterministic seasonal lead times

source("C:/Users/Bruger/Documents/P8/Projekt/R/src/inventory_model_season_deterministic.R")
library(Metrics)
library(ggplot2)
library(dplyr)


#Determine parameters
n <- 5
set.seed(123)
demand <- runif((365*20), min = 4500, max = 5500)

#Deterministic lead times for normal and winter season
normal_LT <- 7
winter_LT <- 21

service_levels <- c(0.99, 0.95, 0.9, 0.8, 0.75)
output <- list()

for (i in 1:length(service_levels)){
  output[[i]] <- inventory_model_season_deterministic(n, service_levels[i], demand, normal_LT, winter_LT)
}

#NRMSE
(rmse(output[[3]]$leadtime_demand_retailer[6:7279], (output[[3]]$leadtime_demand_retailer[6:7279]+output[[3]]$errors[6:7279]))/mean(na.omit(leadtime_demand_retailer)))

#SME
mean(((output[[3]]$errors[1887:7099])))/mean(leadtime_demand_retailer[1887:7099])*100



data <- data.frame(output[[3]]$replenishment_orders_retailer[8:6800], output[[3]]$season[8:6800], 8:6800)
colnames(data) <- c("orders", "season", "id")

ggplot(data, aes(x=season, y=orders)) + geom_jitter(alpha=0.05) + geom_violin(draw_quantiles = c(0.25,0.5,0.75), alpha = 0.1) + ylim(c(0,20000)) + ggtitle("Violin plot of the orders")

data %>% dplyr::filter(season == "normal") %>% dplyr::summarize(mean(orders))

data %>% group_by(season) %>% dplyr::summarize(mean(orders))

data %>% group_by(season) %>% summarize(sum(orders < 0))

data %>% group_by(season) %>% summarize(sum(orders > 20000))

data %>% filter(orders < 0)

data %>% filter(orders > 20000)



stat <- matrix(NA, 2,5)
rownames(stat) <- c("rmse", "mean_error")
for (i in 1:length(service_levels)){
  stat[1,i] <-sqrt(mean((na.omit(output[[i]]$errors))^2))
  stat[2,i] <- mean((na.omit(output[[i]]$errors)))
}

stat

## Long run

#Calculate convergens of bullwhip measure
# p/m 1%
convergence_1 <- rep(NA, length(service_levels))
for (k in 1:length(service_levels)){
  i=(n+3+winter_LT)
  while (sum(((output[[k]]$BM_retailer[i]/output[[k]]$BM_retailer[(i+1):(length(demand)-winter_LT-1)]) > 1.01 )
             | ((output[[k]]$BM_retailer[i]/output[[k]]$BM_retailer[(i+1):(length(demand)-winter_LT-1)]) < 0.99)) > 0){
    i = i+1
  }
  convergence_1[k] <- i
}

# p/m 5%
convergence_5 <- rep(NA, length(service_levels))
for (k in 1:length(service_levels)){
  j=(n+3+winter_LT)
  while (sum(((output[[k]]$BM_retailer[j]/output[[k]]$BM_retailer[(j+1):(length(demand)-winter_LT-1)]) > 1.05 )
             | ((output[[k]]$BM_retailer[j]/output[[k]]$BM_retailer[(j+1):(length(demand)-winter_LT-1)]) < 0.95)) > 0){
    j = j+1
  }
  convergence_5[k] <- j
}

library(ggplot2)
x <- 1:length(demand)
BM_plots <- list()
for (i in 1: length(service_levels)) {
  data <- as.data.frame(cbind(x,output[[i]]$BM_retailer, output[[i]]$errors, output[[i]]$season))
  colnames(data) <- c("x", "BM", "errors", "season")
  BM_plots[[i]] <- data %>% ggplot(aes(x = as.numeric(x), y = as.numeric(BM))) + geom_line() +
    geom_vline(xintercept = convergence_1[i], color = "red") + 
    geom_vline(xintercept = convergence_5[i], color = "blue") +
    labs(x = "Time iterations in days", y = "Bullwhip effect measure")
}

BM_plots[[1]] <- BM_plots[[1]] + ggtitle("Service level 0.99")
BM_plots[[2]] <- BM_plots[[2]] + ggtitle("Service level 0.95")
BM_plots[[3]] <- BM_plots[[3]] + ggtitle("Service level 0.90")
BM_plots[[4]] <- BM_plots[[4]] + ggtitle("Service level 0.85")
BM_plots[[5]] <- BM_plots[[5]] + ggtitle("Service level 0.75")

gridExtra::grid.arrange(BM_plots[[1]],BM_plots[[2]], BM_plots[[3]], BM_plots[[4]], BM_plots[[5]])

error_plot[[1]] <- error_plot[[1]] + ggtitle("Service level 0.99")
error_plot[[2]] <- error_plot[[2]] + ggtitle("Service level 0.95")
error_plot[[3]] <- error_plot[[3]] + ggtitle("Service level 0.90")
error_plot[[4]] <- error_plot[[4]] + ggtitle("Service level 0.85")
error_plot[[5]] <- error_plot[[5]] + ggtitle("Service level 0.75")

gridExtra::grid.arrange(error_plot[[1]],error_plot[[2]], error_plot[[3]], error_plot[[4]], error_plot[[5]])

mean <- rep(NA, length(service_levels))
variance <- rep(NA, length(service_levels))
min <- rep(NA, length(service_levels))
max <- rep(NA, length(service_levels))


for (i in 1: length(service_levels)) {
 mean[i] <- mean(na.omit(output[[i]]$BM_retailer))
 variance[i] <- var(na.omit(output[[i]]$BM_retailer))
 min[i] <- min(na.omit(output[[i]]$BM_retailer))
 max[i] <- max(na.omit(output[[i]]$BM_retailer))
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
    mean[j,i] <- mean(na.omit(output[[i]]$BM_yearly[(n+2+winter_LT+(365*(j-1))):(n+3+winter_LT+(365*j))]))
    variance[j,i] <- var(na.omit(output[[i]]$BM_yearly[(n+2+winter_LT+(365*(j-1))):(n+3+winter_LT+(365*j))]))
    min[j,i] <- min(na.omit(output[[i]]$BM_yearly[(n+2+winter_LT+(365*(j-1))):(n+3+winter_LT+(365*j))]))
    max[j,i] <- max(na.omit(output[[i]]$BM_yearly[(n+2+winter_LT+(365*(j-1))):(n+3+winter_LT+(365*j))]))
  }
}





