#Inventory model
library(forecast)
library(inventorize)
source("C:/Users/Bruger/Documents/P8/Projekt/R/src/Inventory_model_retailer.R")

#demand <- runif(5000, min = 4500, max = 5500)
#leadtimes_retailer <- round(runif(5000, min = 1, max = 7))
#leadtimes_manufacturer <- round(runif(5000, min = 1, max = 7))

Inventory_model_manufacturer<- function(m, n, demand, leadtimes_manufacturer, leadtimes_retailer){
  #Generate orders from retailer
  output <- Inventory_model_retailer(m,n,demand,leadtimes_retailer)
  orders <- output$replenishment_orders_retailer
  
  #manufacturer
  #Generate forecast orders with moving average method with delay param n
  forecast_demand_manufacturer <- rep(NA,length(orders))
  for (i in ((max(n,m)+3)+n):(length(orders))) {
    train_demand <- orders[(i - n):(i - 1)]
    forecast_demand_manufacturer[i] <- sum(train_demand)/n
  }  
  
  #Generate forecast leadtime with moving average method with delay param m
  forecast_leadtime_manufacturer <- rep(NA,length(leadtimes_manufacturer))
  for (i in (m+1):(length(leadtimes_manufacturer))) {
    train_leadtime <- leadtimes_manufacturer[(i-m):(i - 1)]
    forecast_leadtime_manufacturer[i] <- (((sum(train_leadtime))/m))
  }
  
  #Calculate lead time demand at the beginning of period t
  leadtime_demand_manufacturer <- rep(NA,length(forecast_leadtime_manufacturer))
  for (i in (((max(n,m)+3)+n)):(length(forecast_leadtime_manufacturer)-max(leadtimes_manufacturer))) {
    leadtime_demand_manufacturer[i] <- sum(orders[i:(i + leadtimes_manufacturer[i] -1)])
  }
  
  #Forecast lead time demand
  leadtime_demand_forecast_manufacturer <- rep(NA, length(forecast_leadtime_manufacturer))
  for (i in (((max(n,m)+3)+n)):(length(forecast_leadtime_manufacturer))) {
    leadtime_demand_forecast_manufacturer[i] <- (forecast_leadtime_manufacturer[i]*forecast_demand_manufacturer[i])
  }
  
  
  # Calculate Lead time demand errors
  leadtime_demand_errors_manufacturer <- rep(NA, length(forecast_leadtime_manufacturer))
  leadtime_demand_errors_manufacturer[1:(length(leadtimes_manufacturer)-max(leadtimes_manufacturer))] <- (leadtime_demand_manufacturer[1:(length(leadtimes_manufacturer)-max(leadtimes_manufacturer))] - leadtime_demand_forecast_manufacturer[1:(length(leadtimes_manufacturer)-max(leadtimes_manufacturer))])
  
  # Inventory level (position) to time t
  z <- 1.65 # specifies the probability 95% that demand is fulfilled by the on-hand inventory on normal z-score
  S_t <- rep(NA,length(leadtime_demand_forecast_manufacturer))
  for (i in (((max(n,m)+4)+n)):(length(leadtime_demand_forecast_manufacturer))){
    S_t[i] <- leadtime_demand_forecast_manufacturer[i] + z*(sd(na.omit(leadtime_demand_errors_manufacturer[(max(n,m)+1):i])))
  }
  
  # Order-up-to-level policy quantity
  q_t_manufacturer <- rep(NA,length(leadtime_demand_forecast_manufacturer))
  for (i in ((((max(n,m)+5)+n)):length(leadtime_demand_forecast_manufacturer))){
    q_t_manufacturer[i] <- S_t[i] - S_t[i-1] + orders[i-1]
  }
  
  # Bullwhip
  BM_manufacturer <- rep(NA, length(leadtimes_manufacturer))
  for (i in (((max(n,m)+5)+n)+max(leadtimes_manufacturer)):(length(leadtimes_manufacturer)-max(leadtimes_manufacturer))){
    BM_manufacturer[i] <- (var(na.omit(q_t_manufacturer[(max(n,m)+5+n):i]))/(var(demand[(max(n,m)+5+n):i])))
  }
  
  BM_sim_manufacturer <- ((var(q_t_manufacturer[(max(n,m)+5+n):length(leadtimes_manufacturer)]))/(var(demand)))
  
  return(list("BM_sim_manufacturer"=BM_sim_manufacturer, 
              "replenishment_orders_manufacturer"=q_t_manufacturer,
              "BM_manufacturer"=BM_manufacturer))
}

#output <- Inventory_model_manufacturer(m = 20, n = 1, demand, leadtimes_manufacturer, leadtimes_retailer)
#output$BM_sim_manufacturer

#x <- 1:5000
#plot(x, output$BM_manufacturer[,2], type = "l", lty = 1)
#lines(x, output$BM_manufacturer[,1], col = 'red')

#output_manu <- Inventory_model_MA(m = 1, n = 1, output$replenishment_orders_manufacturer, leadtimes_manufacturer)

#output_manu <- Inventory_model_MA(m = 5, n = 5, demand, leadtimes_manufacturer)
#output_manu$BM_sim_manufacturer
#output_manu$BM_theoretical_manufacturer

#x <- 1:5000
#plot(x, output_manu$BM_manufacturer[,2], type = "l", lty = 1)
#lines(x, output_manu$BM_manufacturer[,1], col = 'red')
