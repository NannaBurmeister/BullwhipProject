#Inventory model
library(forecast)
library(inventorize)

#demand <- runif(5000, min = 4500, max = 5500)
#leadtimes_retailer <- round(runif(5000, min = 1, max = 7))

Inventory_model_retailer<- function(m, n, demand, leadtimes_retailer){
  
#Retailer
  #Generate forecast demand with moving average method with delay param n
  forecast_demand_retailer <- rep(NA,length(demand))
  for (i in (n+1):(length(demand))) {
    train_demand <- demand[(i - n):(i - 1)]
    forecast_demand_retailer[i] <- sum(train_demand)/n
  }  
  
  #Generate forecast leadtime with moving average method with delay param m
  forecast_leadtime_retailer <- rep(NA,length(leadtimes_retailer))
  for (i in (m+1):(length(leadtimes_retailer))) {
    train_leadtime <- leadtimes_retailer[(i-m):(i - 1)]
    forecast_leadtime_retailer[i] <- (((sum(train_leadtime))/m))
  }
  
  #Calculate lead time demand at the beginning of period t
  leadtime_demand_retailer <- rep(NA,length(forecast_leadtime_retailer))
  for (i in (max(m,n)+1):(length(forecast_leadtime_retailer)-max(leadtimes_retailer))) {
    leadtime_demand_retailer[i] <- sum(demand[i:(i + leadtimes_retailer[i] -1)])
  }
  
  #Forecast lead time demand
  leadtime_demand_forecast_retailer <- rep(NA, length(forecast_leadtime_retailer))
  for (i in (max(m,n)+1):(length(forecast_leadtime_retailer))) {
    leadtime_demand_forecast_retailer[i] <- (forecast_leadtime_retailer[i]*forecast_demand_retailer[i])
  }
  

  # Calculate Lead time demand errors
  leadtime_demand_errors_retailer <- rep(NA, length(forecast_leadtime_retailer))
  leadtime_demand_errors_retailer[1:(length(leadtimes_retailer)-max(leadtimes_retailer))] <- (leadtime_demand_retailer[1:(length(leadtimes_retailer)-max(leadtimes_retailer))] - leadtime_demand_forecast_retailer[1:(length(leadtimes_retailer)-max(leadtimes_retailer))])
  
  # Inventory level (position) to time t
  z <- 1.65 # specifies the probability 95% that demand is fulfilled by the on-hand inventory on normal z-score
  S_t <- rep(NA,length(leadtime_demand_forecast_retailer))
  for (i in ((max(n,m)+2)):(length(leadtime_demand_forecast_retailer))){
    S_t[i] <- leadtime_demand_forecast_retailer[i] + z*(sd(na.omit(leadtime_demand_errors_retailer[(max(n,m)+1):i])))
  }
  
  # Order-up-to-level policy quantity
  q_t_retailer <- rep(NA,length(leadtime_demand_forecast_retailer))
  for (i in ((max(n,m)+3):length(leadtime_demand_forecast_retailer))){
    q_t_retailer[i] <- S_t[i] - S_t[i-1] + demand[i-1]
  }
  
  # Bullwhip
  BM_retailer <- matrix(NA, length(leadtimes_retailer), 2)
  for (i in ((max(n,m)+3+max(leadtimes_retailer))):(length(leadtimes_retailer)-max(leadtimes_retailer))){
    BM_retailer[i,1] <- ((var(q_t_retailer[(max(n,m)+3):i]))/(var(demand[(max(n,m)+3):i])))
    BM_retailer[i,2]<- ((2*var(leadtimes_retailer[(max(n,m)+5):i])*(m+n-1))/((m**2)*(n**2)))+(((2*(mean(demand[(max(n,m)+5):i])**2))*var(leadtimes_retailer[(max(n,m)+5):i]))/((var(demand[(max(n,m)+5):i]))*(m**2)))+((2*(mean(leadtimes_retailer[(max(n,m)+5):i])**2))/(n**2))+((2*mean(leadtimes_retailer[(max(n,m)+5):i]))/n)+1
  }
  
  BM_sim_retailer <- ((var(q_t_retailer[(max(n,m)+3):length(leadtimes_retailer)]))/(var(demand)))
  BM_theoretical_retailer <- ((2*var(leadtimes_retailer[(max(n,m)+5):length(leadtimes_retailer)])*(m+n-1))/((m**2)*(n**2)))+(((2*(mean(demand[(max(n,m)+5):length(leadtimes_retailer)])**2))*var(leadtimes_retailer[(max(n,m)+5):length(leadtimes_retailer)]))/((var(demand[(max(n,m)+5):length(leadtimes_retailer)]))*(m**2)))+((2*(mean(leadtimes_retailer[(max(n,m)+5):length(leadtimes_retailer)])**2))/(n**2))+((2*mean(leadtimes_retailer[(max(n,m)+5):length(leadtimes_retailer)]))/n)+1

  return(list("BM_theoretical_retailer"=BM_theoretical_retailer, 
              "BM_sim_retailer"=BM_sim_retailer, 
              "replenishment_orders_retailer"=q_t_retailer,
              "BM_retailer"=BM_retailer))
}

#output <- Inventory_model_retailer(m = 1, n = 5, demand, leadtimes_retailer)
#output$BM_sim_retailer
#output$BM_theoretical_retailer

#x <- 1:5000
#plot(x, output$BM_retailer[,2], type = "l", lty = 1)
#lines(x, output$BM_retailer[,1], col = 'red')

#output_manu <- Inventory_model_MA(m = 5, n = 5, output$replenishment_orders_retailer, leadtimes_retailer)

#output_manu <- Inventory_model_MA(m = 5, n = 5, demand, leadtimes_retailer)
#output_manu$BM_sim_retailer
#output_manu$BM_theoretical_retailer

#x <- 1:5000
#plot(x, output_manu$BM_retailer[,2], type = "l", lty = 1)
#lines(x, output_manu$BM_retailer[,1], col = 'red')
