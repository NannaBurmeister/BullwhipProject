#Inventory model
library(forecast)
library(inventorize)
library(stats)
library(tsibble)

inventory_model_season_stochastic_holiday <- function(n, m, p, demand, leadtimes_retailer, season){
  
  #Retailer
  #Generate forecast demand with moving average method with delay param n
  forecast_demand_retailer <- rep(NA,length(demand))
  for (i in (n+1):(length(demand))) {
    train_demand <- demand[(i - n):(i - 1)]
    forecast_demand_retailer[i] <- sum(train_demand)/n
  }  
  
  
  #Observed lead times
  observed_leadtimes <- (leadtimes_retailer + (1:length(leadtimes_retailer)))
  
  #Generate forecast leadtime with moving average method with delay param m
  #Forecasts in R build ion frequency, but i have to seasonal periods during the year
  forecast_leadtime_retailer <- rep(NA,length(leadtimes_retailer))
#  for (i in (((365*15)+1):(length(leadtimes_retailer)))) {
#    train_leadtime <- leadtimes_retailer[observed_leadtimes <= (i-1)]
#    forecast_leadtime_retailer[i] <- forecast(HoltWinters(ts(train_leadtime, frequency = 365)), h=1)$mean
#  }

  #Seasonal moving average
  for (i in (((365*5)+1):(length(leadtimes_retailer)))) {
    if(season[i]=="normal"){
    temp <- leadtimes_retailer[(observed_leadtimes <= (i-1)) & (season == "normal")]
    train_leadtime <-  tail(temp,m)
    forecast_leadtime_retailer[i] <- sum(train_leadtime)/m
  }
    else {
      temp <- leadtimes_retailer[(observed_leadtimes <= (i-1)) & (season == "winter")]
      train_leadtime <-  tail(temp,m)
      forecast_leadtime_retailer[i] <- sum(train_leadtime)/m
    }
  }
  

  #Calculate lead time demand at the beginning of period t
  leadtime_demand_retailer <- rep(NA,length(demand))
  for (i in (n+1):(length(demand)-max(leadtimes_retailer[observed_leadtimes<=length(demand)]))) {
      leadtime_demand_retailer[i] <- sum(demand[i:(i + leadtimes_retailer[i] -1)])
  }
  
  
  
  
  #Forecast lead time demand
  leadtime_demand_forecast_retailer <- rep(NA, length(demand))
  for (i in ((365*5)+max(n,m)+1):(length(demand))) {
      leadtime_demand_forecast_retailer[i] <- (forecast_leadtime_retailer[i]*forecast_demand_retailer[i])
  }
  
  
  
  # Calculate Lead time demand errors
  leadtime_demand_errors_retailer <- rep(NA, length(demand))
  leadtime_demand_errors_retailer[1:(length(demand)-(max(leadtimes_retailer[observed_leadtimes>length(demand)])))] <- (leadtime_demand_retailer[1:(length(demand)-(max(leadtimes_retailer[observed_leadtimes>length(demand)])))] - leadtime_demand_forecast_retailer[1:(length(demand)-(max(leadtimes_retailer[observed_leadtimes>length(demand)])))])
  
  
  # Inventory level (position) to time t
  z <- qnorm(p) # specifies the probability 95% that demand is fulfilled by the on-hand inventory on normal z-score
  S_t <- rep(NA,length(demand))
  for (i in ((365*5)+max(n,m)+2):(length(demand))){
    S_t[i] <- leadtime_demand_forecast_retailer[i] + z*(sd(na.omit(leadtime_demand_errors_retailer[(n+1):i])))
  }
  
  # Order-up-to-level policy quantity
  q_t_retailer <- rep(NA,length(demand))
  for (i in (((365*5)+max(n,m)+3):(length(demand)))){
    q_t_retailer[i] <- S_t[i] - S_t[i-1] + demand[i-1]
  }
  
  
  # Bullwhip
  BM_retailer <- rep(NA, length(demand))
  for (i in (((365*5)+max(n,m)+4):(length(demand)-(round(max(leadtimes_retailer[observed_leadtimes>length(demand)])))))){
    BM_retailer[i] <- ((var(q_t_retailer[((365*5)+max(n,m)+3):i]))/(var(demand[((365*5)+max(n,m)+3):i])))
  }
  
  
  BM_yearly <- matrix(NA, length(demand), 1)
for (j in (1:5)){
    for (i in (((max(n,m)+4+(365)*(4+j))):(max(n,m)+4+((365)*(4+j+1))))){
      BM_yearly[i] <- (((var(q_t_retailer[(((365)*(4+j)+max(n,m)+3):i)]))/(var(demand[((((365)*(4+j+1))+max(n,m)+3):i)]))))
    }
  }
  
  
  BM_sim <- ((var(q_t_retailer[(max(n,m)+4+(365*5)):length(demand)]))/(var(demand)))
  
  return(list("BM_sim"=BM_sim, 
              "replenishment_orders_retailer"=q_t_retailer,
              "BM_retailer"=BM_retailer,
              "BM_yearly"=BM_yearly,
              "errors" = leadtime_demand_errors_retailer,
              "season" = season,
              "leadtime_demand_retailer"=leadtime_demand_retailer))
}

