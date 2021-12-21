#Inventory model
library(forecast)
library(inventorize)

#leadtimes_retailer <- round(runif(5000, min = 1, max = 7))

inventory_model_season_deterministic<- function(n, p, demand, normal_LT, winter_LT){
  
  #Retailer
  #Generate forecast demand with moving average method with delay param n
  forecast_demand_retailer <- rep(NA,length(demand))
  for (i in (n+1):(length(demand))) {
    train_demand <- demand[(i - n):(i - 1)]
    forecast_demand_retailer[i] <- sum(train_demand)/n
  }  
  
  
  
  
  #Calculate lead time demand at the beginning of period t
  leadtime_demand_retailer <- rep(NA,length(demand))
  for (i in (n+1):(length(demand)-max(winter_LT))) {
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
      leadtime_demand_retailer[i] <- sum(demand[i:(i + winter_LT -1)])
    } else {
      leadtime_demand_retailer[i] <- sum(demand[i:(i + normal_LT -1)]) 
    }
  }

  
  
  #Forecast lead time demand
  leadtime_demand_forecast_retailer <- rep(NA, length(demand))
  season <- rep(NA, length(demand))
  for (i in (n+1):(length(demand))) {
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
      leadtime_demand_forecast_retailer[i] <- (winter_LT*forecast_demand_retailer[i])
      season[i] <- "winter"
    } else {
      leadtime_demand_forecast_retailer[i] <- (normal_LT*forecast_demand_retailer[i])
      season[i] <- "normal"
    }
  }
  
  
  # Calculate Lead time demand errors
  leadtime_demand_errors_retailer <- rep(NA, length(demand))
  leadtime_demand_errors_retailer[1:(length(demand)-winter_LT)] <- (leadtime_demand_retailer[1:(length(demand)-winter_LT)] - leadtime_demand_forecast_retailer[1:(length(demand)-winter_LT)])
  
  
  # Inventory level (position) to time t
  z <- qnorm(p) # specifies the probability 95% that demand is fulfilled by the on-hand inventory on normal z-score
  S_t <- rep(NA,length(demand))
  for (i in ((n+2):(length(demand)))){
    S_t[i] <- leadtime_demand_forecast_retailer[i] + z*(sd(na.omit(leadtime_demand_errors_retailer[(n+1):i])))
  }
  
  
  
  
  # Order-up-to-level policy quantity
  q_t_retailer <- rep(NA,length(demand))
  for (i in ((n+3):length(demand))){
    q_t_retailer[i] <- S_t[i] - S_t[i-1] + demand[i-1]
  }
  
  
  # Bullwhip
  BM_retailer <- rep(NA, length(demand))
  for (i in (((n+4)):(length(demand)-winter_LT))){
    BM_retailer[i] <- ((var(q_t_retailer[(n+3):i]))/(var(demand[(n+3):i])))
  }

  BM_yearly <- matrix(NA, length(demand), 1)
for (j in 1:10){
  for (i in ((n+3+winter_LT+(365*(j-1))):(n+3+winter_LT+(365*j)))){
    BM_yearly[i] <- ((var(q_t_retailer[(n+3+winter_LT+(365*(j-1))):i]))/(var(demand[(n+3+winter_LT+(365*(j-1))):i])))
  }
}
  
  
  BM_sim <- ((var(q_t_retailer[(n+3):length(demand)]))/(var(demand)))

  return(list("BM_sim"=BM_sim, 
              "replenishment_orders_retailer"=q_t_retailer,
              "BM_retailer"=BM_retailer,
              "BM_yearly"=BM_yearly,
              "errors" = leadtime_demand_errors_retailer,
              "season" = season,
              "leadtime_demand_retailer"=leadtime_demand_retailer))
}
