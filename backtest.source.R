backtest <- function(historical.price, days, r, d, coupon, sigma, initial.price, management.fee, c) {
  
  k1 <- c * historical.price[1,1]   # Strike price of first put
  k2 <- c * historical.price[127,1] # Strike price of second put
  k3 <- c * historical.price[253,1] # Strik3 price of third put
  k4 <- c * historical.price[379,1] # Strike price of final put
  p  <- euro.put(historical.price[1,1], sigma, r, d, 0.5, c*initial.price) # Strike price of put on put
  
  cash[1,1] <- initial.price - p - 
    put.on.put(historical.price[1,1], sigma, r, d, 0.5, 0.5, p, k1) - 
    put.on.put(historical.price[1,1], sigma, r, d, 1, 0.5, p, k1) - 
    put.on.put(historical.price[1,1], sigma, r, d, 1.5, 0.5, p, k1)
  
  # Portfolio value is updated for each trading day
  for (j in 2:505) {
    # Day 2 ~ 126, period 0, the first half year:
    if (j <= 126) {
      value[j,1] <- euro.put(historical.price[j,1], sigma, r, d, 0.5-j/252, k1) +
        put.on.put(historical.price[j,1], sigma, r, d, 0.5-j/252, 0.5, p, k1) + 
        put.on.put(historical.price[j,1], sigma, r, d, 1-j/252, 0.5, p, k1) +
        put.on.put(historical.price[j,1], sigma, r, d, 1.5-j/252, 0.5, p, k1)
      cash[j,1]  <- cash[j-1,1] * exp(r * (1 / 252))
      total.value[j,1] <- value[j,1] + cash[j,1]
    } 
    # Day 127:
    else if (j == 127) {
      value[j,1] <- euro.put(historical.price[j,1], sigma, r, d, 1-j/252, k2) + 
        put.on.put(historical.price[j,1], sigma, r, d, 1-j/252, 0.5, p, k1) + 
        put.on.put(historical.price[j,1], sigma, r, d, 1.5-j/252, 0.5, p, k1)
      cash[j,1]  <- cash[j-1,1] * exp(r * (1 / 252)) - p + max(k1 - historical.price[127,1], 0) - 
        initial.price * coupon
      total.value[j,1]  <- value[j,1] + cash[j,1] + initial.price * coupon
      bank.revenue[1,1] <- max(euro.put(historical.price[1,1], sigma, r, d, 0.5, c*historical.price[1,1]) - 
                                 euro.put(historical.price[j,1], sigma, r, d, 0.5, c*historical.price[1,1]),0) + 
        euro.put(historical.price[1,1], sigma, r, d, 0.5, c*historical.price[1,1]) - 
        euro.put(historical.price[j,1], sigma, r, d, 0.5, c*historical.price[j,1])
    }
    # Day 128 ~ 252, period 1, the second half year:
    else if (j <= 252) {
      value[j,1] <- euro.put(historical.price[j,1], sigma, r, d, 1-j/252, k2) + 
        put.on.put(historical.price[j,1], sigma, r, d, 1-j/252, 0.5, p, k1) + 
        put.on.put(historical.price[j,1], sigma, r, d, 1.5-j/252, 0.5, p, k1)
      cash[j,1]  <- cash[j-1,1] * exp(r * (1 / 252)) 
      total.value[j,1] <- value[j,1] + cash[j,1] + initial.price * coupon
    }
    # Day 253:
    else if (j == 253) {
      value[j,1] <- euro.put(historical.price[j,1], sigma, r, d, 1.5-j/252, k3) +
        put.on.put(historical.price[j,1], sigma, r, d, 1.5-j/252, 0.5, p, k1)
      cash[j,1]  <- cash[j-1,1] * exp(r * (1 / 252)) - p + max(k2 -historical.price[253,1], 0) - 
        initial.price * coupon
      total.value[j,1]  <- value[j,1] + cash[j,1] + initial.price * coupon * 2
      bank.revenue[2,1] <- max(euro.put(historical.price[1,1], sigma, r, d, 0.5, c*historical.price[1,1]) - 
                                 euro.put(historical.price[j,1], sigma, r, d, 0.5, c*historical.price[1,1]),0) + 
        euro.put(historical.price[1,1], sigma, r, d, 0.5, c*historical.price[1,1]) - 
        euro.put(historical.price[j,1], sigma, r, d, 0.5, c*historical.price[j,1])
    }
    # Day 254 ~ 378, period 3, the third half year:
    else if (j <= 378) {
      value[j,1] <- euro.put(historical.price[j,1], sigma, r, d, 1.5-j/252, k3) + 
        put.on.put(historical.price[j,1], sigma, r, d, 1.5-j/252, 0.5, p, k1)
      cash[j,1]  <- cash[j-1,1] * exp(r * (1 / 252)) 
      total.value[j,1] <- value[j,1] + cash[j,1] + initial.price * coupon * 2
    }
    # Day 379:
    else if (j == 379) {
      value[j,1] <- euro.put(historical.price[j,1], sigma, r, d, 2-j/252, k4) 
      cash[j,1]  <- cash[j-1,1] * exp(r * (1 / 252)) - p + max(k3 -historical.price[379,1], 0) - 
        initial.price * coupon
      total.value[j,1]  <- value[j,1] + cash[j,1] + initial.price * coupon * 3
      bank.revenue[3,1] <- max(euro.put(historical.price[1,1], sigma, r, d, 0.5, c*historical.price[1,1]) - 
                                 euro.put(historical.price[j,1], sigma, r, d, 0.5, c*historical.price[1,1]),0) + 
        euro.put(historical.price[1,1], sigma, r, d, 0.5, c*historical.price[1,1]) - 
        euro.put(historical.price[j,1], sigma, r, d, 0.5, c*historical.price[j,1])
    }
    # Day 380 ~ 504, period 4, the last half year:
    else if (j <= 504) {
      value[j,1] <- euro.put(historical.price[j,1], sigma, r, d, 2-j/252, k4)  
      cash[j,1]  <- cash[j-1,1] * exp(r*(1/252)) 
      total.value[j,1] <- value[j,1] + cash[j,1] + initial.price * coupon * 3
    }
    # Day 505, the last day:
    else if (j == 505) {
      value[j,1] <- 0
      cash[j,1]  <- cash[j-1,1] * exp(r * (1 / 252)) + max(k4 - historical.price[505,1], 0) 
      total.value[j,1] <- cash[j,1] + initial.price * coupon * 3
    }
    
    # We set a downward knock out rate of 95%. If the value of our portfolio is below 95% 
    # , we clear out all position and invest in bond to make pricipal guaranteed.
    if (!exists("period", mode = "function")) {
      period <- function(j) {
        if (j <= 127) {period <- 0} 
        else if (j <= 253) {period <- 1} 
        else if (j <= 379) {period <- 2} 
        else if (j <= 505) {period <- 3}
      }
    }
    if (total.value[j,1] < 0.95 * initial.price * exp(-r*(2-j/252))) {
      cash[j,1] <- cash[j,1] + value[j,1]
      value[j:505,1] <- 0
      for (n in (j+1):505) {
        cash[n,1] <- cash[n-1,1] * exp(r * 1 / 252)
        total.value[n,1] <- cash[n,1] + period(j) * coupon * initial.price
      }
      bank.revenue[(period(j)+1):4,1] <- 0
      print(paste("Knock out in day:",j))
      knockout.date <- j
    }
    
    if (knockout.date != 0) {
      break
    }
  }
  
  print(paste("Backtest successful!"))
  
  # Store simulation results in data frames
  RETURN      <<- c(RETURN, (total.value[505,1]/initial.price - 1))
  KNOCKOUT    <<- c(KNOCKOUT, knockout.date)
  PRICE       <<- cbind(PRICE, historical.price)
  CASH        <<- cbind(CASH, cash)
  VALUE       <<- cbind(VALUE, value)
  TOTAL.VALUE <<- cbind(TOTAL.VALUE, total.value)
  BANK        <<- cbind(BANK, bank.revenue)
}