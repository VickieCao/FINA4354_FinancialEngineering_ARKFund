# Pricing functions for component options

# The function to calculate the price of a european call.
euro.put <- function(s0, sigma, r, d, T, K) { 
  d1 <- (log(s0 / K) + (r - d + (0.5) * sigma^2) * T)/(sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  K * exp(-r * T) * pnorm(-d2) - s0 * exp(-d * T) * pnorm(-d1)
}

# The function to calculate the price of a european put.
euro.call <- function(s0, sigma, r, d, T, K) {
  d1 <- (log(s0 / K)+(r - d + (0.5) * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  pnorm(d1) * s0 * exp(-d * T) - pnorm(d2) * K * exp(-r * (T))
}

# Use Monte Carlo simulation to price a put on a given put. 
# This put-on-put is an upward protection.
put.on.put <- function(s0, sigma, r, d, T, t, p, K, trials = 10000) {
  # T: maturity of put on put
  # t: maturity of underlying put
  # p: strike price of put on put
  dW <- rnorm(trials) * sqrt(T)
  sT <- s0 * exp((r - d - (1/2) * sigma^2) * T + sigma * dW)
  put.price <- euro.put(sT, sigma, r, d, t, K)
  payoff <- pmax(p - put.price,0)
  exp(-r * T) * mean(payoff)
}