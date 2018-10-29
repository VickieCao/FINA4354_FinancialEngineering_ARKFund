library("data.table")
library("readxl")
# Store all files in directory "./Simulations", sore graphs in "./Simulations/Graph"
dir.create("./Simulations")
dir.create("./Simulations/Graph")

# PartI. Include pricing functions for component options
if ((!exists("euro.put",   mode = "function")) &&
	  (!exists("euro.call",  mode = "function")) &&
  	(!exists("put.on.put", mode = "function"))) {
	source("./options.R")
}

# PartII. Pricing for our structured product
# Parameters of component options
trading.days.yr <- 252 
days   <- trading.days.yr * 2 + 1 # The maturity of the structured product
r      <- 0.008  # Risk-free Rate
d      <- 0.018  # Annualized HSI dividend yield
coupon <- 0.1/3  # Coupon rate
sigma  <- 0.2407 # HSI volatility
initial.price  <- 30708 # The current value of HSI
management.fee <- 0     # 0 management fee
c      <- 0.85    # A ratio to adjust the floating strike price of puts.

# Initialize data frames.
PRICE <- data.frame(matrix(nrow = days, ncol = 0))
CASH  <- data.frame(matrix(nrow = days, ncol = 0))
VALUE <- data.frame(matrix(nrow = days, ncol = 0))
TOTAL.VALUE <- data.frame(matrix(nrow = days, ncol = 0))
BANK     <- data.frame(matrix(nrow = 4, ncol = 0))
KNOCKOUT <- c()
RETURN   <- c()

# Number of trials
times <- 5

for (a in 1:times) {
  # Set up temp data frames for a simulation, release these memory when one simulation is completed
  price <- data.frame(rep(0, days)) # HSI 
  value <- data.frame(rep(0, days)) # Portfolio value
  cash  <- data.frame(rep(0, days)) # Investment in risk-free asset
  total.value  <- data.frame(rep(0,days)) # Total value of our portfolio
  bank.revenue <- data.frame(rep(0,4))
  knockout.date <- 0
  price[1,1] <- initial.price

  if (!exists("simulation", mode = "function")) {
    source("simulation.R")
  }
  simulation(days, r, d, coupon, sigma, initial.price, management.fee, c)
  
  # Ploting for each simulation
  png(paste("./Simulations/Graph/", a, ".png", sep=""))
  par(mfrow=c(2,2))
  plot(PRICE[,a], main="price", type = "l")
  plot(VALUE[2:505,a], main="value")
  plot(CASH[,a], main="cash")
  plot(TOTAL.VALUE[2:505,a], main = "total value")
  dev.off()
}

write.csv(CASH, file = "./Simulations/CASH.csv")
write.csv(PRICE, file = "./Simulations/PRICE.csv")
write.csv(VALUE, file = "./Simulations/VALUE.csv")
write.csv(TOTAL.VALUE, file = "./Simulations/TOTAL.csv")
write.csv(BANK, file = "./Simulations/BANK.csv")
write.csv(KNOCKOUT, file = "./Simulations/KNOCKOUT.csv")
write.csv(RETURN, file = "./Simulations/RETURN.csv")
