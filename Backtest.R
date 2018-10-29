library("data.table")
library("readxl")
# Store all files in directory "./Backtests", sore graphs in "./Backtests/Graph"
dir.create("./Backtests")
dir.create("./Backtests/Graph")

# PartI. Include pricing functions for component options
if ((!exists("euro.put",   mode = "function")) &&
    (!exists("euro.call",  mode = "function")) &&
    (!exists("put.on.put", mode = "function"))) {
  source("./options.R")
}

# PartII. Load in historical HSI price
bear <- read_excel("./BacktestBear.xlsx")
bull <- read_excel("./BacktestBull.xlsx")

# PartIII. Backtesting for our structured product
# Parameters of component options
trading.days.yr <- 252 
days   <- trading.days.yr * 2 + 1 # The maturity of the structured product
r      <- 0.008  # Risk-free Rate
d      <- 0.018  # Annualized HSI dividend yield
coupon <- 0.1/3  # Coupon rate
sigma  <- 0.2407 # HSI volatility
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

# Set up temp data frames for a simulation
historical.price <- data.frame(rep(0, days)) # HSI 
value <- data.frame(rep(0, days)) # Portfolio value
cash  <- data.frame(rep(0, days)) # Investment in risk-free asset
total.value  <- data.frame(rep(0,days)) # Total value of our portfolio
bank.revenue <- data.frame(rep(0,4))
knockout.date <- 0

if (!exists("backtest.function", mode = "function")) {
  source("backtest.source.R")
}

# Backtesting for 2007~2009 bear market
# Load in historical stock price
for (i in 1:days) {historical.price[i,1] <- bear$Price[i]}
initial.price <- bear$Price[1]
backtest(historical.price, days, r, d, coupon, sigma, initial.price, management.fee, c)

# Backtesting for 2016~2018 bull market
# Load in historical stock price
for (i in 1:days) {historical.price[i,1] <- bull$Price[i]}
initial.price <- bull$Price[1]
historical.price[1,1] <- initial.price
backtest(historical.price, days, r, d, coupon, sigma, initial.price, management.fee, c)

# Ploting for each simulation
png(paste("./Backtests/Graph/Bear.png", sep=""))
par(mfrow=c(2,2))
plot(PRICE[,1], main="price", type = "l")
plot(VALUE[2:505,1], main="value")
plot(CASH[,1], main="cash")
plot(TOTAL.VALUE[2:505,1], main = "total value")
dev.off()
png(paste("./Backtests/Graph/Bull.png", sep=""))
par(mfrow=c(2,2))
plot(PRICE[,2], main="price", type = "l")
plot(VALUE[2:505,2], main="value")
plot(CASH[,2], main="cash")
plot(TOTAL.VALUE[2:505,2], main = "total value")
dev.off()

write.csv(CASH, file = "./Backtests/CASH.csv")
write.csv(PRICE, file = "./Backtests/PRICE.csv")
write.csv(VALUE, file = "./Simulations/VALUE.csv")
write.csv(TOTAL.VALUE, file = "./Backtests/TOTAL.csv")
write.csv(BANK, file = "./Backtests/BANK.csv")
write.csv(KNOCKOUT, file = "./Backtests/KNOCKOUT.csv")
write.csv(RETURN, file = "./Backtests/RETURN.csv")
