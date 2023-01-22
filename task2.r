list.of.packages <- c("quantmod", 'magrittr', 'xts')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(magrittr)
library(xts)
library(quantmod)

# getting data
file.name <- "F-F_Research_Data_Factors_daily.CSV"
factors <- read.csv(file = paste(file.name, sep = '/'), skip = 4) %>% na.omit() 
factors[,1] <- as.Date(factors[,1], format='%Y%m%d')
factors <- as.xts(factors[,-1], factors[,1])

data <- read.csv(file = paste("data.csv", sep = '/'), skip = 4) %>% na.omit() 
data <- as.xts(data[,-1], as.Date(data[,1]))

# Data preporation
newdata <- apply.monthly(data, last)
factors_sub <- factors[index(newdata)]

## Strategy implementation
getWeights <- function(data, factors) {
  k <- 1   
  a <- matrix(, nrow = nrow(newdata)-1, ncol = 12)
  while(k<nrow(newdata))
  {
    for (i in c(1,2,3,4,5,6,7,8,9,10,11,12)) 
      a[k,i] <- as.numeric(log(newdata[k+1,i]))-as.numeric(log(newdata[k,i]))
    k <- k+1
  }
  model_with_intercept <- lm(a[-1,]~factors_sub[,1]+factors_sub[,2]+factors_sub[,3]+factors_sub[,4]+0)
  r=c(12)
  b=rep(0,12)
  for (i in 1:12)
  r[i] <- model_with_intercept$coefficients[1,i]*factors_sub[nrow(factors_sub),1]+model_with_intercept$coefficients[2,i]*factors_sub[nrow(factors_sub),2]+model_with_intercept$coefficients[3,i]*factors_sub[nrow(factors_sub),3]+model_with_intercept$coefficients[4,i]*factors_sub[nrow(factors_sub),4]
 r.sort <- sort(r)
 ind=match(r.sort[7:12], r)
 b[ind] <- 1/6
  return b
  }


com.pct <- 0.0005               # commission on market value
holding.period <- 1
lookback <- 22
day <- lookback + 1             # Initialize first day, from wich we are going to backtest strategy 
trade.day <- day                # Initialize first day, on which the first trade will occur
money <- 1000000                # Set capital
expanding <- TRUE                     

backtest.data <- as.matrix(data[index(factors_sub)])
backtest.factors <- as.matrix(factors_sub)
pnl <- numeric(nrow(backtest.data)) # A vector which will collect PnL during backtesting
assets.count <- prev.assets.count <- rep(0, 12)

while(day <= nrow(backtest.data)) {
  pnl[day] <- sum((backtest.data[day, ] - backtest.data[day - 1, ])
                  * assets.count) 
  if (day < trade.day) {
    day <- day + 1 
    next
  }
  if(expanding){
    range <- 1:day
  }else{
    range <- (day - lookback):day
  }
  cur.weights <- getWeights(backtest.data[range, ], backtest.factors[range, ])
  assets.count <- (money * cur.weights / backtest.data[day, ]) %>% {sign(.) * floor(abs(.))}
  pnl[day] <- pnl[day] 
  #- # TODO commission com.pct * diffrence between market values of new position and old
  prev.assets.count <- assets.count
  day <- day + 1
  trade.day <- trade.day + holding.period
}

m <- pnl %>% {.[1] <- money; .} %>% cumsum 

pnl.xts <- xts(m, index(factors_sub))

plot(pnl.xts)

data.frame(PnL = diff(pnl.xts)["/2012-01-01"]) %>% na.fill(0) %>% set_colnames("PnL") %>% write.csv("train.csv")

data.frame(id = as.character(index(pnl.xts["2012-01-01/"])), PnL = diff(pnl.xts, na.pad = FALSE)["2012-01-01/"]) %>%
  set_colnames(c("id", "PnL")) %>% 
  write.csv("test.csv", row.names = FALSE)


