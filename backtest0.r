# Loading required packages
list.of.packages <- c("quantmod", 'magrittr', 'xts', 'dplyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(quantmod)
library(magrittr)
library(xts)
library(dplyr)


# Download data
#asset1 <- quantmod::getSymbols("IWM", auto.assign = FALSE, from = "2005-01-01", to = "2019-04-02") %>% 
 #   Ad %>% set_names("IWM")
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# UNCOMMENT THIS LINE FOR HW
asset1 <- quantmod::getSymbols("GLD", auto.assign = FALSE, from = "2005-01-01", to = "2019-04-02") %>%
    Ad %>% set_names("GLD")
asset2 <- quantmod::getSymbols("SPY", auto.assign = FALSE, from = "2005-01-01", to = "2019-04-02") %>% 
    Ad %>% set_names('SPY')
asset3 <- quantmod::getSymbols("TLT", auto.assign = FALSE, from = "2005-01-01", to = "2019-04-02") %>% 
    Ad %>% set_names("TLT")

# Merging data
data <- na.omit(cbind(asset1, asset2, asset3))


#strategy params


holding.period <-21


# Calculating scores for each assets 

# Function for intro lecture
#getWeights <- function(data) {
 #   short.period <- 21
  #  middle.period <- 62
   # long.period <- 123
    #i <- nrow(data)
    #scores <- (data[i,] - data[i - long.period,]) / data[i - long.period,] + 
     #   (data[i,] - data[i - short.period,]) / data[i - short.period,] + 
      #  (data[i,] - data[i - middle.period,]) / data[i - middle.period,]
  #  
   # if (scores[1] < 0 & scores[2] < 0) {return(c(0, 0, 1))}
    #if (scores[1] > scores[2]) {return(c(1, 0, 0))}
    #if (scores[1] < scores[2]) {return(c(0, 1, 0))}
#}

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# In HW YOU should uncomment this function and define it! 
getWeights <- function(datas) {
  k <- 1   
  a <- matrix(, nrow = nrow(datas)-1, ncol = 3)
  while(k<nrow(datas))
  {
  for (i in c(1,2,3)) 
   a[k,i] <- log(datas[k+1,i])-log(datas[k,i])
   k <- k+1
  }
  vec1 <- c(mean(a[,1]), mean(a[,2]), mean(a[,3]))
  covmatrix <- solve(matrix(c(cov(a)), nrow=3, ncol=3))
  rr <- covmatrix %*% vec1
  return (rr/(abs(rr[1])+abs(rr[2])+abs(rr[3])))
   }




# check for working 
#res <- getWeights(backtest.data)


#lookback <- 123

# UNCOMMENT THIS FOR HOMEWORK !!!!!!!!!
lookback <- 250
day <- lookback + 1                 # Initialize first day, from wich we are going to backtest strategy 
trade.day <- day                    # Initialize first day, on which the first trade will occur
money <- 1000000                    # Set capital
hold.index <- 0                     # Number of days, during which the assets will hold
backtest.data <- as.matrix(data) 
pnl <- numeric(nrow(backtest.data)) # A vector which will collect PnL during backtesting


#range <- (day - lookback):day
# CHANGE DAYS RANGE FOR HW !!!!!!!!!!
range <- 1:251
cur.weights <- getWeights(backtest.data[range, ])
assets.count <- floor(money * cur.weights / backtest.data[day, ])
day <- day + 1
trade.day <- trade.day + holding.period

while(day <= nrow(backtest.data)) {
    # calculate profit and loss every day
    pnl[day] <- sum((backtest.data[day, ] - backtest.data[day - 1, ] )
                      * assets.count)
    # if holding period do not pass hold the position
    if (day < trade.day) {
        day <- day + 1 
        next
    }
    
    # else get new weights
    range <- 1:day
    # CHANGE DAYS RANGE FOR HW !!!!!!!!!!
    # range <- ...
    cur.weights <- getWeights(backtest.data[range, ])
    assets.count <- floor(money * cur.weights / backtest.data[day, ])
    day <- day + 1
    trade.day <- trade.day + holding.period
}



plot(pnl)
m <- pnl %>% {.[1] <- money; .} %>% cumsum 
plot(m, type = 'l')
mean(ROC(m, na.pad = FALSE)) / sd(ROC(m, na.pad = FALSE)) * sqrt(252)

pnl.xts <- xts(m, index(data))

plot(pnl.xts)

data.frame(PnL = diff(pnl.xts)["/2012-01-01"]) %>% na.fill(0) %>% set_colnames("PnL") %>% write.csv("train.csv")

data.frame(id = as.character(index(pnl.xts["2012-01-01/"])), PnL = diff(pnl.xts, na.pad = FALSE)["2012-01-01/"]) %>%
    set_colnames(c("id", "PnL")) %>% 
    write.csv("test.csv", row.names = FALSE)





