
library(xts)
library(highfrequency)
library(quantmod)
library(repr)
library(zoo)

rm(list = ls())


#combine tick and quote data

tq = matchTradesQuotes(sampleTData, sampleQData)

tq = xts(tq, order.by = tq$DT)
tq[,c('BID', 'PRICE', 'OFR', 'OFRSIZ', 'BIDSIZ', 'MIDQUOTE', 'SIZE')] = as.numeric(tq[,c('BID', 'PRICE', 'OFR', 'OFRSIZ', 
                                                                                            'BIDSIZ', 'MIDQUOTE', 'SIZE')])

#trade metrics
cat('Within Bid-Ask: ',
   sum((tq$PRICE < tq$OFR) & (tq$PRICE > tq$BID)) / nrow(tq))

cat(' At Ask: ',
   sum(tq$PRICE == tq$OFR) / nrow(tq))

cat(' At Bid: ',
   sum(tq$PRICE == tq$BID) / nrow(tq))


#TICK TEST
prices_l1 = lags(tq$PRICE, n = 1)
mode(prices_l1) = 'numeric'
delt = prices_l1[,1] - prices_l1[,2]
test = lapply(delt, function(x) x/abs(x) )
test = c(1, unlist(test))
tt = na.locf(test)


#LEE-READY
mat = matrix(tq[,c('PRICE','MIDQUOTE')], ncol = 2) #wish i could get rid
mode(mat) = 'numeric' #wish i could get rid
delt = mat[,1] - mat[,2] 
test = lapply(delt, function(x) x/abs(x) )
n = matrix(c(unlist(test), tt), ncol = 2)
lee_ready = apply(n, 1,function(x) ifelse(TRUE, x[2], x[1]) )
]

#Check
setequal(getTradeDirection(tq), lee_ready)
