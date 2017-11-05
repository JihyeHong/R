# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

# Data load
poloBid <- read.csv('~/Desktop/IT/R data/poloBid.csv', stringsAsFactors = F)
poloAsk  <- read.csv('~/Desktop/IT/R data/poloAsk.csv', stringsAsFactors = F)
btxBid <- read.csv('~/Desktop/IT/R data/btxBid.csv', stringsAsFactors = F)
btxAsk  <- read.csv('~/Desktop/IT/R data/btxAsk.csv', stringsAsFactors = F)

str(poloBid)
head(poloBid)

nrow(distinct(poloAsk))
nrow(distinct(poloBid))
nrow(distinct(btxAsk))

btxAsk <- distinct(btxAsk[,3:6])
btxBid <- distinct(btxBid[,3:6])
poloAsk <- distinct(poloAsk[,3:6])
poloBid <- distinct(poloBid[,3:6])

plot(btxAsk$created_time,btxAsk$rate)
summary(btxAsk)

#Character conversion to date
btxAsk$created_time2 <- as.POSIXlt(btxAsk$created_time)
btxBid$created_time2 <- as.POSIXlt(btxBid$created_time)
poloAsk$created_time2 <- as.POSIXlt(poloAsk$created_time)
poloBid$created_time2 <- as.POSIXlt(poloBid$created_time)

btxAsk <- btxAsk[btxAsk$created_time2>'2017-10-09',]
btxBid <- btxBid[btxBid$created_time2>'2017-10-09',]
poloAsk <- poloAsk[poloAsk$created_time2>'2017-10-09',]
poloBid <- poloBid[poloBid$created_time2>'2017-10-09',]

#plotting
ggplot(btxAsk, aes(created_time2,rate)) + geom_line() + xlab("") + ylab(" ")

poloAsk$exchange_id <- 'PLN'
btxBid$exchange_id <- 'BTX'

poloBid$exchange_id <- 'PLN'
btxAsk$exchange_id <- 'BTX'

#Commission rate calculation
poloAsk$rateWithComs <- poloAsk$rate*1.005
btxAsk$rateWithComs <- btxAsk$rate*1.005

btxAskComs <- btxAsk
btxAskComs$rate <- btxAskComs$rateWithComs

btxAsk <- btxAsk[,1:6]
btxAskComs <- btxAsk
btxAskComs$rate <- btxAskComs$rate*1.005
btxAskComs$exchange_id <- 'BTXC'

poloAskComs <- poloAsk
poloAskComs$rate <- poloAskComs$rateWithComs

poloAsk <- poloAsk[,1:6]
poloAskComs <- poloAsk
poloAskComs$rate <- poloAskComs$rate*1.005
poloAskComs$exchange_id <- 'PLNC'


#time-series line graphs 
#poloniex ask price to bittrex bid price
poloToBtx <- rbind(poloAsk,btxBid,poloAskComs)
a <- ggplot(data = poloToBtx, aes(x=created_time2, y=rate)) + geom_line(aes(colour=exchange_id)) 
a + xlab("datetime") + ylab("rate") + ggtitle("From poloniex to Bittrex")

#bittrex ask price to poloniex bid price 
BtxToPolo <- rbind(poloBid,btxAsk,btxAskComs)
b <- ggplot(data = BtxToPolo, aes(x=created_time2, y=rate)) + geom_line(aes(colour=exchange_id)) 
b + xlab("datetime") + ylab("rate") + ggtitle("From Bittrex to Poloniex") #scale_color_manual(values=c("#CC6666", "#9999CC"))
