###### sort individual stocks

###### extract stock tickers in separate data frame 

stocks <- senators_full_data["Ticker"]
class(stocks)

###### sort stocks alphabetically

stocks <- stocks[order(stocks$Ticker),]
as.data.frame(stocks)
stocks<-as.data.frame(stocks)
View(stocks)

###### eliminate duplicates

library(dplyr)

stocks <- distinct(stocks)


##################### get returns for individual senators










