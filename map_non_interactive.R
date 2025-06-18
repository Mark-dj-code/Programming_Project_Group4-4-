transactions <- read.csv("all_transactions.csv")
Stockprices <- read.csv("stock_prices_2023_to_now.csv")
congresstrades <- read.csv("Copyofcongress-trading-all.csv")
Sp500 <- read.csv("sp-500-historical-annual-returns.csv")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("maps")

library(ggplot2)
library(dplyr)
library(maps)

us_states <- map_data("state")
congresstrades$state_lower <- tolower(congresstrades$State)

state_trade_counts <- congresstrades %>%
  filter(!is.na(state_lower)) %>%
  filter(Transaction =="Sale")%>%
  group_by(state_lower) %>%
  summarise(num_trades = n())

map_data_joined <- left_join(us_states, state_trade_counts, by = c("region" = "state_lower"))

ggplot(map_data_joined, aes(x = long, y = lat, group = group, fill = num_trades)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  labs(title = "Number of Trades by State", fill = "Trade Count") +
  theme_minimal()