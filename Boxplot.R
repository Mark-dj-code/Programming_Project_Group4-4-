transactions <- read.csv("all_transactions.csv")
Stockprices <- read.csv("stock_prices_2023_to_now.csv")
congresstrades <- read.csv("Copyofcongress-trading-all.csv")
Sp500 <- read.csv("sp-500-historical-annual-returns.csv")
install.packages("ggplot2")
install.packages("maps")
install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)
library(ggplot2)

top_states <- congresstrades %>%
  filter(!is.na(State)) %>%
  group_by(State) %>%
  summarise(total_trades = n()) %>%
  slice_max(total_trades, n = 5) %>%
  pull(State) 

filtered_data <- congresstrades %>%
  filter(State %in% top_states)

ggplot(filtered_data, aes(x = State, y = excess_return, fill = State)) +  
  geom_boxplot() +
  labs(title = "Top 5 States by Number of Trades",
       x = "State",
       y = "Returns",
       fill = "State"
       )+
  theme_minimal() +
  theme(
    legend.position = "right"
  )
