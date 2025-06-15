####################################################
##
##
## Map of average excess return per state
##
##
#####################################################

### average market returns over the years (2012 to 2024)

average_market_return_over_time <- mean(SP_500_annual_returns$SP_500_return[85:98])



## group data by state in new dataframe, sum abnormal return size over states for all years

library(tidyverse)
library(dplyr)

returns_by_state <- senators_full_data |>
  group_by(State) |>
  summarize(weighted_average_return_per_state = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop")


### new variable is difference between average return per state and average market return

returns_by_state <- returns_by_state |>
  mutate( State_average_abnormal_return = 100*( weighted_average_return_per_state - average_market_return_over_time )/abs(average_market_return_over_time))

### remove alaska and hawaii

returns_by_state <- returns_by_state |>
  filter(!(State %in% c("Alaska", "Hawaii")))

####### install shapefile pacakges to get US states Map


devtools::install_github("ropensci/rnaturalearthhires")

library(rnaturalearth)



## import shapefile object of US states map: 

us_states_map = ne_states(country ="united states of america", returnclass = "sf" )


################## create heatmap

# merge map and returns per state data

us_states_returns_map <- us_states_map |>
  inner_join(returns_by_state, by = c("name" = "State"))


#plot merged data

ggplot(us_states_returns_map, aes(fill = State_average_abnormal_return)) +
  geom_sf( color = "white", size = 0.2 ) +
  scale_fill_gradient(low = "white", high = "red",
                      name = "Average market\noutperformance per state\n(in%) from 2012 to 2024"
                      )

