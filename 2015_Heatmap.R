########################## 2015 heatmap (abnormal eturnn per state)

return_per_state_2015 <- senators_full_data |> 
  filter(Trade_year == "2015")|>
  group_by(State)|>
  summarize(weighted_average_return = sum(Trade_Size_USD*scaled_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )
 
### no need to calculate abnormal return size (ie difference with market return, rankimg would stay the same since for a single year)

## remove Hawaii and Alaska

### remove alaska and hawaii

return_per_state_2015 <- return_per_state_2015 |>
  filter(!(State %in% c("Alaska", "Hawaii")))

####### install shapefile pacakges to get US states Map


devtools::install_github("ropensci/rnaturalearthhires")

library(rnaturalearth)



## import shapefile object of US states map: 

us_states_map = ne_states(country ="united states of america", returnclass = "sf" )


################## create heatmap

# merge map and returns per state data

us_states_returns_map_2015 <- us_states_map |>
  left_join(return_per_state_2015, by = c("name" = "State")) |>
  mutate(Return = replace_na(weighted_average_return, 0))|> ## replace NA for missing states with 0
  filter(!(name %in% c("Alaska", "Hawaii")))




### plot heatmap

ggplot(us_states_returns_map_2015, aes(fill = weighted_average_return)) +
  geom_sf( color = "orange", size = 0.2 ) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white",
                      name = "Excess return\nby state in \n2015 (%)"
  )

