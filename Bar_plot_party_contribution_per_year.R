##### bar plot for senate average return by year per party


## gettotal  trade size for each year

Yearly_Tradesize<- senators_full_data |>
  group_by(Trade_year) |>
   summarise(Yearly_trade_size = sum(Trade_Size_USD, na.rm = T ), .groups = "drop")

## new datafraem including yearly trade size column

returns_per_year_per_party <- senators_full_data |>
  left_join(Yearly_Tradesize, by = "Trade_year") 

## calcualate each patrty's yearly weighted average return 

returns_per_year_per_party <- returns_per_year_per_party |>
  group_by(Trade_year, Party) |>
 summarize(weighted_retrun_per_party = sum(excess_return *Trade_Size_USD, na.rm=T),
           .groups = "drop")

## add yearly trade size to data frame

returns_per_year_per_party <- returns_per_year_per_party |> 
  left_join(Yearly_Tradesize, by ="Trade_year")

returns_per_year_per_party <- returns_per_year_per_party |>
  mutate(contribution_weighted_average_return = weighted_retrun_per_party/Yearly_trade_size)


## plot stacked bar chart

ggplot(returns_per_year_per_party, aes(x = as.factor(Trade_year),
                                       y = contribution_weighted_average_return, 
                                       fill = factor(Party))) +
  
  geom_col() +
  
  scale_fill_manual(
    values = c("D" = "blue", "R" = "red", "I" = "green"),  # Custom colors
    labels = c("Democrats", "Independents" , "Republicans" )    # Custom labels
  ) +
  
  labs(
    x = "Trade Year",
    y = "Weighted Average Return\n",
    title = "Parties contribution to total senate average return",
    fill = "Political Party"
  ) 







