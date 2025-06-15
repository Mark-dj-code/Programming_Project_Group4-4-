#### Abnormal return amplitue bar chart

ggplot(senators_full_data_by_year, aes(x = as.factor(Trade_year),
                                       y = abnormal_return_magnitude)) +
  
  geom_col() +
  
  labs(x = "\nTrade Year",
       y = "Abnormal return Magnitude\n",
       title = "Difference between Senate return and Market return (%)\n")

