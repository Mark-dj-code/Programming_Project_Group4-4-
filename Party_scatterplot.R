### scatterplot by party

ggplot(senators_full_data, aes(x= Party , y= excess_return)) +
  geom_point()
