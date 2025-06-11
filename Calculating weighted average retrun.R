
###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          3                               ####
####                                                                       ####
###############################################################################
###############################################################################



###############################################################################
###############################################################################
##########   Calculating weighted average excess return for each senator  #####
###############################################################################
###############################################################################



library(dplyr)

#remove NA values of trans size and excess return from data

senators_full_data <- senators_full_data |> 
                             filter(!is.na(Trade_Size_USD), !is.na(excess_return))

### calculate weighted average return per senator per year in new data frame

senators_full_data_grouped <- senators_full_data |>
  group_by(Name, Trade_year) |>
  summarize(weighted_average_return = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )


 
 


