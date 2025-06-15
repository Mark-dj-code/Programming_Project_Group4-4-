
###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          5                               ####
####                                                                       ####
###############################################################################
###############################################################################



##########################################################
#
#
#     compare senators yearly weighted average returns to SP 500 data
#
#
#########################################################


### Merge SP 500 data with senators weighted avg return data

senators_full_data_by_year <- left_join(senators_full_data_by_year, SP_500_annual_returns, by = c("Trade_year"="date"))



#### calculate percent difference between  yearly SP 500 return (market return) and
#### senator weighted average yearly return

senators_full_data_by_year <- senators_full_data_by_year |>
  
  mutate(abnormal_return_magnitude = weighted_average_return - SP_500_return,
         
         abnormal_return_percentage = 100*abnormal_return_magnitude/abs(SP_500_return),
         
         explanation = ifelse (abnormal_return_percentage >0, 
                               paste0(sprintf("%.2f" , abnormal_return_percentage), " % higher"),
                               paste0(sprintf("%.2f" , abnormal_return_percentage), " % lower") )

        )                      
#

### arrange by abnormal return size

senators_full_data_by_year <- arrange(senators_full_data_by_year, desc(abnormal_return_magnitude ) )


