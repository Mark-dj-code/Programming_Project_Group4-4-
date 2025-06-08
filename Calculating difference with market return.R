
###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          4                               ####
####                                                                       ####
###############################################################################
###############################################################################



##########################################################
#
#
#     compare senatros yearly weighted average returns to SP 500 data
#
#
#########################################################


### Merge SP 500 data with senators weighted avg return data

senators_with_totals <- left_join(senators_with_totals, SP_500_annual_returns, by = c("year_in_office"="date"))



#### caluculate percent difference between  yealy SP 500 return (market return) and
#### senator weighted average yearly return

abnormal_return_size_vector <- numeric(nrow(senators_with_totals))

for (i in 1:nrow(senators_with_totals)){
  
  abnormal_return_size_vector[i] <- (100*(senators_with_totals[i, "weighted_average_returns" ] -
                                       senators_with_totals[i, "SP_500_return"]) / 
                                senators_with_totals[i, "SP_500_return"]) 
  
}

# assigning new column in senators_with_totals

for (i in 1:nrow(senators_with_totals)){
  
     if (abnormal_return_size_vector[i] >0) {
  
      senators_with_totals$abnormal_return_size[i] <- 
         paste0(sprintf("%.2f" , abnormal_return_size_vector[i]) , " % higher")
  
    } else if (abnormal_return_size_vector[i] <0) {
  
      senators_with_totals$abnormal_return_size[i] <-
       paste0(sprintf("%.2f" , -abnormal_return_size_vector[i]) , " % lower")
    } else {
  
     senators_with_totals$abnormal_return_size[i] <-
       paste0(paste0("0%"))
  
    }

}

