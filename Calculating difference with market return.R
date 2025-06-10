
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
#     compare senators yearly weighted average returns to SP 500 data
#
#
#########################################################


### Merge SP 500 data with senators weighted avg return data

senators_with_totals <- left_join(senators_with_totals, SP_500_annual_returns, by = c("year_in_office"="date"))



#### calculate percent difference between  yearly SP 500 return (market return) and
#### senator weighted average yearly return

abnormal_return_size_vector <- numeric(nrow(senators_with_totals))

for (i in 1:nrow(senators_with_totals)){
  
  abnormal_return_size_vector[i] <- (100*(senators_with_totals[i, "weighted_average_returns" ] -
                                       senators_with_totals[i, "SP_500_return"]) / 
                                abs(senators_with_totals[i, "SP_500_return"])) 
  
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


### adding column with only numeric abnormal return size (no text, for easy sorting)

for (i in 1:nrow(senators_with_totals)) {
  
  senators_with_totals$abnormal_return_numeric[i] <- (100*(senators_with_totals[i, "weighted_average_returns" ] -
                                                             senators_with_totals[i, "SP_500_return"]) / 
                                                        abs(senators_with_totals[i, "SP_500_return"])) 
  
}


### sorting data by descending abnormal return size

senators_with_totals <- arrange(senators_with_totals, desc(abnormal_return_numeric))


####### export new table (senators with totals to data folder)

write.csv(senators_with_totals, "data/senators_compared_average_returns.csv")

senators_data <- read.csv("data/senators_compared_average_returns.csv")


