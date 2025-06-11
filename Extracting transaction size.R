
###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          2                               ####
####                                                                       ####
###############################################################################
###############################################################################





##                 Clean Up Data
############################################################
########### extract senators transaction size ##############
############################################################
########### change trade date format #######################
############################################################



library(stringr)

################## Modify senators_full_data column entries to extract useful data

### Text date --> Date object
### Trade size range text --> Numeric mean of trade size boundaries
### Ecxess return adjusted for type of transaction (sale/ purchase)

senators_full_data <- senators_full_data|>
  
  mutate( Traded = as.Date(Traded, format = "%A, %B %d, %Y"),
          
          # add Trade_year column
          
          Trade_year = format(Traded, "%Y"),
         
         excess_return = ifelse(Trade_Size_USD == "Sale", - abs(excess_return), abs(excess_return) ),
         
         Trade_Size_USD = Trade_Size_USD |>
           str_extract_all( "\\$?\\d{1,3}(,\\d{3})*(\\.\\d+)?|\\$?\\d+(\\.\\d+)?"),
         
         # now column is a list, entries are 2 D character vectors containing boundaries the trade size range
         
         # modify Trade_Size_USD column to contain numeric mean of range boundaries
         
         Trade_Size_USD = sapply(Trade_Size_USD, function(x) {
           nums <- as.numeric(gsub("[^0-9]", "", x))  # Extract numbers
           mean(nums, na.rm = TRUE) #calculate mean 
         })
         
           ) 



