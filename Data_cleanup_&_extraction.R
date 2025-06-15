
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

library(lubridate)

library(stringr)

################## Modify senators_full_data column entries to extract useful data

### Text date --> Date object
### Trade size range text --> Numeric mean of trade size boundaries
### Excess return adjusted for type of transaction (sale/ purchase)
### scale down excess return
### add trade minth column

senators_full_data <- senators_full_data|>
  
  mutate( Traded = as.Date(Traded, format = "%A, %B %d, %Y"), # character to date object
          
          
          Trade_year = format(Traded, "%Y"), # new trade_year column
         
         excess_return = ifelse(Trade_Size_USD == "Sale", - abs(excess_return), abs(excess_return) ), # adjust return sign fore sale/purchase
         
         Trade_Size_USD = Trade_Size_USD |>
           str_extract_all( "\\$?\\d{1,3}(,\\d{3})*(\\.\\d+)?|\\$?\\d+(\\.\\d+)?"),
         
         # now  Trade_size_usd column is a list, entries are 2 D character vectors containing boundaries of the trade size range
         
         # modify Trade_Size_USD column to contain numeric mean of range boundaries
         
         Trade_Size_USD = sapply(Trade_Size_USD, function(x) {
           nums <- as.numeric(gsub("[^0-9]", "", x))  # Extract numbers, replace everything that is not a digit between 0 and 9 with nothing
           mean(nums, na.rm = TRUE) #calculate mean 
         }),
         
         scaled_return = excess_return/100, #add scaled return column
         
         Trade_month = floor_date(as.Date(Traded), unit = "month") # add trade month column
         
         
         
           ) 



