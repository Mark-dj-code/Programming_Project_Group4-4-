
###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          2                               ####
####                                                                       ####
###############################################################################
###############################################################################






############################################################
########### extract senators transaction size ##############
############################################################


##### #test# extract single transaction soze numeric from character string

library(stringr)

text <- senators_full_data[1,"Trade_Size_USD"]

numbers_string <- str_extract_all( text , "\\$?\\d{1,3}(,\\d{3})*(\\.\\d+)?|\\$?\\d+(\\.\\d+)?")[[1]]

numbers_clean <- as.numeric(gsub("[^0-9]", "" , numbers_string ))

print(numbers_clean)

class(numbers_clean)

numbers_clean <- mean(numbers_clean, na.rm=T)
print(numbers_clean)


################################## full data complete extraction code######
###########################################################################
############### create numeric n  = number of rows + call on stringr library





library(stringr)

n <- nrow(senators_full_data)

###### allocate space for n dimensional vectors to later insert in data frame

names_vector <- character(n) 
transaction_size_vector <- numeric(n)
stocks_vector <- character(n)
year_vector <- numeric(n)
return_vector <- numeric(n)

####### add data to vectors from senators full data row by row

for (i in 1:nrow(senators_full_data)) {
  
  
  
  names_vector[i] <- senators_full_data[i , "Name"]
  
  stocks_vector[i] <- senators_full_data [i , "Ticker"]
  
  year_vector[i] <- senators_full_data [i , "Traded"]
  
    ## make transaction > 0 if purchase, <0 if sale
  
          if (senators_full_data[i, "Transaction" ] == "Sale"){
            
            return_vector[i] <- -senators_full_data[i, "excess_return"]
            
          } else if (senators_full_data[i, "Transaction" ] == "Purchase") {
            
            return_vector[i] <- senators_full_data[i, "excess_return"]
            
          } else {
               
            return_vector[i] <- NA
            
            }
  
  
      ## extract transaction size for row i, calculate mean
  
  text <- senators_full_data[i ,"Trade_Size_USD"]
  
  numbers_string <- str_extract_all( text , "\\$?\\d{1,3}(,\\d{3})*(\\.\\d+)?|\\$?\\d+(\\.\\d+)?")[[1]]
  
  # substitute everything that's not a digit between 0 and 9 with nothing 
  
  numbers_clean <- as.numeric(gsub("[^0-9]", "" , numbers_string )) 
  
  
  numbers_clean <- mean(numbers_clean, na.rm=T)
  
  
      ## assign value of extracted mean to transaction vector row i
  
  transaction_size_vector[i] <- numbers_clean
  
}



####### Combine vectors into new data frame

transaction_size_senators <- data.frame(
  
  Senator_Name = names_vector , 
  
  Traded_stock = stocks_vector,
  
  Trade_date = year_vector,
  
  Transaction_Size = transaction_size_vector,
  
  Excess_Return = return_vector
    
    )

View(transaction_size_senators)




 
############################################################
############################################################
####### change trade date column entries from text string to date format containing only year
############################################################
############################################################


for (i in 1:nrow(transaction_size_senators)) {
  

    trade_year_str <- str_extract_all(transaction_size_senators[i, "Trade_date"],"\\d{4}" )[[1]]

    if (length(trade_year_str) > 0) {
      
      # Store full date from year in temporary date object
      
      temporary_full_trade_date <- as.Date(paste0(trade_year_str, "-01-01")) 
      
      # assign year only date to row i  date entry
     transaction_size_senators[i, "Trade_date"] <- format(temporary_full_trade_date, "%Y") 
     
    } else {
      
      #####to avoid problems with missing data
      
     transaction_size_senators[i, "Trade_date"] <- NA
    }
  
  
}

################################################################################
################################################################################





