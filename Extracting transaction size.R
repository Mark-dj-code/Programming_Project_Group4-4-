############################################################
########### extract senators transaction size ##############
############################################################

library(stringr)

text <- senators_full_data[1,"Trade_Size_USD"]

numbers_string <- str_extract_all( text , "\\$?\\d{1,3}(,\\d{3})*(\\.\\d+)?|\\$?\\d+(\\.\\d+)?")[[1]]

numbers_clean <- as.numeric(gsub("[^0-9]", "" , numbers_string ))

print(numbers_clean)

class(numbers_clean)

numbers_clean <- mean(numbers_clean, na.rm=T)
print(numbers_clean)


############### create numerci = number of rows

n <- nrow(senators_full_data)

################ allocate vectors to later insert in dataframe

names_vector <- character(n) 
transaction_size_vector <- numeric(n)
stocks_vector <- character(n)
year_vector <- numeric(n)

################ add data to vectors from senators full data row by row

for (i in 1:nrow(senators_full_data)) {
  
  names_vector[i] <- senators_full_data[i , "Name"]
  
  stocks_vector[i] <- senators_full_data [i , "Ticker"]
  
  year_vector[i] <- senators_full_data [i , "Filed"]
  
      ## extract transaction size for row i, calculate mean
  
  text <- senators_full_data[i ,"Trade_Size_USD"]
  
  numbers_string <- str_extract_all( text , "\\$?\\d{1,3}(,\\d{3})*(\\.\\d+)?|\\$?\\d+(\\.\\d+)?")[[1]]
  
  numbers_clean <- as.numeric(gsub("[^0-9]", "" , numbers_string ))
  
  numbers_clean <- mean(numbers_clean, na.rm=T)
  
      ## assign value of extracted mean to transaction vector row i
  
  transaction_size_vector[i] <- numbers_clean
  
}



####################### Combine vectors into new data frame

transaction_size_senators <- data.frame(
  
  Senator_Name = names_vector , 
  
  Traded_stock = stocks_vector,
  
  Filing_date = year_vector,
  
  Transaction_Size = transaction_size_vector
    
    )

View(transaction_size_senators)
 