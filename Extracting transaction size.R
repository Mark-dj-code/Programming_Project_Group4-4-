######## extract trade size as numeric value test

transaction_size <- senators_full_data["Trade_Size_USD"]
class(transaction_size)

class(transaction_size[1,1])
transaction_size[,1]<-as.numeric(transaction_size[,1])
View(transaction_size)
