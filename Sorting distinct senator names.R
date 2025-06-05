
###############################
###upload data as dataframe####
###############################

senators_full_data<-read.csv("data/Copyofcongress-trading-all.csv")
View(senators_full_data)
class(senators_full_data)

############extract names in separate dataframe
senator_names <- senators_full_data["Name"]
class(senator_names)
View(senator_names)

######## order names alphabetically in new dataframe

senator_names<- senator_names[order(senator_names$Name),]
class(senator_names_sorted)

senator_names<- as.data.frame(senator_names)
View(senator_names)

######### create new data frame without duplicate names

install.packages("dplyr")
library(dplyr)

senator_names <- distinct(senator_names)

View(senator_names)
 




