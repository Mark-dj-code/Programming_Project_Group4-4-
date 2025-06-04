
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

senator_names_sorted <- senator_names[order(senator_names$Name),]
class(senator_names_sorted)

senator_names_sorted <- as.data.frame(senator_names_sorted)
View(senator_names_sorted)

######### create new data framw without duplicate names

install.packages("dplyr")
library(dplyr)

senator_names_sorted_disitnct <- distinct(senator_names_sorted)

View(senator_names_sorted_disitnct)



