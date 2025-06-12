

###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          1                               ####
####                                                                       ####
###############################################################################
###############################################################################








################################
###upload data as data frame####
################################

senators_full_data<-read.csv("data/Copyofcongress-trading-all.csv")
senators_control_data <- read.csv("data/Copyofcongress-trading-all.csv")


############extract names in separate dataframe
senator_names <- senators_full_data["Name"]


######## order names alphabetically in new dataframe

senator_names<- senator_names[order(senator_names$Name),]


senator_names<- as.data.frame(senator_names)

######### create new data frame without duplicate names

install.packages("dplyr")
library(dplyr)


senator_names <- distinct(senator_names)

