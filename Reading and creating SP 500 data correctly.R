###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          4                               ####
####                                                                       ####
###############################################################################
###############################################################################

#################################################
##    import SP 500 return data into a data frame
#################################################


######## assign market annual return data frame, skip first 15 lines of files (metadata) to get proper
########## columns names, Actual data starts at line 16

library(readr)

readLines("data/sp-500-historical-annual-returns.csv", n=20)  ##check where data starts

SP_500_annual_returns <- read_csv("data/sp-500-historical-annual-returns.csv" , skip = 15) ##skip metadata

SP_500_annual_returns <- as.data.frame(SP_500_annual_returns) ##set data to class = dataframe



################## formatting date column to only years (date column entries are class == date)

SP_500_annual_returns[,"date"] <- format(SP_500_annual_returns[,"date"], "%Y")



###### changing value column name

colnames(SP_500_annual_returns)[2] <- "SP_500_return"
