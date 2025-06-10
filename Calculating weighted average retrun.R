
###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          3                               ####
####                                                                       ####
###############################################################################
###############################################################################



###############################################################################
###############################################################################
##########   Calculating weighted average excess return for each senator  #####
###############################################################################
###############################################################################


###############################################################################
##########   find senators and years they operated in                     #####

## example : senator a    2016
###          senator a    2018
###          senator b    2014
###          senator c    2015
###            ...
###############################################################################


### create dataframe containing individual senator names once per 
### distinct year they operated

library(dplyr)

#remove NA values of trans size and excerss return from data

transaction_size_senators <- transaction_size_senators |> 
                             filter(!is.na(Transaction_Size), !is.na(Excess_Return))

# dataframe containing dupicates (multiple years because multiple stocks traded)



senators_operating_years <- data.frame(
  
  senator_name = transaction_size_senators[,"Senator_Name"],
  
  year_in_office = transaction_size_senators[,"Trade_date"]
  
  
)

# keep only unique year/name combinations

senators_operating_years <- distinct(senators_operating_years)



weighted_average_returns <- numeric(nrow(senators_with_totals))

for (i in 1:nrow(senators_with_totals)) {
  
###### create new data frame containing total transaction size per senator per year (totals)


totals <- transaction_size_senators |>
  group_by(Senator_Name, Trade_date) |>
  summarise(Total_Transaction = sum(Transaction_Size), .groups = "drop")


### add totals table to senator operating years table

senators_with_totals <- senators_operating_years |>
  left_join(totals, by = c("senator_name" = "Senator_Name", "year_in_office" = "Trade_date"))



### create new data frame containing weighted returns per senator per year (weighted returns) 

weighted_returns <- transaction_size_senators |>
            group_by(Senator_Name, Trade_date) |>
  summarise(yearly_weighted_return = sum(Transaction_Size*Excess_Return), .groups = "drop")


## add weighted returns table to operating years table

senators_with_totals <- senators_with_totals |>
  left_join(weighted_returns , by = c("senator_name" = "Senator_Name", "year_in_office" = "Trade_date"))
  

### calculate and add weighted average return column to senators with totals

  weighted_average_returns[i] <- senators_with_totals$yearly_weighted_return[i] / 
                                  senators_with_totals$Total_Transaction[i]
  
}

## add weighted average return column to senators with totals dataframe

 senators_with_totals$weighted_average_returns <- weighted_average_returns
 
 


