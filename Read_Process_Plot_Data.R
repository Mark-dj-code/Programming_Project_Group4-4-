###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          1                               ####
####                                                                       ####
###############################################################################
###############################################################################


##################### Set up environment, install packages, call on libraries


install.packages("devtools")
install.packages("rnaturalearth")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("zoo")

library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(zoo)





################################
###upload data as data frame####
################################

senators_full_data<-read.csv("data/Copyofcongress-trading-all.csv")
senators_control_data <- read.csv("data/Copyofcongress-trading-all.csv")

#### eliminate duplicate rows

senators_full_data <- distinct(senators_full_data)




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
### add trade month column

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
          
          Trade_month = floor_date(as.Date(Traded), unit = "month") # add trade month column
          
          
          
  ) 




###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          3                               ####
####                                                                       ####
###############################################################################
###############################################################################


##########################################################################
#   Calculating weighted average excess return for each year     
#
#      detect anomalous years                                        
#
##########################################################################




library(dplyr)

#remove NA values of trans size and excess return from data

senators_full_data <- senators_full_data |> 
  filter(!is.na(Trade_Size_USD), !is.na(excess_return))

### calculate weighted average return per senator per year in new data frame

senators_full_data_by_year <- senators_full_data |>
  group_by(Trade_year) |>
  summarize(weighted_average_return = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )









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










###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          5                               ####
####                                                                       ####
###############################################################################
###############################################################################



##########################################################
#
#
#     compare senators yearly weighted average returns to SP 500 data
#
#
#########################################################


### Merge SP 500 data with senators weighted avg return data

senators_full_data_by_year <- left_join(senators_full_data_by_year, SP_500_annual_returns, by = c("Trade_year"="date"))



#### calculate percent difference between  yearly SP 500 return (market return) and
#### senator weighted average yearly return

senators_full_data_by_year <- senators_full_data_by_year |>
  
  mutate(abnormal_return_magnitude = weighted_average_return - SP_500_return,
         
         abnormal_return_percentage = 100*abnormal_return_magnitude/abs(SP_500_return),
         
         explanation = ifelse (abnormal_return_percentage >0, 
                               paste0(sprintf("%.2f" , abnormal_return_percentage), " % higher"),
                               paste0(sprintf("%.2f" , abnormal_return_percentage), " % lower") )
         
  )                      
#

### arrange by abnormal return size

senators_full_data_by_year <- arrange(senators_full_data_by_year, desc(abnormal_return_magnitude ) )













###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          6                               ####
####                                                                       ####
###############################################################################
###############################################################################



########################################
##########     temporal plot          ##
########################################


## load monthly sp 500 data 

sp_500_monthly <- read.csv("data/sp500_monthly.csv")

## change Date column from characater to date object

sp_500_monthly<- sp_500_monthly |>
  mutate(Date = as.Date(Date))

# month of October is not displayed properly month = 01, instead of month =10 modify that and change it

library(lubridate)

position <- 10   # first October is in 10th row of the list

while( position < 1833 ) {
  
  temporary_date <- sp_500_monthly$Date[position]  
  month(temporary_date) <- 10
  sp_500_monthly$Date[position] <- temporary_date  ## modify temp date month from 01 to 10 then assign back to date vector
  
  position <- position +12 ## go 12 month forward
  
}

#change column name to something more intuitive

colnames(sp_500_monthly)[2] <- "price"

## calculate monthly return by using lag function 

sp_500_monthly <- sp_500_monthly |>
  mutate(price_prior_month = lag(price),
         sp_500_monthly_return = 100*((price + Dividend - price_prior_month )/price_prior_month ) )

## create dataframe with only date and return, remove excess columns

sp_500_monthly_returns <- subset(sp_500_monthly, select = c(Date, sp_500_monthly_return))

## make sure date column is date object

sp_500_monthly_returns <- sp_500_monthly_returns |>
  mutate(Date =as.Date(Date))


## fill missing market return data using interpolation (avoid gaps in line plot)

library(zoo)

sp_500_monthly_returns$sp_500_monthly_return <- na.approx(sp_500_monthly_returns$sp_500_monthly_return, na.rm = FALSE)


#########################################################
########################################################

################## get average senator return per month 

##########################################################
#########################################################


### add trade month column to senators full data

library(lubridate)


senators_full_data <- senators_full_data |>
  mutate(Trade_month = floor_date(as.Date(Traded), unit = "month"))


## create new data frame with weighted average return per month for entire senate

senators_weighted_average_returns_monthly <- senators_full_data |> 
  group_by(Trade_month) |>
  summarize(total_weight = sum(Trade_Size_USD, na.rm = TRUE),
            weighted_average_return = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )


## merge datasets 

senators_weighted_average_returns_monthly <- senators_weighted_average_returns_monthly |>
  left_join(sp_500_monthly_returns, by = c("Trade_month" = "Date"))

##############################################################

# reshape data frame to plot two lines in ggplot() geomline(
##### get something like:
###
###
##   01-01-2012    return 1    sp500
##   01-01-2012    return a    senators
##   01-02-2012    return 2    sp500
##   01-02-2012    return b    senators
##     ...           ...        ...
##########################################
## use function pivot_longer

library(tidyr)

reshaped_monthly_returns <- senators_weighted_average_returns_monthly |>
  pivot_longer(
    cols = c(weighted_average_return, sp_500_monthly_return),
    values_to = "Return",
    names_to = "Series"
  )

###################
###################
# create line plot 
##################

library(ggplot2)

reshaped_monthly_returns |>
  filter(!is.na(Return)) |> #remove NA returns
  ggplot( aes(x=Trade_month , y=Return, group=Series, color=Series)) +
  geom_line(linewidth = 1.2) +
  
  scale_x_date(
    name = "\nYears",
    breaks = as.Date(paste0(2012:2024, "-01-01")),
    limits = as.Date(c("2012-01-01", "2024-12-31")) ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) ) + 
  
  scale_color_discrete(name = "Monthly returns on investment\n",
                       labels = c("sp_500_monthly_return" = "S&P 500" , "weighted_average_return" = "US Senate")) + 
  geom_vline(xintercept = as.Date("2020-03-01"), 
             linetype = "dashed",
             color = "black", 
             size = 0.1) +
  
  annotate("text", 
           x = as.Date("2020-03-01"), 
           y = 500, 
           label = "COVID 19",
           hjust = 1.1, 
           angle = 0, 
           size = 3.5,
           color = "black")

#



########################## covid 19 Zoom in line plot


reshaped_monthly_returns[167:206,] |>
  filter(!is.na(Return)) |> #remove NA returns
  ggplot( aes(x=Trade_month , y=Return, group=Series, color=Series)) +
  geom_line(linewidth = 1.2) +
  
  scale_x_date(
    name = "\nYears",
    limits = as.Date(c("2019-06-01", "2021-01-01")) ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) ) + 
  
  scale_color_discrete(name = "Monthly returns on investment\n",
                       labels = c("sp_500_monthly_return" = "S&P 500" , "weighted_average_return" = "US Senate")) + 
  geom_vline(xintercept = as.Date("2020-03-01"), 
             linetype = "dashed",
             color = "black", 
             size = 0.1) +
  
  annotate("text", 
           x = as.Date("2020-03-01"), 
           y = 150, 
           label = "COVID 19",
           hjust = 1.1, 
           angle = 0, 
           size = 3.5,
           color = "black")

#








###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          7                               ####
####                                                                       ####
###############################################################################
###############################################################################

#### Abnormal return magnitude bar chart

ggplot(senators_full_data_by_year, aes(x = as.factor(Trade_year),
                                       y = abnormal_return_magnitude)) +
  
  geom_col() +
  
  labs(x = "\nTrade Year",
       y = "Abnormal return Magnitude\n",
       title = "Difference between Senate return and Market return (%)\n")

#### abnormal return percentage bar chart


ggplot(senators_full_data_by_year, aes(x = as.factor(Trade_year),
                                       y = abnormal_return_percentage)) +
  
  geom_col() +
  
  labs(x = "\nTrade Year",
       y = "Abnormal return Percentage\n",
       title = "Proportion of market performance achieved by senate (%)\n")







###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          8                               ####
####                                                                       ####
###############################################################################
###############################################################################


######## heatmap 2013

########################## 2013 heatmap (abnormal eturnn per state)

return_per_state_2013 <- senators_full_data |> 
  filter(Trade_year == "2013")|>
  group_by(State)|>
  summarize(weighted_average_return = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )

### no need to calculate abnormal return size (ie difference with market return, rankimg would stay the same since for a single year)

## remove Hawaii and Alaska

### remove alaska and hawaii

return_per_state_2013 <- return_per_state_2013 |>
  filter(!(State %in% c("Alaska", "Hawaii")))

####### install shapefile pacakges to get US states Map


devtools::install_github("ropensci/rnaturalearthhires")

library(rnaturalearth)



## import shapefile object of US states map: 

us_states_map = ne_states(country ="united states of america", returnclass = "sf" )


################## create heatmap

# merge map and returns per state data

us_states_returns_map_2013 <- us_states_map |>
  left_join(return_per_state_2013, by = c("name" = "State")) |>
  mutate(Return = replace_na(weighted_average_return, 0))|> ## replace NA for missing states with 0
  filter(!(name %in% c("Alaska", "Hawaii")))




### plot heatmap

ggplot(us_states_returns_map_2013, aes(fill = weighted_average_return)) +
  geom_sf( color = "orange", size = 0.2 ) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white",
                      name = "Excess return\nby state in \n2013 (%)"
  )







###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          9                               ####
####                                                                       ####
###############################################################################
###############################################################################


########################## 2015 heatmap (abnormal eturnn per state)

return_per_state_2015 <- senators_full_data |> 
  filter(Trade_year == "2015")|>
  group_by(State)|>
  summarize(weighted_average_return = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )

### no need to calculate abnormal return size (ie difference with market return, rankimg would stay the same since for a single year)

## remove Hawaii and Alaska

### remove alaska and hawaii

return_per_state_2015 <- return_per_state_2015 |>
  filter(!(State %in% c("Alaska", "Hawaii")))

####### install shapefile pacakges to get US states Map


devtools::install_github("ropensci/rnaturalearthhires")

library(rnaturalearth)



## import shapefile object of US states map: 

us_states_map = ne_states(country ="united states of america", returnclass = "sf" )


################## create heatmap

# merge map and returns per state data

us_states_returns_map_2015 <- us_states_map |>
  left_join(return_per_state_2015, by = c("name" = "State")) |>
  mutate(Return = replace_na(weighted_average_return, 0))|> ## replace NA for missing states with 0
  filter(!(name %in% c("Alaska", "Hawaii")))




### plot heatmap

ggplot(us_states_returns_map_2015, aes(fill = weighted_average_return)) +
  geom_sf( color = "orange", size = 0.2 ) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white",
                      name = "Excess return\nby state in \n2015 (%)"
  )











###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          10                              ####
####                                                                       ####
###############################################################################
###############################################################################


####################################################
##
##
## Map of average excess return per state for all years 2012 --> 2024
##
##
#####################################################

### average market returns over the years (2012 to 2024)

average_market_return_over_time <- mean(SP_500_annual_returns$SP_500_return[85:98])



## group data by state in new dataframe, sum abnormal return size over states for all years

library(tidyverse)
library(dplyr)

returns_by_state <- senators_full_data |>
  group_by(State) |>
  summarize(weighted_average_return_per_state = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop")


### new variable is difference between average return per state and average market return

returns_by_state <- returns_by_state |>
  mutate( State_average_abnormal_return = 100*( weighted_average_return_per_state - average_market_return_over_time )/abs(average_market_return_over_time))

### remove alaska and hawaii

returns_by_state <- returns_by_state |>
  filter(!(State %in% c("Alaska", "Hawaii")))

####### install shapefile pacakges to get US states Map


devtools::install_github("ropensci/rnaturalearthhires")

library(rnaturalearth)



## import shapefile object of US states map: 

us_states_map = ne_states(country ="united states of america", returnclass = "sf" )


################## create heatmap

# merge map and returns per state data

us_states_returns_map <- us_states_map |>
  inner_join(returns_by_state, by = c("name" = "State"))


#plot merged data

ggplot(us_states_returns_map, aes(fill = State_average_abnormal_return)) +
  geom_sf( color = "white", size = 0.2 ) +
  scale_fill_gradient(low = "white", high = "red",
                      name = "Average market\noutperformance per state\n(in%) from 2012 to 2024"
  )









###############################################################################
###############################################################################
####                                                                       ####
####                         STEP          11                              ####
####                                                                       ####
###############################################################################
###############################################################################


##### bar plot for senate average return by year per party


## gettotal  trade size for each year

Yearly_Tradesize<- senators_full_data |>
  group_by(Trade_year) |>
  summarise(Yearly_trade_size = sum(Trade_Size_USD, na.rm = T ), .groups = "drop")

## new datafraem including yearly trade size column

returns_per_year_per_party <- senators_full_data |>
  left_join(Yearly_Tradesize, by = "Trade_year") 

## calcualate each patrty's yearly weighted average return 

returns_per_year_per_party <- returns_per_year_per_party |>
  group_by(Trade_year, Party) |>
  summarize(weighted_retrun_per_party = sum(excess_return *Trade_Size_USD, na.rm=T),
            .groups = "drop")

## add yearly trade size to data frame

returns_per_year_per_party <- returns_per_year_per_party |> 
  left_join(Yearly_Tradesize, by ="Trade_year")

returns_per_year_per_party <- returns_per_year_per_party |>
  mutate(contribution_weighted_average_return = weighted_retrun_per_party/Yearly_trade_size)


## plot stacked bar chart

ggplot(returns_per_year_per_party, aes(x = as.factor(Trade_year),
                                       y = contribution_weighted_average_return, 
                                       fill = factor(Party))) +
  
  geom_col() +
  
  scale_fill_manual(
    values = c("D" = "blue", "R" = "red", "I" = "green"),  # Custom colors
    labels = c("Democrats", "Independents" , "Republicans" )    # Custom labels
  ) +
  
  labs(
    x = "Trade Year",
    y = "Weighted Average Return\n",
    title = "Parties contribution to total senate average return",
    fill = "Political Party"
  ) 







