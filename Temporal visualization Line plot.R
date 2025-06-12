



########################################
##########     temporal plot          ##
########################################


## load monthly sp 500 data 

sp_500_monthly <- read.csv("data/sp500_monthly.csv")

#change column name to something more intuitive

colnames(sp_500_monthly)[2] <- "price"
 
## calculate monthly return by using lag function 

sp_500_monthly <- sp_500_monthly |>
  mutate(price_prior_month = lag(price),
         sp_500_monthly_return = 100*((price + Dividend)/price_prior_month -1) )

## create dataframe with only date and return, remove excess columns

sp_500_monthly_returns <- subset(sp_500_monthly, select = c(Date, sp_500_monthly_return))



#########################################################
########################################################

################## get average senator return per month 

##########################################################
#########################################################


### add trade month column to senators full data

senators_full_data <- senators_full_data |>
  mutate(Trade_month = format(Traded, "%Y-%m-01")
         )

## create new data frame with weighted average return per month for entire senate

senators_weighted_average_returns_monthly <- senators_full_data |> 
  group_by(Trade_month) |>
  summarize(weighted_average_return = sum(Trade_Size_USD*excess_return, na.rm = T)/sum(Trade_Size_USD, na.rm = T ), .groups = "drop" )


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

reshaped_monthly_returns |>
  ggplot( aes(x=Trade_month , y=Return, group=Series, color=Series)) +
  geom_line() 
  
#

