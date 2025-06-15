



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
  geom_line(size = 1.2) +
  
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
