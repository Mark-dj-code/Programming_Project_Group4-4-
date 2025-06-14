

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


