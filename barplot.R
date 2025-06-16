install.packages("tidyverse")
install.packages("timechange")
library(tidyverse)

getwd()

congresstrades$PartyNumeric <- as.numeric(factor(congresstrades$Party))
levels(factor(congresstrades$Party))

party_labels <- c("1" = "D", "2" = "I", "3" = "R")
counts <- table(congresstrades$PartyNumeric)

barplot(counts,
        main = "Number of Trades by Party",
        xlab = "Party",
        ylab = "Count",
        ylim = c(0, 30000),
        col = "steelblue",
        names.arg = party_labels[names(counts)])