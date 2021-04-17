## set wd
dir.create("./RStudio/store24")
setwd("./RStudio/store24")

library(stringr)
library(dplyr)
library(tidyr)

## load library
store24_3_raw <- read.csv("store24_3.csv")
## tight-up the data
store24_3 <- sapply(store24_3_raw, function(x) gsub("[^0-9.-]", "", x))
store24_3 <- apply(store24_3, 2, function(x) as.numeric(x))
store24_3 <- as.data.frame(store24_3)
names(store24_3) <- tolower(names(store24_3))
names(store24_3) <- gsub("\\.", "_", names(store24_3))
names(store24_3)[1] <- "store_number"
## add gdp column
store24_3$gdp <- store24_3$per_capita_income * store24_3$population
## re-size the data range to 0~1
adjust_weight <- sapply(store24_3, max)
store24_3_adjusted <- data.frame(store24_3[,1:2], sweep(store24_3, 2, adjust_weight, "/")[,3:length(names(store24_3))])

## time to separate the shops based on future controllable contribution
upper_group <- store24_3[store24_3[,2] >= 30000,]
lower_group <- store24_3[store24_3[,2] < 30000,]
## add adjusted groups
upper_group_adjusted <- store24_3_adjusted[store24_3[,2] >= 30000,]
lower_group_adjusted <- store24_3_adjusted[store24_3[,2] < 30000,]

#########################
## regression analysis ##
#########################

linearMod_upper_adjusted <- lm(formula = future_controllable_contribution ~ . - store_number - 
                        gdp, data = upper_group_adjusted)
summary(linearMod_upper_adjusted)

linearMod_lower_adjusted <- lm(formula = future_controllable_contribution ~ . - store_number - 
                              gdp, data = lower_group_adjusted)
summary(linearMod_lower_adjusted)     
