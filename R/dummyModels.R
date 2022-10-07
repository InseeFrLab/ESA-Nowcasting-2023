###############################################################################
#                             Dummy models                                    #  
###############################################################################

#########################################
# 0.a) Import packages
#########################################

library(dplyr)
library(lubridate)

#########################################
# 0.b) Import global variables
#########################################

source("R/globalVariables.R")
source("R/getData.R")
date_to_predict <- ymd(date_to_predict)

#########################################
# 1. Models re-using the last relevant value
#########################################

# 1.a) PPI
# Use the value of the last available month

ppi_preds_naive_1m <- data.frame(matrix(ncol = length(countries_PPI), nrow = 1))
colnames(ppi_preds_naive_1m) <- countries_PPI

for (country in countries_PPI){
  last_available_value <- tail(ppi_data %>%
                                 select(country) %>%
                                 drop_na(),
                               n = 1)
  ppi_preds_naive_1m[[country]][1] <- round(as.numeric(last_available_value),1)
}

ppi_preds_naive_1m

# 1.b) PVI
# Use the value of the last available month

pvi_preds_naive_1m <- data.frame(matrix(ncol = length(countries_PVI), nrow = 1))
colnames(pvi_preds_naive_1m) <- countries_PVI

for (country in countries_PVI){
  last_available_value <- tail(pvi_data %>%
                                 select(country) %>%
                                 drop_na(),
                               n = 1)
  pvi_preds_naive_1m[[country]][1] <- round(as.numeric(last_available_value),1)
}

pvi_preds_naive_1m


# 1.c) Tourism
# Use the value of the last available year at the same month

tourism_preds_naive_1y <- data.frame(matrix(ncol = length(countries_tourism), nrow = 1))
colnames(tourism_preds_naive_1y) <- countries_tourism

for (country in countries_tourism){
  last_available_value <- tail(tourism_data %>%
                                 filter(month(time) == month(date_to_predict)) %>%
                                 select(country) %>%
                                 drop_na(),
                               n = 1)
  tourism_preds_naive_1y[[country]][1] <- round(as.numeric(last_available_value),1)
}

tourism_preds_naive_1y


#########################################
# 2. Models re-using the x last relevant values
#########################################

# 2.a) PPI
# Use the value of the last 3 available months

ppi_preds_naive_3m <- data.frame(matrix(ncol = length(countries_PPI), nrow = 1))
colnames(ppi_preds_naive_3m) <- countries_PPI

for (country in countries_PPI){
  last_available_values <- tail(ppi_data %>%
                                 select(country) %>%
                                 drop_na(),
                               n = 3)
  ppi_preds_naive_3m[[country]][1] <- round(as.numeric(mean(as.matrix(last_available_values))),1)
}

ppi_preds_naive_3m

# 2.b) PVI
# Use the value of the last 3 available months

pvi_preds_naive_3m <- data.frame(matrix(ncol = length(countries_PVI), nrow = 1))
colnames(pvi_preds_naive_3m) <- countries_PVI

for (country in countries_PVI){
  last_available_values <- tail(pvi_data %>%
                                 select(country) %>%
                                 drop_na(),
                               n = 3)
  pvi_preds_naive_3m[[country]][1] <- round(as.numeric(mean(as.matrix(last_available_values))),1)
}

pvi_preds_naive_3m


# 2.c) Tourism
# Use the value of the last available 6 years at the same month

tourism_preds_naive_6y <- data.frame(matrix(ncol = length(countries_tourism), nrow = 1))
colnames(tourism_preds_naive_6y) <- countries_tourism

for (country in countries_tourism){
  last_available_values <- tail(tourism_data %>%
                                 filter(month(time) == month(date_to_predict)) %>%
                                 select(country) %>%
                                 drop_na(),
                               n = 6)
  tourism_preds_naive_6y[[country]][1] <- round(as.numeric(mean(as.matrix(last_available_values))),1)
}

tourism_preds_naive_6y


