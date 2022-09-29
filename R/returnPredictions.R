###############################################################################
#                           Return predictions                                #  
###############################################################################

#########################################
# 0.a) Import packages
#########################################

library(dplyr)
library(data.table)
library(jsonlite)

#########################################
# 0.b) Import global variables
#########################################

source("R/dummyModels.R")
source("R/sarima.R")

list_ppi_preds <- list(ppi_preds_naive_1m, ppi_preds_naive_3m, ppi_preds_arima)
list_pvi_preds <- list(pvi_preds_naive_1m, pvi_preds_naive_3m, pvi_preds_arima)
list_tourism_preds <- list(tourism_preds_naive_1y, tourism_preds_naive_6y, tourism_preds_sarima)

#########################################
# 1) Concatenate the predictions
#########################################

ppi_all_preds <- rbindlist(list_ppi_preds)
pvi_all_preds <- rbindlist(list_pvi_preds)
tourism_all_preds <- rbindlist(list_tourism_preds)

#########################################
# 1) Concatenate the predictions
#########################################

ppi_json <- toJSON(ppi_all_preds)
pvi_json <- toJSON(pvi_all_preds)
tourism_json <- toJSON(tourism_all_preds)

ppi_json

pvi_json

tourism_json





