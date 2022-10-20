###############################################################################
#                             Main file for PVI                               #  
###############################################################################

#### Import global variables ####
source("R/utils/globalVariables.R")
source("R/utils/getData.R")
source("R/utils/save_entries.R")

#### Import the data ####
data <- getData("PVI")

#### Run the different models ####
source("R/PVI/LastPeriod_model.R") # to be converted to functions
source("R/PVI/S-ARIMA.R") # to be converted to functions

#### Save the results #### 
entries <- list("entry_1"= preds_naive_1m, "entry_2"= preds_sarima)
save_entries(entries, "Submissions/PVI/results_october.json")
