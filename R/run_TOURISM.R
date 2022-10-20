###############################################################################
#                             Main file for TOURISM                           #  
###############################################################################

#### Import global variables ####
source("R/utils/globalVariables.R")
source("R/utils/getData.R")
source("R/utils/save_entries.R")

#### Import the data ####
data <- getData("TOURISM")

#### Run the different models ####
source("R/TOURISM/LastPeriod_model.R") # to be converted to functions
source("R/TOURISM/S-ARIMA.R") # to be converted to functions

#### Save the results #### 
entries <- list("entry_1"= preds_naive_1y, "entry_2"= preds_sarima)
save_entries(entries, "Submissions/TOURISM/results_october.json")
