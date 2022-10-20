###############################################################################
#                             Main file for PPI                               #  
###############################################################################

#### Import global variables ####
source("R/utils/globalVariables.R")
source("R/utils/getData.R")
source("R/utils/save_entries.R")

#### Import the data ####
data <- getData("PPI")

#### Run the different models ####
source("R/PPI/LastPeriod_model.R") # to be converted to functions
source("R/PPI/S-ARIMA.R") # to be converted to functions

#### Save the results #### 
entries <- list("entry_1"= preds_naive_1m, "entry_2"= preds_sarima)
save_entries(entries, "Submissions/PPI/results_october.json")
