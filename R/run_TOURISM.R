###############################################################################
#                             Main file for TOURISM                           #  
###############################################################################
rm(list = ls())
#### Import global variables ####
source("R/utils/globalVariables.R")
source("R/utils/getData.R")
source("R/utils/save_entries.R")
source("R/utils/plot_routines.R")

#### Import the data ####
data <- getData("TOURISM")

#### Run the different models ####
date_to_pred <- ymd("2022-10-01")

source("R/TOURISM/LastPeriod_model.R") # to be converted to functions
source("R/TOURISM/S-ARIMA.R") # to be converted to functions
source("R/TOURISM/XGBoost.R") # to be converted to functions
source("R/TOURISM/DFM.R") # to be converted to functions

#### Plotting the results #### 
predictions <- bind_rows(list(
  "entry_1"= preds_naive_1y%>%mutate(Entries = "Naive"), 
  "entry_2"= preds_sarima%>%mutate(Entries = "S-ARIMA"),
  "entry_3"= preds_xgboost%>%mutate(Entries = "XGBoost"),
  "entry_4"= preds_dfm%>%mutate(Entries = "DFM")
)
)

plot_preds(data$TOURISM, predictions, countries_tourism[1:9], ncol = 3)
plot_preds(data$TOURISM, predictions, countries_tourism[10:18], ncol = 3)
plot_preds(data$TOURISM, predictions, countries_tourism[-1:-18], ncol = 3)


#### Save the results #### 
entries <- list(
  "entry_1"= preds_naive_1y%>%pull(value, Country), 
  "entry_2"= preds_sarima%>%pull(value, Country),
  "entry_3"= preds_xgboost%>%pull(value, Country),
  "entry_3"= preds_dfm%>%pull(value, Country)
)
save_entries(entries, "Submissions/TOURISM/results_october.json")
