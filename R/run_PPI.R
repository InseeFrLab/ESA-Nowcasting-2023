###############################################################################
#                             Main file for PPI                               #  
###############################################################################
rm(list = ls())
#### Import global variables ####
source("R/utils/globalVariables.R")
source("R/utils/getData.R")
source("R/utils/save_entries.R")
source("R/utils/plot_routines.R")

#### Import the data ####
data <- getData("PPI")

#### Run the different models ####
date_to_pred <- ymd("2022-11-01")

source("R/PPI/LastPeriod_model.R") # to be converted to functions
# source("R/PPI/S-ARIMA.R") # to be converted to functions
source("R/PPI/Regarima_ppi.R") # to be converted to functions
source("R/PPI/XGBoost.R") # to be converted to functions
source("R/PPI/DFM.R") # to be converted to functions

#### Plotting the results #### 
predictions <- bind_rows(list(
  "entry_1"= preds_naive_1m%>%mutate(Entries = "Naive"), 
  "entry_2"= preds_sarima%>%mutate(Entries = "Regarima"),
  "entry_3"= preds_xgboost%>%mutate(Entries = "XGBoost"),
  "entry_4"= preds_dfm%>%mutate(Entries = "DFM")
)
)

plot_preds(data$PPI, predictions, countries_PPI[1:9], ncol = 3)
plot_preds(data$PPI, predictions, countries_PPI[10:18], ncol = 3)
plot_preds(data$PPI, predictions, countries_PPI[-1:-18], ncol = 3)


#### Save the results #### 
entries <- list(
  "entry_1"= preds_naive_1m%>%pull(value, Country), 
  "entry_2"= preds_sarima%>%pull(value, Country),
  "entry_3"= preds_xgboost%>%pull(value, Country),
  "entry_4"= preds_dfm%>%pull(value, Country)
)
save_entries(entries, "Submissions/PPI/results_november.json")
