###############################################################################
#                             Main file for PVI                               #  
###############################################################################
rm(list = ls())
#### Import global variables ####
source("R/utils/globalVariables.R")
source("R/utils/getData.R")
source("R/utils/save_entries.R")
source("R/utils/plot_routines.R")

#### Import the data ####
data <- getData("PVI")

#### Run the different models ####
date_to_pred <- "2022-10-01"

source("R/PVI/LastPeriod_model.R") # to be converted to functions
source("R/PVI/S-ARIMA.R") # to be converted to functions
source("R/PVI/DFM.R") # to be converted to functions

#### Plotting the results #### 
predictions <- bind_rows(list(
  "entry_1"= preds_naive_1m%>%mutate(Entries = "Naive"), 
  "entry_2"= preds_sarima%>%mutate(Entries = "S-ARIMA"),
  "entry_3"= preds_dfm%>%mutate(Entries = "DFM")
)
)

plot_preds(data$PVI, predictions, countries_PVI[1:9], ncol = 3)
plot_preds(data$PVI, predictions, countries_PVI[10:18], ncol = 3)
plot_preds(data$PVI, predictions, countries_PVI[-1:-18], ncol = 3)


#### Save the results #### 
entries <- list(
  "entry_1"= preds_naive_1m%>%pull(value, Country), 
  "entry_2"= preds_sarima%>%pull(value, Country),
  "entry_3"= preds_dfm%>%pull(value, Country)
)
save_entries(entries, "Submissions/PVI/results_october.json")