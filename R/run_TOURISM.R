###############################################################################
#                             Main file for TOURISM                           #  
###############################################################################

#### Import global variables ####
source("R/utils/globalVariables.R")
source("R/utils/getData.R")
source("R/utils/save_entries.R")
source("R/utils/plot_routines.R")

#### Import the data ####
data <- getData("TOURISM")

#### Run the different models ####
source("R/TOURISM/LastPeriod_model.R") # to be converted to functions
source("R/TOURISM/S-ARIMA.R") # to be converted to functions

#### Plotting the results #### 
predictions <- bind_rows(list(
  "entry_1"= preds_naive_1y%>%mutate(Entries = "Naive"), 
  "entry_2"= preds_sarima%>%mutate(Entries = "S-ARIMA"))
)

plot_preds(data$TOURISM, predictions, countries_tourism[1:9], ncol = 3)
plot_preds(data$TOURISM, predictions, countries_tourism[10:18], ncol = 3)
plot_preds(data$TOURISM, predictions, countries_tourism[-1:-18], ncol = 3)


#### Save the results #### 
entries <- list(
  "entry_1"= preds_naive_1y%>%pull(value, Country), 
  "entry_2"= preds_sarima%>%pull(value, Country)
)
save_entries(entries, "Submissions/TOURISM/results_october.json")