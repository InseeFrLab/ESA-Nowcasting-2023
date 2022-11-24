###############################################################################
#                             Main file for TOURISM                           #
###############################################################################
library(lubridate)
#### Import global variables ####
source("R/utils/globalVariables.R")
source("R/utils/getData.R")
source("R/utils/functions.R")

#### Import the data ####
data <- getData("TOURISM")

#### Run the different models ####
date_to_pred <- ymd("2022-11-01")
current_date <- date_to_pred %m-% months(1)

source("R/TOURISM/LastPeriod_model.R") # to be converted to functions
source("R/TOURISM/S-ARIMA.R") # to be converted to functions
source("R/TOURISM/XGBoost.R") # to be converted to functions
source("R/TOURISM/DFM.R") # to be converted to functions

#### Plotting the results ####
predictions <- bind_rows(list(
  "entry_1" = preds_naive_1y %>% mutate(Entries = "Naive"),
  "entry_2" = preds_sarima %>% mutate(Entries = "S-ARIMA"),
  "entry_3" = preds_xgboost %>% mutate(Entries = "XGBoost"),
  "entry_4" = preds_dfm %>% mutate(Entries = "DFM")
))

plot_preds(data$TOURISM, predictions, countries_tourism[1:9], ncol = 3)
plot_preds(data$TOURISM, predictions, countries_tourism[10:18], ncol = 3)
plot_preds(data$TOURISM, predictions, countries_tourism[-1:-18], ncol = 3)

#### Analyse the residuals
resids <- bind_rows(list(
  "entry_1" = resid_naive_1y %>% mutate(Entries = "Naive"),
  "entry_4" = resid_dfm %>% mutate(Entries = "DFM")
))

plot_statistics(get_metrics(resids, countries_tourism, current_date, as.Date("2010-01-01")))
plot_statistics(get_metrics(resids, countries_tourism, as.Date("2019-12-31"), as.Date("2010-01-01")))

#### Save the results ####
entries <- list(
  "entry_1" = lapply(split(preds_naive_1y %>% pull(value, Country), names(preds_naive_1y %>% pull(value, Country))), unname),
  "entry_2" = lapply(split(preds_sarima %>% pull(value, Country), names(preds_sarima %>% pull(value, Country))), unname),
  "entry_3" = lapply(split(preds_xgboost %>% pull(value, Country), names(preds_xgboost %>% pull(value, Country))), unname),
  "entry_4" = lapply(split(preds_dfm %>% pull(value, Country), names(preds_dfm %>% pull(value, Country))), unname)
)
save_entries(entries, "Submissions/TOURISM/results_november.json")
