###############################################################################
#                             Main file for PVI                               #
###############################################################################

library(lubridate)

#### Import global variables ####
source("R/utils/globalVariables.R")
source("R/utils/getData.R")
source("R/utils/functions.R")

#### Import the data ####
data <- getData("PVI")

#### Run the different models ####
date_to_pred <- ymd("2022-11-01")
current_date <- date_to_pred %m-% months(1)

source("R/PVI/LastPeriod_model.R") # to be converted to functions
source("R/PVI/Regarima.R") # to be converted to functions
source("R/PVI/XGBoost.R") # to be converted to functions
source("R/PVI/DFM.R") # to be converted to functions
source("R/PVI/ETS.R") # to be converted to functions

#### Plotting the results ####
predictions <- bind_rows(list(
  "entry_1" = preds_naive_1m %>% mutate(Entries = "Naive"),
  "entry_2" = preds_regarima %>% mutate(Entries = "REG-ARIMA"),
  "entry_3" = preds_xgboost %>% mutate(Entries = "XGBoost"),
  "entry_4" = preds_dfm %>% mutate(Entries = "DFM"),
  "entry_5" = preds_ets %>% mutate(Entries = "ETS")
))

plot_preds(data$PVI, predictions, countries_PVI[1:9], ncol = 3)
plot_preds(data$PVI, predictions, countries_PVI[10:18], ncol = 3)
plot_preds(data$PVI, predictions, countries_PVI[-1:-18], ncol = 3)

#### Analyse the residuals
resids <- bind_rows(list(
  "entry_1" = resid_naive_1m %>% mutate(Entries = "Naive"),
  "entry_2" = resid_regarima %>% mutate(Entries = "REG-ARIMA"),
  "entry_3" = resid_xgboost %>% mutate(Entries = "XGBoost"),
  "entry_4" = resid_dfm %>% mutate(Entries = "DFM"),
  "entry_5" = resid_ets %>% mutate(Entries = "ETS")
))

plot_statistics(get_metrics(resids, countries_PVI, current_date, as.Date("2022-01-01")))
plot_statistics(get_metrics(resids, countries_PVI, current_date, as.Date("2010-01-01")))

#### Save the results ####
entries <- list(
  "entry_1" = lapply(split(preds_naive_1m %>% pull(value, Country), names(preds_naive_1m %>% pull(value, Country))), unname),
  "entry_2" = lapply(split(preds_regarima %>% pull(value, Country), names(preds_regarima %>% pull(value, Country))), unname),
  "entry_3" = lapply(split(preds_xgboost %>% pull(value, Country), names(preds_xgboost %>% pull(value, Country))), unname),
  "entry_4" = lapply(split(preds_dfm %>% pull(value, Country), names(preds_dfm %>% pull(value, Country))), unname),
  "entry_5" = lapply(split(preds_ets %>% pull(value, Country), names(preds_ets %>% pull(value, Country))), unname)
)
month <- "november"
save_entries(entries, paste0("Submissions/PVI/results_", month, ".json"))
# entries = list("entry_5"= preds_ets%>%pull(value, Country))
# add_entries(entries, "Submissions/PVI/results_november.json")

#### Save the results in S3 ####
system(
  paste(
  paste0("mc cp Submissions/PVI/results_", month, ".json"),
  paste0("s3/projet-esa-nowcasting/submissions/PVI/results_", month, ".json")
  )
)

#### Save the data in S3 ####
save(data, file = paste0("data_PVI_", month, ".R"))
system(
  paste(
    paste0("mc cp data_PVI_", month, ".R"),
    paste0("s3/projet-esa-nowcasting/data/PVI/data_", month, ".R")
  )
)
