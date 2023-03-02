# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c(
    "xts", "lubridate", "dplyr", "tidyr", "data.table",
    "dfms", "cowplot", "jsonlite", "ggplot2", "styler", "visNetwork"
  ),
  memory = "transient",
  garbage_collection = TRUE
)
options(dplyr.summarise.inform = FALSE)

tar_source(files = "R")

#data <- targets::tar_read(data, store = "store_data")

list(
  tar_target(
    name = data_info_file,
    command = "data.yaml",
    format = "file"
  ),
  tar_target(
    name = challenges_file,
    command = "challenges.yaml",
    format = "file"
  ),
  tar_target(
    name = models_file,
    command = "models.yaml",
    format = "file"
  ),
  tar_target(
    name = data_info,
    command = yaml::read_yaml(data_info_file),
  ),
  tar_target(
    name = challenges,
    command = yaml::read_yaml(challenges_file),
  ),
  tar_target(
    name = models,
    command = yaml::read_yaml(models_file),
  )#,
  # tar_target(
  #   name = ets_tourism,
  #   command = run_ETS("TOURISM", challenges, data, models)
  # ),
  # tar_target(
  #   name = regarima_tourism,
  #   command = run_regarima("TOURISM", challenges, data, models)
  # ),
  # tar_target(
  #   name = dfms_tourism,
  #   command = run_DFMs("TOURISM", challenges, data, models)
  # ),
  # tar_target(
  #   name = xgboost_tourism,
  #   command = run_xgboost_per_country(
  #     data = data,
  #     config_models = models,
  #     config_env = challenges,
  #     challenge = "TOURISM"
  #   )
  # ),
  # tar_target(
  #   name = lstm_tourism,
  #   command = run_lstm_per_country(
  #     data = data,
  #     config_models = models,
  #     config_env = challenges,
  #     challenge = "TOURISM"
  #   )
  # ),
  # tar_target(
  #   name = predictions_tourism,
  #   command = bind_rows(list(
  #     "entry_1" = regarima_tourism$preds %>% mutate(Entries = "REG-ARIMA"),
  #     "entry_2" = dfms_tourism$preds %>% mutate(Entries = "DFM"),
  #     "entry_3" = ets_tourism$preds %>% mutate(Entries = "ETS"),
  #     "entry_4" = xgboost_tourism$preds %>% mutate(Entries = "XGBOOST"),
  #     "entry_5" = lstm_tourism$preds %>% mutate(Entries = "LSTM")
  #   ))
  # ),
  # tar_target(
  #   name = resids_tourism,
  #   command = bind_rows(list(
  #     "entry_1" = regarima_tourism$resids %>% mutate(Entries = "REG-ARIMA"),
  #     "entry_2" = dfms_tourism$resids %>% mutate(Entries = "DFM"),
  #     "entry_3" = ets_tourism$resids %>% mutate(Entries = "ETS"),
  #     "entry_4" = xgboost_tourism$resids %>% mutate(Entries = "XGBOOST"),
  #     "entry_5" = lstm_tourism$resids %>% mutate(Entries = "LSTM")
  #   ))
  # ),
  # tar_target(
  #   name = plot_preds_tourism_1,
  #   command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[1:4])
  # ),
  # tar_target(
  #   name = plot_preds_tourism_2,
  #   command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[5:8])
  # ),
  # tar_target(
  #   name = plot_preds_tourism_3,
  #   command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[9:12])
  # ),
  # tar_target(
  #   name = plot_preds_tourism_4,
  #   command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[13:16])
  # ),
  # tar_target(
  #   name = plot_preds_tourism_5,
  #   command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[17:20])
  # ),
  # tar_target(
  #   name = plot_preds_tourism_6,
  #   command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[21:24])
  # ),
  # tar_target(
  #   name = plot_resids_tourism,
  #   command = plot_statistics(get_metrics(resids_tourism, challenges$TOURISM$countries, as.Date("2019-12-31"), as.Date("2010-01-01")))
  # ),
  # tar_target(
  #   name = save_tourism,
  #   command = save_entries(
  #     "TOURISM", list(
  #       "entry_1" = regarima_tourism,
  #       "entry_2" = dfms_tourism,
  #       "entry_3" = ets_tourism,
  #       "entry_4" = xgboost_tourism,
  #       "entry_5" = lstm_tourism
  #     ),
  #     challenges
  #   )
  # )
)