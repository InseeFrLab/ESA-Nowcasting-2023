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

data <- targets::tar_read(data, store = "store_data")

list(
  tar_target(
    name = challenge,
    command = "PPI"
  ),
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
  #   name = ets_ppi,
  #   command = run_ETS(challenge, challenges, data, models)
  # ),
  # tar_target(
  #   name = regarima_ppi,
  #   command = run_regarima(challenge, challenges, data, models)
  # ),
  # tar_target(
  #   name = dfms_ppi,
  #   command = run_DFMs(challenge, challenges, data, models)
  # ),
  # tar_target(
  #   name = xgboost_ppi,
  #   command = run_xgboost_per_country(
  #     data = data,
  #     config_models = models,
  #     config_env = challenges,
  #     challenge = challenge
  #   )
  # ),
  # tar_target(
  #   name = lstm_ppi,
  #   command = run_lstm_per_country(
  #     data = data,
  #     config_models = models,
  #     config_env = challenges,
  #     challenge = challenge
  #   )
  # ),
  # tar_target(
  #   name = predictions_ppi,
  #   command = bind_rows(list(
  #     "entry_1" = regarima_ppi$preds %>% mutate(Entries = "REG-ARIMA"),
  #     "entry_2" = dfms_ppi$preds %>% mutate(Entries = "DFM"),
  #     "entry_3" = ets_ppi$preds %>% mutate(Entries = "ETS"),
  #     "entry_4" = xgboost_ppi$preds %>% mutate(Entries = "XGBOOST"),
  #     "entry_5" = lstm_ppi$preds %>% mutate(Entries = "LSTM")
  #   ))
  # ),
  # tar_target(
  #   name = resids_ppi,
  #   command = bind_rows(list(
  #     "entry_1" = regarima_ppi$resids %>% mutate(Entries = "REG-ARIMA"),
  #     "entry_2" = dfms_ppi$resids %>% mutate(Entries = "DFM"),
  #     "entry_3" = ets_ppi$resids %>% mutate(Entries = "ETS"),
  #     "entry_4" = xgboost_ppi$resids %>% mutate(Entries = "XGBOOST"),
  #     "entry_5" = lstm_ppi$resids %>% mutate(Entries = "LSTM")
  #   ))
  # ),
  # tar_target(
  #   name = plot_preds_ppi_1,
  #   command = plot_preds(challenge, challenges, data, predictions_ppi, challenges$PPI$countries[1:4])
  # ),
  # tar_target(
  #   name = plot_preds_ppi_2,
  #   command = plot_preds(challenge, challenges, data, predictions_ppi, challenges$PPI$countries[5:8])
  # ),
  # tar_target(
  #   name = plot_preds_ppi_3,
  #   command = plot_preds(challenge, challenges, data, predictions_ppi, challenges$PPI$countries[9:12])
  # ),
  # tar_target(
  #   name = plot_preds_ppi_4,
  #   command = plot_preds(challenge, challenges, data, predictions_ppi, challenges$PPI$countries[13:16])
  # ),
  # tar_target(
  #   name = plot_preds_ppi_5,
  #   command = plot_preds(challenge, challenges, data, predictions_ppi, challenges$PPI$countries[17:20])
  # ),
  # tar_target(
  #   name = plot_preds_ppi_6,
  #   command = plot_preds(challenge, challenges, data, predictions_ppi, challenges$PPI$countries[21:24])
  # ),
  # tar_target(
  #   name = plot_preds_ppi_7,
  #   command = plot_preds(challenge, challenges, data, predictions_ppi, challenges$PPI$countries[25:26])
  # ),
  # tar_target(
  #   name = plot_resids_ppi,
  #   command = plot_statistics(get_metrics(resids_ppi, challenges$PPI$countries, challenges$DATES$current_date, as.Date("2022-01-01")))
  # ),
  # tar_target(
  #   name = save_ppi,
  #   command = save_entries(
  #     challenge, list(
  #       "entry_1" = regarima_ppi,
  #       "entry_2" = dfms_ppi,
  #       "entry_3" = ets_ppi,
  #       "entry_4" = xgboost_ppi,
  #       "entry_5" = lstm_ppi
  #     ),
  #     challenges
  #   )
  # )
)