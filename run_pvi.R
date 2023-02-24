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
  ),
  tar_target(
    name = ets_pvi,
    command = run_ETS("PVI", challenges, data, models)
  ),
  tar_target(
    name = regarima_pvi,
    command = run_regarima("PVI", challenges, data, models)
  ),
  tar_target(
    name = dfms_pvi,
    command = run_DFMs("PVI", challenges, data, models)
  ),
  tar_target(
    name = xgboost_pvi,
    command = run_xgboost_per_country(
      data = data,
      config_models = models,
      config_env = challenges,
      challenge = "PVI"
    )
  ),
  # tar_target(
  #   name = lstm_pvi,
  #   command = run_lstm_per_country(
  #     data = data,
  #     config_models = models,
  #     config_env = challenges,
  #     challenge = "PVI"
  #   )
  # ),
  tar_target(
    name = predictions_pvi,
    command = bind_rows(list(
      "entry_1" = regarima_pvi$preds %>% mutate(Entries = "REG-ARIMA"),
      "entry_2" = dfms_pvi$preds %>% mutate(Entries = "DFM"),
      "entry_3" = ets_pvi$preds %>% mutate(Entries = "ETS"),
      "entry_4" = xgboost_pvi$preds %>% mutate(Entries = "XGBOOST")
      # "entry_5" = lstm_pvi$preds %>% mutate(Entries = "LSTM")
    ))
  ),
  tar_target(
    name = resids_pvi,
    command = bind_rows(list(
      "entry_1" = regarima_pvi$resids %>% mutate(Entries = "REG-ARIMA"),
      "entry_2" = dfms_pvi$resids %>% mutate(Entries = "DFM"),
      "entry_3" = ets_pvi$resids %>% mutate(Entries = "ETS"),
      "entry_4" = xgboost_pvi$resids %>% mutate(Entries = "XGBOOST")
      # "entry_5" = lstm_pvi$resids %>% mutate(Entries = "LSTM")
    ))
  ),
  tar_target(
    name = plot_preds_pvi_1,
    command = plot_preds("PVI", challenges, data, predictions_pvi, challenges$PVI$countries[1:4])
  ),
  tar_target(
    name = plot_preds_pvi_2,
    command = plot_preds("PVI", challenges, data, predictions_pvi, challenges$PVI$countries[5:8])
  ),
  tar_target(
    name = plot_preds_pvi_3,
    command = plot_preds("PVI", challenges, data, predictions_pvi, challenges$PVI$countries[9:12])
  ),
  tar_target(
    name = plot_preds_pvi_4,
    command = plot_preds("PVI", challenges, data, predictions_pvi, challenges$PVI$countries[13:16])
  ),
  tar_target(
    name = plot_preds_pvi_5,
    command = plot_preds("PVI", challenges, data, predictions_pvi, challenges$PVI$countries[17:20])
  ),
  tar_target(
    name = plot_preds_pvi_6,
    command = plot_preds("PVI", challenges, data, predictions_pvi, challenges$PVI$countries[21:23])
  ),
  tar_target(
    name = plot_resids_pvi,
    command = plot_statistics(get_metrics(resids_pvi, challenges$PVI$countries, challenges$DATES$current_date, as.Date("2022-01-01")))
  ),
  tar_target(
    name = save_pvi,
    command = save_entries(
      "PVI", list(
        "entry_1" = regarima_pvi,
        "entry_2" = dfms_pvi,
        "entry_3" = ets_pvi,
        "entry_4" = xgboost_pvi
        # "entry_5" = lstm_pvi
      ),
      challenges
    )
  )
)