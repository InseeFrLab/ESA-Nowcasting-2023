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

list(
  tar_target(
    name = data_info,
    command = yaml::read_yaml("data.yaml")
  ),
  tar_target(
    name = challenges,
    command = yaml::read_yaml("challenges.yaml")
  ),
  tar_target(
    name = models,
    command = yaml::read_yaml("models.yaml")
  ),
  tar_target(
    name = data,
    command = get_data(data_info, challenges)
  ),
  tar_target(
    name = ets_ppi,
    command = run_ETS("PPI", challenges, data, models)
  ),
  tar_target(
    name = ets_pvi,
    command = run_ETS("PVI", challenges, data, models)
  ),
  tar_target(
    name = ets_tourism,
    command = run_ETS("TOURISM", challenges, data, models)
  ),
  tar_target(
    name = regarima_ppi,
    command = run_regarima("PPI", challenges, data, models)
  ),
  tar_target(
    name = regarima_pvi,
    command = run_regarima("PVI", challenges, data, models)
  ),
  tar_target(
    name = regarima_tourism,
    command = run_regarima("TOURISM", challenges, data, models)
  ),
  tar_target(
    name = dfms_ppi,
    command = run_DFMs("PPI", challenges, data, models)
  ),
  tar_target(
    name = dfms_pvi,
    command = run_DFMs("PVI", challenges, data, models)
  ),
  tar_target(
    name = dfms_tourism,
    command = run_DFMs("TOURISM", challenges, data, models)
  ),
  tar_target(
    name = xgboost_ppi,
    command = run_xgboost_per_country(
      data = data,
      config_models = models,
      config_env = challenges,
      challenge = "PPI"
    )
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
  tar_target(
    name = xgboost_tourism,
    command = run_xgboost_per_country(
      data = data,
      config_models = models,
      config_env = challenges,
      challenge = "TOURISM"
    )
  ),
  # tar_target(
  #   name = lstm_ppi,
  #   command = run_lstm_per_country(
  #     data = data,
  #     config_models = models,
  #     config_env = challenges,
  #     challenge = "PPI"
  #   )
  # ),
  # tar_target(
  #   name = lstm_pvi,
  #   command = run_lstm_per_country(
  #     data = data,
  #     config_models = models,
  #     config_env = challenges,
  #     challenge = "PVI"
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
  tar_target(
    name = predictions_ppi,
    command = bind_rows(list(
      "entry_1" = regarima_ppi$preds %>% mutate(Entries = "REG-ARIMA"),
      "entry_2" = dfms_ppi$preds %>% mutate(Entries = "DFM"),
      "entry_3" = ets_ppi$preds %>% mutate(Entries = "ETS"),
      "entry_4" = xgboost_ppi$preds %>% mutate(Entries = "XGBOOST")
      # "entry_5" = lstm_ppi$preds %>% mutate(Entries = "LSTM")
    ))
  ),
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
    name = predictions_tourism,
    command = bind_rows(list(
      "entry_1" = regarima_tourism$preds %>% mutate(Entries = "REG-ARIMA"),
      "entry_2" = dfms_tourism$preds %>% mutate(Entries = "DFM"),
      "entry_3" = ets_tourism$preds %>% mutate(Entries = "ETS"),
      "entry_4" = xgboost_tourism$preds %>% mutate(Entries = "XGBOOST")
      # "entry_5" = lstm_tourism$preds %>% mutate(Entries = "LSTM")
    ))
  ),
  tar_target(
    name = resids_ppi,
    command = bind_rows(list(
      "entry_1" = regarima_ppi$resids %>% mutate(Entries = "REG-ARIMA"),
      "entry_2" = dfms_ppi$resids %>% mutate(Entries = "DFM"),
      "entry_3" = ets_ppi$resids %>% mutate(Entries = "ETS"),
      "entry_4" = xgboost_ppi$resids %>% mutate(Entries = "XGBOOST")
      # "entry_5" = lstm_ppi$resids %>% mutate(Entries = "LSTM")
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
    name = resids_tourism,
    command = bind_rows(list(
      "entry_1" = regarima_tourism$resids %>% mutate(Entries = "REG-ARIMA"),
      "entry_2" = dfms_tourism$resids %>% mutate(Entries = "DFM"),
      "entry_3" = ets_tourism$resids %>% mutate(Entries = "ETS"),
      "entry_4" = xgboost_tourism$resids %>% mutate(Entries = "XGBOOST")
      # "entry_5" = lstm_tourism$resids %>% mutate(Entries = "LSTM")
    ))
  ),
  tar_target(
    name = plot_preds_ppi_1,
    command = plot_preds("PPI", challenges, data, predictions_ppi, challenges$PPI$countries[1:4])
  ),
  tar_target(
    name = plot_preds_ppi_2,
    command = plot_preds("PPI", challenges, data, predictions_ppi, challenges$PPI$countries[5:8])
  ),
  tar_target(
    name = plot_preds_ppi_3,
    command = plot_preds("PPI", challenges, data, predictions_ppi, challenges$PPI$countries[9:12])
  ),
  tar_target(
    name = plot_preds_ppi_4,
    command = plot_preds("PPI", challenges, data, predictions_ppi, challenges$PPI$countries[13:16])
  ),
  tar_target(
    name = plot_preds_ppi_5,
    command = plot_preds("PPI", challenges, data, predictions_ppi, challenges$PPI$countries[17:20])
  ),
  tar_target(
    name = plot_preds_ppi_6,
    command = plot_preds("PPI", challenges, data, predictions_ppi, challenges$PPI$countries[21:24])
  ),
  tar_target(
    name = plot_preds_ppi_7,
    command = plot_preds("PPI", challenges, data, predictions_ppi, challenges$PPI$countries[25:26])
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
    name = plot_preds_tourism_1,
    command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[1:4])
  ),
  tar_target(
    name = plot_preds_tourism_2,
    command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[5:8])
  ),
  tar_target(
    name = plot_preds_tourism_3,
    command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[9:12])
  ),
  tar_target(
    name = plot_preds_tourism_4,
    command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[13:16])
  ),
  tar_target(
    name = plot_preds_tourism_5,
    command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[17:20])
  ),
  tar_target(
    name = plot_preds_tourism_6,
    command = plot_preds("TOURISM", challenges, data, predictions_tourism, challenges$TOURISM$countries[21:24])
  ),
  tar_target(
    name = plot_resids_ppi,
    command = plot_statistics(get_metrics(resids_ppi, challenges$PPI$countries, challenges$DATES$current_date, as.Date("2022-01-01")))
  ),
  tar_target(
    name = plot_resids_pvi,
    command = plot_statistics(get_metrics(resids_pvi, challenges$PVI$countries, challenges$DATES$current_date, as.Date("2022-01-01")))
  ),
  tar_target(
    name = plot_resids_tourism,
    command = plot_statistics(get_metrics(resids_tourism, challenges$TOURISM$countries, as.Date("2019-12-31"), as.Date("2010-01-01")))
  ),
  tar_target(
    name = save_ppi,
    command = save_entries(
      "PPI", list(
        "entry_1" = regarima_ppi,
        "entry_2" = dfms_ppi,
        "entry_3" = ets_ppi,
        "entry_4" = xgboost_ppi
        # "entry_5" = lstm_ppi
      ),
      challenges
    )
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
  ),
  tar_target(
    name = save_tourism,
    command = save_entries(
      "TOURISM", list(
        "entry_1" = regarima_tourism,
        "entry_2" = dfms_tourism,
        "entry_3" = ets_tourism,
        "entry_4" = xgboost_tourism
        # "entry_5" = lstm_tourism
      ),
      challenges
    )
  )
)