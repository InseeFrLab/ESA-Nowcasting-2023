#' Pipeline for PVI challenge
#'
#' This pipeline executes five different models (Reg-Arima, DFM, XGBoost, ETS, 
#' LSTM) that were utilized in the ESA Nowcasting Challenge. The purpose is to 
#' perform nowcasting of the Production Volume in Industry based on these models. 
#' If the `SAVE_TO_S3` variable is set to TRUE, the submission can be saved in 
#' a S3 bucket.

library(targets)

# Set target options:
tar_option_set(
  packages = c(
    "xts", "lubridate", "dplyr", "tidyr", "data.table",
    "dfms", "jsonlite", "styler", "visNetwork"
  ),
  memory = "transient",
  garbage_collection = TRUE
)
options(dplyr.summarise.inform = FALSE)

# Execute files stored in R/
tar_source(files = "R")

# Saving flag to S3 (TOKEN NEEDED)
SAVE_TO_S3 <- TRUE

# Pipeline
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
    name = data,
    command = read_data_from_s3(challenges, data_info),
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
  tar_target(
    name = lstm_pvi,
    command = run_lstm_per_country(
      data = data,
      config_models = models,
      config_env = challenges,
      challenge = "PVI"
    )
  ),
  tar_target(
    name = predictions_pvi,
    command = bind_rows(list(
      "entry_1" = regarima_pvi$preds %>% mutate(Entries = "REG-ARIMA"),
      "entry_2" = dfms_pvi$preds %>% mutate(Entries = "DFM"),
      "entry_3" = ets_pvi$preds %>% mutate(Entries = "ETS"),
      "entry_4" = xgboost_pvi$preds %>% mutate(Entries = "XGBOOST"),
      "entry_5" = lstm_pvi$preds %>% mutate(Entries = "LSTM")
    ))
  ),
  tar_target(
    name = save_pvi,
    command = save_entries(
      "PVI", list(
        "entry_1" = regarima_pvi,
        "entry_2" = dfms_pvi,
        "entry_3" = ets_pvi,
        "entry_4" = xgboost_pvi,
        "entry_5" = lstm_pvi
      ),
      challenges,
      SAVE_TO_S3
    )
  )
)
