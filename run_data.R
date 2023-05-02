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
    name = eurostat,
    command = get_data_from_eurostat(data_info),
  ),
  tar_target(
    name = yahoo,
    command = get_data_from_yahoo(data_info),
  ),
  tar_target(
    name = ember,
    command = get_data_from_ember(data_info),
  ),
  tar_target(
    name = week_ends,
    command = get_weekend_days(data_info, challenges),
  ),
  tar_target(
    name = destatis,
    command = get_data_from_destatis(data_info),
  ),
  tar_target(
    name = wifo,
    command = get_data_from_wifo(data_info),
  ),
  tar_target(
    name = gtrends,
    command = get_data_from_google_trends(data_info),
  ),
  tar_target(
    name = data,
    command = get_data(data_info, c(eurostat, yahoo, ember, week_ends, destatis, wifo, gtrends))
  ),
  tar_target(
    name = data_saved,
    command = save_data(data, challenges),
    format = "file"
  )
)
