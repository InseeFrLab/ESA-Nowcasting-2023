# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c(
    "xts", "lubridate", "dplyr", "tidyr", "data.table", "tibble",
    "dfms", "cowplot", "jsonlite", "ggplot2", "styler", "visNetwork"
  ),
  memory = "transient",
  garbage_collection = TRUE
)
options(dplyr.summarise.inform = FALSE)

tar_source(files = "R")

data <- tar_read(data, store = "store_data")

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
    name = submitted_models_file,
    command = "submitted_models.yaml",
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
    name = submitted_models,
    command = yaml::read_yaml(submitted_models_file),
  ),
  
  tar_target(
    name = past_submissions_ppi,
    command = create_table_past_submissions(submitted_models, "PPI", FALSE)
  ),
  tar_target(
    name = past_submissions_ppi_by_entry,
    command = create_table_past_submissions(submitted_models, "PPI", TRUE)
  ),
  tar_target(
    name = recent_data_ppi,
    command = get_recent_data(data, challenges, submitted_models, "PPI")
  ),
  tar_target(
    name = past_errors_ppi,
    command = get_residuals_past_months(past_submissions_ppi, recent_data_ppi)
  ),
  tar_target(
    name = past_errors_ppi_by_entry,
    command = get_residuals_past_months(past_submissions_ppi_by_entry, recent_data_ppi)
  ),
  tar_target(
    name = past_submissions_pvi,
    command = create_table_past_submissions(submitted_models, "PVI", FALSE)
  ),
  tar_target(
    name = past_submissions_pvi_by_entry,
    command = create_table_past_submissions(submitted_models, "PVI", TRUE)
  ),
  tar_target(
    name = recent_data_pvi,
    command = get_recent_data(data, challenges, submitted_models, "PVI")
  ),
  tar_target(
    name = past_errors_pvi,
    command = get_residuals_past_months(past_submissions_pvi, recent_data_pvi)
  ),
  tar_target(
    name = past_errors_pvi_by_entry,
    command = get_residuals_past_months(past_submissions_pvi_by_entry, recent_data_pvi)
  ),
  tar_target(
    name = past_submissions_tourism,
    command = create_table_past_submissions(submitted_models, "TOURISM", FALSE)
  ),
  tar_target(
    name = past_submissions_tourism_by_entry,
    command = create_table_past_submissions(submitted_models, "TOURISM", TRUE)
  ),
  tar_target(
    name = recent_data_tourism,
    command = get_recent_data(data, challenges, submitted_models, "TOURISM")
  ),
  tar_target(
    name = past_errors_tourism,
    command = get_residuals_past_months(past_submissions_tourism, recent_data_tourism)
  ),
  tar_target(
    name = past_errors_tourism_by_entry,
    command = get_residuals_past_months(past_submissions_tourism_by_entry, recent_data_tourism)
  )
)
