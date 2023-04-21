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
    name = plot_errors_ppi_1,
    command = plot_preds("PPI", challenges,
                         recent_data_ppi, past_submissions_ppi,
                         challenges$PPI$countries[1:4],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_ppi_2,
    command = plot_preds("PPI", challenges,
                         recent_data_ppi, past_submissions_ppi,
                         challenges$PPI$countries[5:8],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_ppi_3,
    command = plot_preds("PPI", challenges,
                         recent_data_ppi, past_submissions_ppi,
                         challenges$PPI$countries[9:12],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_ppi_4,
    command = plot_preds("PPI", challenges,
                         recent_data_ppi, past_submissions_ppi,
                         challenges$PPI$countries[13:16],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_ppi_5,
    command = plot_preds("PPI", challenges,
                         recent_data_ppi, past_submissions_ppi,
                         challenges$PPI$countries[17:20],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_ppi_6,
    command = plot_preds("PPI", challenges,
                         recent_data_ppi, past_submissions_ppi,
                         challenges$PPI$countries[21:24],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_ppi_7,
    command = plot_preds("PPI", challenges,
                         recent_data_ppi, past_submissions_ppi,
                         challenges$PPI$countries[25:26],
                         xlim = as.Date(submitted_models$START_DATE))
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
    name = plot_errors_pvi_1,
    command = plot_preds("PVI", challenges,
                         recent_data_pvi, past_submissions_pvi,
                         challenges$PVI$countries[1:4],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_pvi_2,
    command = plot_preds("PVI", challenges,
                         recent_data_pvi, past_submissions_pvi,
                         challenges$PVI$countries[5:8],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_pvi_3,
    command = plot_preds("PVI", challenges,
                         recent_data_pvi, past_submissions_pvi,
                         challenges$PVI$countries[9:12],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_pvi_4,
    command = plot_preds("PVI", challenges,
                         recent_data_pvi, past_submissions_pvi,
                         challenges$PVI$countries[13:16],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_pvi_5,
    command = plot_preds("PVI", challenges,
                         recent_data_pvi, past_submissions_pvi,
                         challenges$PVI$countries[17:20],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_pvi_6,
    command = plot_preds("PVI", challenges,
                         recent_data_pvi, past_submissions_pvi,
                         challenges$PVI$countries[21:23],
                         xlim = as.Date(submitted_models$START_DATE))
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
  ),

  tar_target(
    name = plot_errors_tourism_1,
    command = plot_preds("TOURISM", challenges,
                         recent_data_tourism, past_submissions_tourism,
                         challenges$TOURISM$countries[1:4],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_tourism_2,
    command = plot_preds("TOURISM", challenges,
                         recent_data_tourism, past_submissions_tourism,
                         challenges$TOURISM$countries[5:8],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_tourism_3,
    command = plot_preds("TOURISM", challenges,
                         recent_data_tourism, past_submissions_tourism,
                         challenges$TOURISM$countries[9:12],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_tourism_4,
    command = plot_preds("TOURISM", challenges,
                         recent_data_tourism, past_submissions_tourism,
                         challenges$TOURISM$countries[13:16],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_tourism_5,
    command = plot_preds("TOURISM", challenges,
                         recent_data_tourism, past_submissions_tourism,
                         challenges$TOURISM$countries[17:20],
                         xlim = as.Date(submitted_models$START_DATE))
  ),
  tar_target(
    name = plot_errors_tourism_6,
    command = plot_preds("TOURISM", challenges,
                         recent_data_tourism, past_submissions_tourism,
                         challenges$TOURISM$countries[21:24],
                         xlim = as.Date(submitted_models$START_DATE))
  )
)
