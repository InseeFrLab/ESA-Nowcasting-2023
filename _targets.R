# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("xts", "lubridate", "dplyr", "dfms", "cowplot", "jsonlite", "data.table"),
  memory = "transient",
  garbage_collection = TRUE
)
options(dplyr.summarise.inform = FALSE)

tar_source("R/data_preprocessing.R")
tar_source("R/data_retrieval.R")
tar_source("R/functions.R")
tar_source("R/regarima_functions.R")
tar_source("R/ets_functions.R")
tar_source("R/dfms_functions.R")

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
    command =  run_regarima("PPI", challenges, data, models)
  ),
  tar_target(
    name = regarima_pvi,
    command =  run_regarima("PVI", challenges, data, models)
  ),
  tar_target(
    name = regarima_tourism,
    command =  run_regarima("TOURISM", challenges, data, models)
  ),
  tar_target(
    name = dfms_ppi,
    command =  run_DFMs("PPI", challenges, data, models)
  ),
  tar_target(
    name = dfms_pvi,
    command =  run_DFMs("PVI", challenges, data, models)
  ),
  tar_target(
    name = dfms_tourism,
    command =  run_DFMs("TOURISM", challenges, data, models)
  )
)
