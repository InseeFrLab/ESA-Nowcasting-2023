library(targets)
tar_option_set(
  packages = c("lubridate"), # packages that your targets need to run
  format = "rds" # default storage format
)

tar_source("R/utils/globalVariables.R")
tar_source("R/utils/getData.R")
tar_source("R/PVI/LastPeriod_model.R")

list(
  tar_target(
    name = data,
    command = getData("PVI")
  ),
  tar_target(
    name = model,
    command = run_last_period_model(data)
  )
)
