#' Post-Mortem Analysis functions
#'
#' This module provides a collection of functions for conducting data analysis 
#' and performing a post-mortem evaluation of predictions. It includes functions
#' specifically designed for analyzing the performance, accuracy, and quality 
#' of predictions made in a forecasting or predictive modeling context. 
#' These functions enable retrospective examination, identification of insights, 
#' and lessons learned from the predictions made.

create_table_past_submissions <- function(submitted_models = yaml::read_yaml("submitted_models.yaml"),
                                          challenge = "PPI",
                                          submissions_folder = "Submissions",
                                          by_entry = FALSE) {
  past_months <- submitted_models$PAST_MONTHS
  date <- as.Date(submitted_models$START_DATE)
  df_submissions <- data.frame(matrix(ncol = 4, nrow = 0))
  x <- c("Country", "Date", "value", "Entry")

  for (month in past_months) {
    while (tolower(format(date, format = "%B")) != month) {
      date <- date + months(1)
      print(month)
    }

    json_data <- jsonlite::fromJSON(
      paste0(submissions_folder, "/", challenge, "/results_", month, ".json")
    )
    list_df_entries <- lapply(json_data, as.data.frame)

    for (entry in names(list_df_entries)) {
      if (length(list_df_entries[[entry]]) > 0) {
        df_entry <- as.data.frame(unlist(list_df_entries[[entry]])) |>
          rename(value = 1) |>
          rownames_to_column(var = "Country") |>
          mutate(
            Date = date,
            value = as.numeric(value),
            Entries = ifelse(
              by_entry,
              entry,
              submitted_models[[challenge]][[month]][[entry]]
            )
          ) |>
          relocate(Country, Date, value, Entries)

        df_submissions <- df_submissions |>
          rbind(df_entry)
      }
    }
  }
  return(df_submissions)
}


get_recent_data <- function(data = get_data(
                              yaml::read_yaml("data.yaml"),
                              yaml::read_yaml("challenges.yaml")
                            ),
                            config_env = yaml::read_yaml("challenges.yaml"),
                            submitted_models = yaml::read_yaml("submitted_models.yaml"),
                            challenge = "PPI") {
  recent_data <- data[[challenge]]$data |>
    filter(
      nace_r2 == config_env[[challenge]]$principal_nace,
      time >= as.Date(submitted_models$START_DATE)
    ) |>
    select(geo, time, nace_r2, values) |>
    drop_na(values)

  return(recent_data)
}


get_residuals_past_months <- function(df_submissions = create_table_past_submissions(),
                                      recent_data = get_recent_data()) {
  df_residuals <- df_submissions |>
    rename(Prediction = value) |>
    left_join(recent_data |>
      select(-nace_r2) |>
      rename(
        Date = time,
        Country = geo,
        TrueValue = values
      )) |>
    mutate(
      Error = Prediction - TrueValue,
      AbsoluteError = abs(Error),
      SquaredError = Error**2
    ) |>
    filter(!is.na(Error)) |>
    arrange(Country, Date)

  return(df_residuals)
}
