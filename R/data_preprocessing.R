#' Data Preprocessing functions
#'
#' This module provides a collection of functions for performing data 
#' preprocessing tasks on different types of data or utils functions.


# Utils
get_latest_dates <- function(data, var) {
  # Returns a list with last available value for each variable of the xts dataset
  return(as.character(last(zoo::index(data)[!is.na(data[, var])])))
}

to_tsibble <- function(x) {
  x |>
    mutate(time = tsibble::yearmonth(time)) |>
    tidyr::drop_na() |>
    tsibble::as_tsibble(key = c(geo), index = time)
}


# Reshaping functions

reshape_eurostat_data <- function(data, country) {
  # Filter the list of data frames to only keep those from Eurostat
  subset_lists <- Filter(function(x) x$source == "Eurostat", data)

  # Reshape each data frame using pivot_wider and join the results together
  reshaped_data <- mapply(function(x, name) {
    x$data |>
      dplyr::mutate(var = name) |>
      dplyr::filter(geo %in% country) |>
      tidyr::pivot_wider(names_from = c(geo, var, names(x$filters)[3]), values_from = values)
  }, subset_lists, names(subset_lists), SIMPLIFY = FALSE) |>
    purrr::reduce(full_join, by = "time")

  return(reshaped_data)
}

reshape_daily_data <- function(data, source) {
  # Filter the list of data frames to only keep those from the desired source
  subset_lists <- Filter(function(x) x$source == source, data)

  # Reshape the daily data by taking monthly means
  reshaped_data <- lapply(subset_lists, function(x) {
    x$data |>
      mutate(
        month = month(time),
        year = year(time)
      ) |>
      group_by(year, month) |>
      summarise(
        across(-time, ~ mean(.x, na.rm = TRUE))
      ) |>
      ungroup() |>
      mutate(time = ymd(paste(year, month, "01", sep = "-"))) |>
      select(-c(year, month))
  }) |>
    purrr::reduce(full_join, by = "time")

  # Removing columns full of zeros
  reshaped_data <- reshaped_data[, colSums(reshaped_data != 0, na.rm = TRUE) > 0]

  return(reshaped_data)
}

reshape_gtrends_data <- function(data, country) {
  # Filter the list of data frames to only keep those from Google Trends
  subset_lists <- Filter(function(x) x$source == "gtrends", data)

  # Reshape each data frame using pivot_wider and join the results together
  reshaped_data <- mapply(function(x, name) {
    x$data |>
      dplyr::mutate(
        var = name,
        time = as.Date(time)
      ) |>
      dplyr::filter(geo %in% country) |>
      tidyr::pivot_wider(names_from = c(geo, var), values_from = x$short_name)
  }, subset_lists, names(subset_lists), SIMPLIFY = FALSE) |>
    purrr::reduce(full_join, by = "time")

  return(reshaped_data)
}


# Reformatting functions for ML methods

pivot_eurostat_data <- function(data) {
  # Filter the list of data frames to only keep those from Eurostat
  subset_lists <- Filter(function(x) x$source == "Eurostat", data)

  # Reshape each data frame using pivot_wider and join the results together
  pivoted_data <- mapply(function(x) {
    x$data |> pivot_wider(
      id_cols = c(geo, time),
      names_from = setdiff(
        colnames(x$data),
        c("geo", "time", "values")
      ),
      values_from = values,
      names_prefix = paste0(x$short_name, "_")
    )
  }, subset_lists, SIMPLIFY = FALSE) |>
    purrr::reduce(full_join)

  return(pivoted_data)
}

format_yahoo_data <- function(data) {
  # Filter the list of data frames to only keep those from Yahoo Finance
  subset_lists <- Filter(function(x) x$source == "Yahoo", data)

  if (length(subset_lists) == 0) {
    return(data.frame(month = integer(), year = integer()))
  }

  formatted_data <- mapply(function(x) {
    # Format the daily data
    daily_x <- x$data |>
      mutate(
        time = time - months(1), # We are interested in the next month values
        day = day(time),
        month = month(time),
        year = year(time)
      ) |>
      group_by(month, year)

    # Convert it to weekly data
    mapply(function(i) {
      max_day <- if (i < 4) {
        7 * i
      } else {
        31
      }

      adjusted_string <- paste(x$short_name, "Adjusted", sep = ".")
      volume_string <- paste(x$short_name, "Volume", sep = ".")
      mean_adjusted_string <- paste("mean", adjusted_string,
        "week", i, "next_month",
        sep = "_"
      )
      mean_volume_string <- paste("mean", volume_string,
        "week", i, "next_month",
        sep = "_"
      )

      weekly_x <- daily_x |>
        filter(
          day > 7 * (i - 1),
          day < max_day
        ) |>
        summarise(
          !!mean_adjusted_string := mean((!!rlang::sym(adjusted_string)),
            na.rm = TRUE
          ),
          !!mean_volume_string := mean((!!rlang::sym(volume_string)),
            na.rm = TRUE
          )
        )
    }, (1:4), SIMPLIFY = FALSE) |>
      purrr::reduce(full_join)
  }, subset_lists, SIMPLIFY = FALSE) |>
    purrr::reduce(full_join)

  return(formatted_data)
}

format_other_daily_data <- function(data) {
  # Filter the list of data frames to only keep those from the desired sources
  subset_lists <- Filter(
    function(x) x$source %in% c("ember-climate", "Destatis"), data
  )

  if (length(subset_lists) == 0) {
    return(data.frame(geo = character(), month = integer(), year = integer()))
  }


  formatted_data <- mapply(function(x) {
    # Format the daily data
    daily_x <- x$data |>
      mutate(
        day = day(time),
        month = month(time),
        year = year(time)
      ) |>
      group_by(geo, month, year)

    # Convert it to weekly data
    mapply(function(i) {
      max_day <- if (i < 4) {
        7 * i
      } else {
        31
      }

      mean_string <- paste("mean", x$short_name,
        "week", i,
        sep = "_"
      )

      weekly_x <- daily_x |>
        filter(
          day > 7 * (i - 1),
          day < max_day
        ) |>
        summarise(
          !!mean_string := mean((!!rlang::sym(x$short_name)),
            na.rm = TRUE
          )
        )
    }, (1:4), SIMPLIFY = FALSE) |>
      purrr::reduce(full_join)
  }, subset_lists, SIMPLIFY = FALSE) |>
    purrr::reduce(full_join)

  return(formatted_data)
}

format_gtrends_data <- function(data) {
  # Filter the list of data frames to only keep those from Google Trends
  subset_lists <- Filter(function(x) x$source == "gtrends", data)

  # Add columns for the previous and following months
  data_with_lead <- mapply(function(x) {
    variable_name_previous_month <- paste0(x$short_name, "_previous_month")
    variable_name_next_month <- paste0(x$short_name, "_next_month")
    x$data |>
      group_by(geo) |>
      mutate(
        time = ymd(time),
        !!variable_name_previous_month := lag(!!rlang::sym(x$short_name)),
        !!variable_name_next_month := lead(!!rlang::sym(x$short_name))
      )
  }, subset_lists, SIMPLIFY = FALSE) |>
    purrr::reduce(full_join)

  return(data_with_lead)
}

build_data_ml <- function(data, config_models, config_env, challenge, model) {
  # Format the data so that it can be used as input by ML algorithms, namely XGBoost and LSTM
  # It consists mainly in:
  # - Combining the data from all the available sources into one big dataset
  # - "Widening" the data to create lagged columns / variables, as the methods
  #   interpretate each row independently, without using the time series.
  # - Deleting the "dummy" columns that may have been created

  selected_data <- Filter(
    function(x) (challenge %in% x$challenge) & (model %in% x$model),
    data
  )

  challenge_to_predict <- paste(challenge, "to_predict", sep = "_")

  ### A) Initialize table

  # Table of countries
  countries <- selected_data[[challenge]]$data |>
    select(geo) |>
    unique() |>
    filter(geo %in% config_env[[challenge]]$countries) |>
    mutate(dummy = 1)

  # Table of dates
  dates <- selected_data[[challenge]]$data |>
    select(time) |>
    add_row(time = ymd(config_env$DATES$current_date)) |>
    unique() |>
    filter(
      year(time) >= config_models[[model]]$init_year,
      day(time) == 1
    ) |>
    mutate(dummy = 1)

  # Table of countries x dates
  df <- dates |>
    full_join(countries) |>
    select(-dummy) |>
    arrange(geo, time)

  # Create a history table of the challenge's main variable
  df_challenge <- selected_data[[challenge]]$data |>
    filter(nace_r2 == config_env[[challenge]]$principal_nace) |>
    select(-nace_r2) |>
    full_join(df) |>
    rename(!!challenge := values) |>
    group_by(geo) |>
    mutate(!!challenge_to_predict := lead(!!rlang::sym(challenge)))
  for (i in 1:(config_models[[model]][[challenge]]$nb_months_past_to_use)) {
    variable <- paste(challenge, "minus", i, "months", sep = "_")
    df_challenge <- df_challenge |>
      mutate(!!variable := lag(!!rlang::sym(challenge), n = i))
  }
  if (challenge == "TOURISM") {
    for (i in 1:(config_models[[model]][[challenge]]$nb_years_past_to_use)) {
      variable <- paste(challenge, "minus", i, "years", sep = "_")
      df_challenge <- df_challenge |>
        mutate(!!variable := lag(!!rlang::sym(challenge), n = 12 * i - 1))
    }
  }
  df_challenge <- df_challenge |>
    ungroup()

  # Merge countries x dates and our history table
  df <- df |>
    left_join(df_challenge) |>
    mutate(
      month = month(time),
      year = year(time)
    ) |>
    relocate(time, geo, !!rlang::sym(challenge_to_predict))

  ### B) Add Eurostat data

  # Remove duplicated column
  variable_with_main_nace <- paste(challenge,
    config_env[[challenge]]$principal_nace,
    sep = "_"
  )

  # Add all the other variables
  df <- df |>
    left_join(pivot_eurostat_data(selected_data),
      by = c("geo", "time")
    ) |>
    select(-!!rlang::sym(variable_with_main_nace))

  # If available, let's use the history of these other variables as well
  list_other_variables_eurostat <- colnames(df)[
    (
      7 + config_models[[model]][[challenge]]$nb_months_past_to_use + config_models[[model]][[challenge]]$nb_years_past_to_use
    ):(
      length(colnames(df)))
  ]

  for (other_variable in list_other_variables_eurostat) {
    for (i in 1:(config_models[[model]][[challenge]]$nb_months_past_to_use_others)) {
      variable <- paste(other_variable, "minus", i, "months", sep = "_")
      df <- df |>
        mutate(!!variable := lag(!!rlang::sym(other_variable), n = i))
    }
  }
  df <- df |>
    ungroup()

  ### C) Add Yahoo Finance data

  df <- df |>
    left_join(format_yahoo_data(selected_data),
      by = c("month", "year")
    )

  ### D) Add electricity data & Germany truck data

  df <- df |>
    left_join(format_other_daily_data(selected_data),
      by = c("geo", "month", "year")
    )

  ### E) Add Google Trends data

  df <- df |>
    left_join(format_gtrends_data(selected_data),
      by = c("geo", "time")
    )

  ### F) Delete dummy columns

  df <- df[colSums(!is.na(df)) > length(df) / (length(countries) + 1)]

  df <- df[c(
    rep(TRUE, 3),
    lapply(df[-(1:3)],
      var,
      na.rm = TRUE
    ) != 0
  )]

  ### G) Return results

  return(df)
}
