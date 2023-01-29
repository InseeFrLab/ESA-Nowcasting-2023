library(lubridate)

get_data_from_eurostat <- function(config) {
  subset_lists <- Filter(function(x) x$source == "Eurostat", config)

  data <- lapply(subset_lists, function(x) {
    eurostat::get_eurostat(x$id,
      select_time = "M",
      filters = x$filters,
      time_format = "date"
    ) |>
      dplyr::select(geo, names(x$filters)[3], time, values) |>
      tidyr::drop_na(values)
  })

  return(data)
}

get_data_from_yahoo <- function(config) {
  subset_lists <- Filter(function(x) x$source == "Yahoo", config)

  data <- mapply(function(x, name) {
    id_var <- gsub("^", "", x$id, fixed = TRUE)
    quantmod::getSymbols(x$id, src = "yahoo", auto.assign = FALSE) |>
      tsbox::ts_tbl() |>
      subset(id %in% paste(id_var, c("Volume", "Adjusted"), sep = ".")) |>
      tidyr::spread(id, value) |>
      dplyr::rename_at(dplyr::vars(dplyr::starts_with(id_var)), list(~ sub(id_var, name, .)))
  }, subset_lists, names(subset_lists), SIMPLIFY = FALSE)
  return(data)
}

get_data_from_ember <- function(config) {
  subset_lists <- Filter(function(x) x$source == "ember-climate", config)

  data <- lapply(subset_lists, function(x) {
    countries_codes <- readr::read_csv(x[["url-geo-code"]]) |>
      dplyr::rename(
        geo = `Alpha-2 code`,
        geo_code_3 = `Alpha-3 code`
      ) |>
      dplyr::select(Country, geo, geo_code_3)

    data <- readr::read_csv(x$url) |>
      dplyr::rename(
        time = Date,
        ELEC_PRICES = `Price (EUR/MWhe)`
      ) |>
      dplyr::inner_join(countries_codes |>
      dplyr::select(-geo_code_3)) |>
      dplyr::select(geo, time, ELEC_PRICES)
  })
  return(data)
}

get_weekend_days <- function(date_to_pred, config) {
  subset_lists <- Filter(function(x) x$source == "Week-end", config)

  data <- lapply(subset_lists, function(x) {
    dates <- seq(as.Date(x[["init_date"]]), date_to_pred + months(1), by = "month")
    nb_weekend_days <- dplyr::tibble(
      month = month(dates), year = year(dates),
      weekends = numeric(length(dates))
    )
    for (i in 1:length(dates)) {
      month_start <- as.Date(paste(
        nb_weekend_days$year[i], nb_weekend_days$month[i], 1,
        sep = "-"
      ))
      month_end <- as.Date(paste(nb_weekend_days$year[i],
        nb_weekend_days$month[i],
        days_in_month(month_start),
        sep = "-"
      ))
      nb_weekend_days$weekends[i] <- sum(
        lubridate::wday(seq(month_start, month_end, by = "day")) %in% c(7, 1)
      )
    }

    return(nb_weekend_days)
  })
  return(data)
}

get_data <- function(config) {
  eurostat <- get_data_from_eurostat(config)
  yahoo <- get_data_from_yahoo(config)
  ember <- get_data_from_ember(config)
  week_ends <- get_weekend_days(date_to_pred, config)

  list_data <- lapply(
    c(eurostat, yahoo, ember, week_ends),
    function(x) list(data = x)
  )

  return(Map(c, list_data, config))
}
