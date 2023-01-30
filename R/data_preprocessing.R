reshape_eurostat_data <- function(data, country) {
  subset_lists <- Filter(function(x) x$source == "Eurostat", data)
  
  reshaped_data <- mapply(function(x, name) {
    x$data %>%
      dplyr::mutate(var = name) %>%
      dplyr::filter(geo %in% country) %>%
      tidyr::pivot_wider(names_from = c(geo, var, names(x$filters)[3]), values_from = values)
  }, subset_lists, names(subset_lists), SIMPLIFY = FALSE) |>
    purrr::reduce(full_join, by = "time")

  return(reshaped_data)
}

reshape_yahoo_data <- function(data) {
  subset_lists <- Filter(function(x) x$source == "Yahoo", data)

  reshaped_data <- lapply(subset_lists, function(x) {
    x$data %>%
      mutate(
        month = month(time),
        year = year(time)
      ) %>%
      group_by(year, month) %>%
      summarise(
        across(-time, ~ mean(.x, na.rm = TRUE))
      ) %>%
      ungroup() %>%
      mutate(time = ymd(paste(year, month, "01", sep = "-"))) %>%
      select(-c(year, month))
  }) |>
    purrr::reduce(full_join, by = "time")

  # Removing columns full of zeros
  reshaped_data <- reshaped_data[, colSums(reshaped_data != 0, na.rm = TRUE) > 0]

  return(reshaped_data)
}
