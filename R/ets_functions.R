build_data_ets <- function(challenge, env) {
  code_variable_interest <- env[[challenge]]$filters[[names(env[[challenge]]$filters)[3]]][1]

  data <- env[[challenge]]$data %>%
    dplyr::filter(nace_r2 %in% code_variable_interest) %>%
    to_tsibble()

  if (challenge == "TOURISM") {
    data <- data %>%
      tsibble::fill_gaps() %>%
      group_by(geo) %>%
      mutate(values = zoo::na.locf(values)) %>%
      ungroup() %>%
      filter((year(time) >= 2003 & !geo %in% c("MT", "FR")) |
        (year(time) >= 2010 & geo %in% c("MT", "FR")))
  }
  return(data)
}

run_ETS <- function(challenge, env, initial_year, last_year = date_to_pred) {
  models <- build_data_ets(challenge, env) %>%
    filter((year(time) >= initial_year) & (year(time) < last_year)) %>%
    fabletools::model(
      ETS = fable::ETS(values)
    )

  if (challenge == "TOURISM") {
    # Model identified without COVID
    # Could be changed for AT and HR
    models <- models %>%
      fable::refit(build_data_ets(challenge, env), reinitialise = FALSE, reestimate = FALSE)
  }

  preds_ets <- models %>%
    fabletools::forecast(h = "12 months") %>%
    filter(as.Date(time) == date_to_pred) %>%
    mutate(
      Country = geo,
      Date = as.Date(time),
      value = round(.mean, 1)
    ) %>%
    as_tibble() %>%
    select(Country, Date, value)

  resid_ets <- models %>%
    residuals() %>%
    mutate(
      Country = geo,
      Date = as.Date(time),
      value = .resid
    ) %>%
    as_tibble() %>%
    select(Country, Date, value)

  return(list(
    "preds" = preds_ets,
    "resids" = resid_ets
  ))
}
