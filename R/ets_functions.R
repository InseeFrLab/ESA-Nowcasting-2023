build_data_ets <- function(challenge, challenges_info, data_info) {
  code_variable_interest <-challenges_info[[challenge]]$principal_nace
  
  data <- data_info[[challenge]]$data %>%
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

estimate_ets <- function(challenge, challenges_info, data_info, initial_year, last_year) {
  ets <- build_data_ets(challenge, challenges_info, data_info) %>%
    filter((year(time) >= initial_year) & (year(time) < last_year)) %>%
    fabletools::model(
      ETS = fable::ETS(values)
    )
  
  return(ets)
}

run_ETS <- function(challenge, challenges_info, data_info, models_info) {

  parameters <- c(list(challenge = challenge, challenges_info = challenges_info, data_info = data_info), models_info$ETS[[challenge]])
  ets <- do.call(estimate_ets, parameters)

  if (challenge == "TOURISM") {
    # Model identified without COVID
    # Could be changed for AT and HR
    ets <- ets %>%
      fabletools::refit(build_data_ets(challenge, challenges_info, data_info), reinitialise = FALSE, reestimate = FALSE)
  }

  preds_ets <- ets %>%
    fabletools::forecast(h = "12 months") %>%
    filter(as.Date(time) == date_to_pred) %>%
    mutate(
      Country = geo,
      Date = as.Date(time),
      value = round(.mean, 1)
    ) %>%
    as_tibble() %>%
    select(Country, Date, value)

  resid_ets <- ets %>%
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
