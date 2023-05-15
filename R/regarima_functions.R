#########################################
# Building database for Regarima for each country
# (using parameters of the configuration files data.yaml and models.yaml)
#########################################

build_data_regarima <- function(challenge, challenges_info, data, models, country) {
  selected_data <- Filter(function(x) (challenge %in% x$challenge) & ("REGARIMA" %in% x$model), data)

  code_variable_interest <- challenges_info[[challenge]]$principal_nace
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)

  # Target variable
  y <- data[[challenge]]$data |>
    dplyr::filter((nace_r2 %in% code_variable_interest) & (geo == country)) |>
    tsbox::ts_ts()

  if (challenge == "TOURISM") {
    y <- replace(y, (floor(time(y)) %in% c(2020, 2021)) & is.na(y), 1) # replace NA by 1 during covid period
    y <- replace(y, (floor(time(y)) %in% c(2020, 2021)) & y == 0, 1) # replace 0 by 1 during covid period

    # specific treatment for tourism challenge : seasonal-adjustment of the target series
    result_x13 <- desaiso(y, challenge, models)
    y_sa <- result_x13$final$series[, "sa"]
    # Projected seasonal coefficient at the predicted date : necessary to produce the non-SA prediction
    coef_sa <- window(
      result_x13$final$forecasts[, "sa_f"] / result_x13$final$forecasts[, "y_f"],
      start = c(year(date_to_pred), month(date_to_pred)),
      end = c(year(date_to_pred), month(date_to_pred))
    )
  }

  # Selection of regressors for the model and the country
  X <- create_regressors(challenge, challenges_info, selected_data, models, country)

  # Differenciate the target series
  if ((challenge == "PPI") | (challenge == "PVI")) {
    dy <- window(log(y) - stats::lag(log(y), -1), start = c(2010, 1))
    DB <- list("y" = dy, "X" = X, "Historical" = y)
  } else {
    dy_sa <- window(log(y_sa) - stats::lag(log(y_sa), -1))
    DB <- list("y" = dy_sa, "X" = X, "Historical" = y, "Historical_sa" = y_sa, "coef_sa" = coef_sa)
  }
  return(DB)
}

#########################################
# function for seasonal adjustement of some series in Tourism challenge
# Using X13 in the RJdemetra package
#########################################

desaiso <- function(serie, challenge, models) {
  specification_sa <- do.call(RJDemetra::x13_spec, models$REGARIMA[[challenge]]$CVS)
  serie_sa <- RJDemetra::x13(window(serie, start = c(2015, 1)), specification_sa)
  return(serie_sa)
}

#########################################
# Creation of the regressors list for a given challenge x country occurrence
# Some exceptions (fine-tuning) are authorized to adapt the regressors to countries specificities
#########################################

create_regressors <- function(challenge, challenges_info, data, models, country) {
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)

  # Defining regressors for PPI challenge
  if (challenge == "PPI") {
    brent <- reshape_daily_data(data, "Yahoo") |>
      mutate(BRENT = BRENT.Adjusted / EUR_USD.Adjusted) |>
      select(time, BRENT) |>
      tsbox::ts_ts()

    brent_1 <- stats::lag(brent, -1)
    brent_2 <- stats::lag(brent, -2)

    dlbrent <- log(brent) - stats::lag(log(brent), -1)
    dlbrent_1 <- stats::lag(dlbrent, -1)
    dlbrent_2 <- stats::lag(dlbrent, -2)

    # Minimal regressors list for PPI challenge
    X <- ts.union(dlbrent, dlbrent_1, dlbrent_2)

    # Check if importation prices are available

    dispo <- grep(paste(country, "IPI", "CPA_B-D", sep = "_"),
      names(reshape_eurostat_data(data, country)),
      value = TRUE
    )

    if (!purrr::is_empty(dispo) & country != "HR") {
      ipi <- reshape_eurostat_data(data, country) |>
        select(time, paste(country, "IPI", "CPA_B-D", sep = "_")) |>
        tidyr::drop_na() |>
        tsbox::ts_ts()

      dlipi <- log(ipi) - stats::lag(log(ipi), -1)
      dlipi_1 <- stats::lag(dlipi, -1)
      dlipi_2 <- stats::lag(dlipi, -2)
      dlipi_3 <- stats::lag(dlipi, -3)
      dlipi_4 <- stats::lag(dlipi, -4)
      # identification of the last available date for importation prices
      ecart_dernier_mois <- lubridate::interval(date_to_pred, last(index(tsbox::ts_xts(ipi)))) %/% months(1)

      # Different cases depending on which is the last available date for importation prices
      if (ecart_dernier_mois == -1) {
        X <- ts.union(dlbrent, dlipi_1, dlipi_2, dlipi_3, dlipi_4)
      }
      if (ecart_dernier_mois == -2) {
        X <- ts.union(dlbrent, dlbrent_1, dlipi_2, dlipi_3, dlipi_4)
      }
      if (ecart_dernier_mois == -3) {
        X <- ts.union(dlbrent, dlbrent_1, dlbrent_2, dlipi_3, dlipi_4)
      }
    }
  }
  # Defining regressors for PVI challenge
  else if (challenge == "PVI") {
    IS <- reshape_eurostat_data(data, country) |>
      select(time, paste(country, "PSURVEY", "BS-ICI", sep = "_")) |>
      tidyr::drop_na() |>
      tsbox::ts_ts() / 100

    IPT <- reshape_eurostat_data(data, country) |>
      select(time, paste(country, "PSURVEY", "BS-IPT", sep = "_")) |>
      tidyr::drop_na() |>
      tsbox::ts_ts() / 100

    dIPT <- diff(IPT)
    # dIPT2 <- stats::lag(dIPT, -1)
    dIS <- diff(IS)
    # dIS2 <- stats::lag(dIS, -1)

    X <- ts.union(IPT, dIPT)

    # Specific treatment on countries to select the regressors for the PVI challenge
    # For DE and AT we use specific early indicators (infra-month)
    if (country %in% c("DE")) {
      toll <- data$TOLL_DE$data
      # Transform daily to monthly data
      # Only using for each month the number of days available in the last month (termporal homogeneity)
      last_day <- day((toll |> dplyr::arrange(desc(time)))$time[1])
      toll_m <- toll |>
        dplyr::filter(day(time) <= last_day) |>
        dplyr::mutate(
          month = month(time),
          year = year(time)
        ) |>
        dplyr::group_by(year, month) |>
        dplyr::summarise(
          across(-c(time, geo), ~ mean(.x, na.rm = TRUE))
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(time = ymd(paste(year, month, "01", sep = "-"))) |>
        dplyr::select(-c(year, month)) |>
        dplyr::mutate(values = toll) |>
        dplyr::select(-toll)

      tolls <- toll_m |>
        tidyr::drop_na() |>
        tsbox::ts_ts() / 100
      dtolls <- diff(tolls)

      X <- ts.union(dIS, IPT, dIPT, dtolls)
    }
    if (country %in% c("AT")) {
      ind_wifo <- data$WEEKLY_INDEX_AT$data |>
        tidyr::drop_na() |>
        mutate(time = ymd(paste(year(time), month(time), "01"))) |>
        group_by(time) |>
        summarize(wifo_ind = mean(wifo_ind)) |>
        ungroup() |>
        tsbox::ts_ts()

      dind_wifo <- ind_wifo - stats::lag(ind_wifo, -1)
      X <- ts.union(IPT, dind_wifo)
    }
    if (country %in% c("IT")) {
      X <- ts.union(IS, dIPT) # dIPT2
    }
    if (country %in% c("ES")) {
      X <- ts.union(IPT, dIS)
    }
    if (country %in% c("FR")) {
      X <- ts.union(IPT, dIPT, IS, dIS)
    }
    if (country %in% c("PL")) {
      X <- ts.union()
    }
    if (country %in% c("PT")) {
      X <- ts.union(
        IPT,
        IS, dIS
      ) # dIS2
    }

    # Defining regressors for Tourism challenge
  } else if (challenge == "TOURISM") {
    X <- ts.union()

    gtrendh <- reshape_gtrends_data(data, country)

    if (paste(country, "GTRENDS", "HOTELS", sep = "_") %in% names(gtrendh)) {
      gtrendh <- gtrendh |>
        select(time, paste(country, "GTRENDS", "HOTELS", sep = "_")) |>
        tidyr::drop_na() |>
        tsbox::ts_ts() / 100
      gtrendh_sa <- desaiso(gtrendh, challenge, models)$final$series[, "sa"]
      dgtrendh_sa <- gtrendh_sa - stats::lag(gtrendh_sa, -1)
      dgtrendh_sa_1 <- stats::lag(dgtrendh_sa, -1)

      date_deb <- ymd(models$REGARIMA$TOURISM$NO_CVS$estimate.from)

      X <- window(ts.union(dgtrendh_sa, dgtrendh_sa_1),
        start = c(year(date_deb), month(date_deb)),
        end = c(year(date_to_pred), month(date_to_pred))
      )
    }
  } else {
    X <- NULL
  }
  return(X)
}

#########################################
# Estimation of parameters
#########################################

estimate_regarima <- function(challenge, data, models, country, h) {
  parameters <- c(models$REGARIMA[[challenge]]$NO_CVS, list(fcst.horizon = h))

  parameters <- c(parameters, list(usrdef.var = data$X))

  # Fine-tunning on the length of the estimation period (especially due to contraints on availability or the quality)
  if (challenge == "PPI") {
    if (country %in% c("EE")) {
      parameters$estimate.from <- "2012-01-01"
    }
    if (country %in% c("LT")) {
      parameters$estimate.from <- "2011-01-01"
    }
    if (country %in% c("LV")) {
      parameters$estimate.from <- "2015-01-01"
    }
  }

  if (challenge == "PVI") {
    if (country %in% c("DE")) {
      parameters$estimate.from <- "2015-01-01"
    }
    if (country %in% c("AT")) {
      parameters$estimate.from <- "2021-06-01"
    }
  }

  # Call of the Regarima (tramo version) function implemented in the JDemetra package
  specification <- do.call(RJDemetra::regarima_spec_tramoseats, parameters)
  regarima <- RJDemetra::regarima(data$y, specification)

  # Exception if estimation gives NA for the prediction
  if (any(is.na(regarima$forecast))) {
    parameters$usrdef.var <- NULL
    parameters$usrdef.varEnabled <- NULL

    specification <- do.call(RJDemetra::regarima_spec_tramoseats, parameters)
    regarima <- RJDemetra::regarima(data$y, specification)
  }

  return(regarima)
}

#########################################
# Calculation of the final prediction
#########################################

run_regarima <- function(challenge, challenges_info, data, models) {
  preds_regarima <- tibble(
    Country = character(),
    Date = as.POSIXct(NA),
    value = numeric()
  )

  resid_regarima <- tibble(
    Country = character(),
    Date = as.POSIXct(NA),
    value = numeric()
  )

  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)

  for (country in challenges_info[[challenge]]$countries) {
    DB <- build_data_regarima(challenge, challenges_info, data, models, country)

    h <- lubridate::interval(last(index(tsbox::ts_xts(DB$y))), date_to_pred) %/% months(1)

    regarima <- estimate_regarima(challenge, DB, models, country, h)

    # For PPI and PVI, the prediction is derived from predictions (differenciated)
    # applied to the last observation of our interest variable
    # For tourism, a final correction is applied by dividing by implicit seasonal coefficient

    if (challenge == "TOURISM") {
      pred <- (last(DB$Historical_sa) * prod(exp(regarima$forecast[, 1]))) / DB$coef_sa
    } else {
      pred <- last(DB$Historical) * prod(exp(regarima$forecast[, 1]))
    }

    # Storing the predictions
    preds_regarima <- preds_regarima |>
      add_row(
        Country = country,
        Date = date_to_pred,
        value = round(as.numeric(pred), 1)
      )

    # Storing the residuals
    resid_regarima <- rbind(
      resid_regarima,
      tsbox::ts_xts(DB$Historical - exp(DB$y - resid(regarima)) * (stats::lag(DB$Historical, -1))) |>
        as_tibble() |>
        mutate(
          Date = zoo::index(tsbox::ts_xts(resid(regarima))),
          Country = country
        ) |>
        select(Country, Date, value)
    )
  }
  return(list(
    "preds" = preds_regarima,
    "resids" = resid_regarima
  ))
}
