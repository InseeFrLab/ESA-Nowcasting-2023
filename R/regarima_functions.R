build_data_regarima <- function(challenge, challenges_info, data, country) {
  selected_data <- Filter(function(x) (challenge %in% x$challenge) & ("REGARIMA" %in% x$model), data)

  code_variable_interest <-challenges_info[[challenge]]$principal_nace

  y <- data[[challenge]]$data %>%
    dplyr::filter((nace_r2 %in% code_variable_interest) & (geo == country)) %>%
    tsbox::ts_ts()

  dy <- window(log(y) - stats::lag(log(y), -1), start = c(2010, 1))

  X <- create_regressors(challenge, challenges_info, selected_data, country)

  return(list("y" = dy, "X" = X, "Historical" = y))
}

create_regressors <- function(challenge, challenges_info, data, country) {
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)
  
  if (challenge == "PPI") {
    brent <- reshape_yahoo_data(data) %>%
      mutate(BRENT = BRENT.Adjusted / EUR_USD.Adjusted) %>%
      select(time, BRENT) %>%
      tsbox::ts_ts()

    brent_1 <- stats::lag(brent, -1)
    brent_2 <- stats::lag(brent, -2)

    dlbrent <- log(brent) - stats::lag(log(brent), -1)
    dlbrent_1 <- stats::lag(dlbrent, -1)
    dlbrent_2 <- stats::lag(dlbrent, -2)

    # Liste de variables exogènes minimum
    X <- ts.union(dlbrent, dlbrent_1, dlbrent_2)

    # Check if IPI is available 
    
    dispo <- grep(paste(country, "IPI", "CPA_B-D", sep = "_"), 
      names(reshape_eurostat_data(data, country)),
      value=TRUE)
    
    if (!purrr::is_empty(dispo)) {
      ipi <- reshape_eurostat_data(data, country) %>%
        select(time, paste(country, "IPI", "CPA_B-D", sep = "_")) %>%
        tidyr::drop_na() %>%
        tsbox::ts_ts()
      
      dlipi <- log(ipi) - stats::lag(log(ipi), -1)
      dlipi_1 <- stats::lag(dlipi, -1)
      dlipi_2 <- stats::lag(dlipi, -2)
      dlipi_3 <- stats::lag(dlipi, -3)
      dlipi_4 <- stats::lag(dlipi, -4)
      # différence éventuelle entre dernière date ppi et dernière date prix d'imports
      ecart_dernier_mois <- lubridate::interval(date_to_pred, last(index(tsbox::ts_xts(ipi)))) %/% months(1)

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
  } else if (challenge == "PVI") {
    IS <- reshape_eurostat_data(data, country) %>%
      select(time, paste(country, "PSURVEY", "BS-ICI", sep = "_")) %>%
      tidyr::drop_na() %>%
      tsbox::ts_ts() / 100

    IPT <- reshape_eurostat_data(data, country) %>%
      select(time, paste(country, "PSURVEY", "BS-IPT", sep = "_")) %>%
      tidyr::drop_na() %>%
      tsbox::ts_ts() / 100

    dIPT <- diff(IPT)
    #dIPT2 <- stats::lag(dIPT, -1)
    dIS <- diff(IS)
    #dIS2 <- stats::lag(dIS, -1)

    X <- ts.union(IPT, dIPT)

    # Traitements particuliers pays sur les exogènes
    if (country %in% c("DE")) {
      X <- ts.union(IPT, dIPT, dIS)
    }
    if (country %in% c("IT")) {
      X <- ts.union(IS, dIPT)#dIPT2 
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
    if (country %in% c("AT", "PT")) {
      X <- ts.union(
        IPT,
        IS, dIS
      ) #dIS2
    }
  } else {
    X <- NULL
  }
  return(X)
}

estimate_regarima <- function(challenge, data, models, country, h) {
  parameters <- c(models$REGARIMA[[challenge]], list(fcst.horizon = h))

  if ((challenge == "PPI") | (challenge == "PVI")) {
    parameters <- c(parameters, list(usrdef.var = data$X))
  }

  if (challenge == "PPI") {
    # Gestion à la main des pays avec profondeur de données limitée
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

  specification <- do.call(RJDemetra::regarima_spec_tramoseats, parameters)
  regarima <- RJDemetra::regarima(data$y, specification)

  if (any(is.na(regarima$forecast))) {
    parameters$usrdef.var <- NULL
    parameters$usrdef.varEnabled <- NULL

    specification <- do.call(RJDemetra::regarima_spec_tramoseats, parameters)
    regarima <- RJDemetra::regarima(data$y, specification)
  }

  return(regarima)
}

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
    DB <- build_data_regarima(challenge, challenges_info, data, country)

    h <- lubridate::interval(last(index(tsbox::ts_xts(DB$y))), date_to_pred) %/% months(1)

    regarima <- estimate_regarima(challenge, DB, models, country, h)

    pred <- last(DB$Historical) * prod(exp(regarima$forecast[, 1]))

    preds_regarima <- preds_regarima %>%
      add_row(
        Country = country,
        Date = date_to_pred,
        value = round(as.numeric(pred), 1)
      )

    resid_regarima <- rbind(
      resid_regarima,
      tsbox::ts_xts(DB$Historical - exp(DB$y - resid(regarima)) * (stats::lag(DB$Historical, -1))) %>%
        as_tibble() %>%
        mutate(
          Date = zoo::index(tsbox::ts_xts(resid(regarima))),
          Country = country
        ) %>%
        select(Country, Date, value)
    )
  }
  return(list(
    "preds" = preds_regarima,
    "resids" = resid_regarima
  ))
}
