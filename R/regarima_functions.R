build_data_regarima <- function(challenge, challenges_info, data, models, country) {
  selected_data <- Filter(function(x) (challenge %in% x$challenge) & ("REGARIMA" %in% x$model), data)

  code_variable_interest <- challenges_info[[challenge]]$principal_nace
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)

  y <- data[[challenge]]$data %>%
    dplyr::filter((nace_r2 %in% code_variable_interest) & (geo == country)) %>%
    tsbox::ts_ts()

  if (challenge == "TOURISM") {
    y <- replace(y, (floor(time(y)) %in% c(2020, 2021)) & is.na(y), 1) # replace NA by 1 during covid period
    y <- replace(y, (floor(time(y)) %in% c(2020, 2021)) & y == 0, 1) # replace 0 by 1 during covid period

    result_x13 <- desaiso(y, challenge, models)
    y_sa <- result_x13$final$series[, "sa"]
    # Coef saisonnier du mois à prédire
    coef_sa <- window(
      result_x13$final$forecasts[, "sa_f"] / result_x13$final$forecasts[, "y_f"],
      start = c(year(date_to_pred), month(date_to_pred)),
      end = c(year(date_to_pred), month(date_to_pred))
    )
    dy_sa <- log(y_sa) - log(stats::lag(y_sa, -1))
  }

  dy <- window(log(y) - stats::lag(log(y), -1), start = c(2010, 1))

  X <- create_regressors(challenge, challenges_info, selected_data, models, country)

  if (challenge == "PPI" | challenge == "PVI") {
    return(list("y" = dy, "X" = X, "Historical" = y))
  }
  if (challenge == "TOURISM") {
    dy_sa <- window(log(y_sa) - stats::lag(log(y_sa), -1))
    return(list("y" = dy_sa, "X" = X, "Historical" = y, "Historical_sa" = y_sa, "coef_sa" = coef_sa))
  }
}

# function for seasonal adjustement of some series in Tourism challenge
desaiso <- function(serie, challenge, models) {
  specification_sa <- do.call(RJDemetra::x13_spec, models$REGARIMA[[challenge]]$CVS)
  serie_sa <- RJDemetra::x13(window(serie, start = c(2015, 1)), specification_sa)
  return(serie_sa)
}

create_regressors <- function(challenge, challenges_info, data, models, country) {
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)

  if (challenge == "PPI") {
    brent <- reshape_daily_data(data, "Yahoo") %>%
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
      value = TRUE
    )

    if (!purrr::is_empty(dispo) & country != "HR") {
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
    # dIPT2 <- stats::lag(dIPT, -1)
    dIS <- diff(IS)
    # dIS2 <- stats::lag(dIS, -1)

    X <- ts.union(IPT, dIPT)

    # Traitements particuliers pays sur les exogènes
    if (country %in% c("DE")) {
      toll <- data$TOLL_DE$data
      # On mensualise
      # + homogénéise en ne prenant que le nombre de jours dispo le dernier mois
      last_day <- day((toll %>% dplyr::arrange(desc(time)))$time[1])
      toll_m <- toll %>%
        dplyr::filter(day(time) <= last_day) %>%
        dplyr::mutate(
          month = month(time),
          year = year(time)
        ) %>%
        dplyr::group_by(year, month) %>%
        dplyr::summarise(
          across(-c(time, geo), ~ mean(.x, na.rm = TRUE))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(time = ymd(paste(year, month, "01", sep = "-"))) %>%
        dplyr::select(-c(year, month)) %>%
        dplyr::mutate(values = toll) %>%
        dplyr::select(-toll)

      tolls <- toll_m %>%
        tidyr::drop_na() %>%
        tsbox::ts_ts() / 100
      dtolls <- diff(tolls)

      X <- ts.union(dIS, IPT, dIPT, dtolls)
    }
    if (country %in% c("AT")) {
      ind_wifo <- data$WEEKLY_INDEX_AT$data %>%
        tidyr::drop_na() %>%
        mutate(time = ymd(paste(year(time), month(time), "01"))) %>%
        group_by(time) %>%
        summarize(wifo_ind = mean(wifo_ind)) %>%
        ungroup() %>%
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
  } else if (challenge == "TOURISM") {
    X <- ts.union()

    if (!(country %in% c("EL", "DK", "LV"))) {
      gtrendh <- reshape_gtrends_data(data, country) %>%
        select(
          time,
          paste(country, "GTRENDS", "HOTELS", sep = "_")
        ) %>%
        tidyr::drop_na() %>%
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

estimate_regarima <- function(challenge, data, models, country, h) {
  parameters <- c(models$REGARIMA[[challenge]]$NO_CVS, list(fcst.horizon = h))

  #  if ((challenge == "PPI") | (challenge == "PVI")) {
  parameters <- c(parameters, list(usrdef.var = data$X))
  #  }

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

  if (challenge == "PVI") {
    # Gestion à la main des longueurs d'estimation en fonction de la qualité des variables
    if (country %in% c("DE")) {
      parameters$estimate.from <- "2015-01-01"
    }
    if (country %in% c("AT")) {
      parameters$estimate.from <- "2021-06-01"
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
    DB <- build_data_regarima(challenge, challenges_info, data, models, country)

    h <- lubridate::interval(last(index(tsbox::ts_xts(DB$y))), date_to_pred) %/% months(1)

    regarima <- estimate_regarima(challenge, DB, models, country, h)

    if (challenge == "TOURISM") {
      pred <- last(DB$Historical_sa) * prod(exp(regarima$forecast[, 1]))
      pred <- pred / DB$coef_sa

      # Traitement du cas où le Regarima générerait une valeur manquante malgré les précautions
      # if (is.na(pred)) {
      #   pred <- window(DB$Historical,start=c(year(date_to_pred)-1,month(date_to_pred)),
      #                  end=c(year(date_to_pred)-1,month(date_to_pred)))[1]
      # }
    }

    if (challenge != "TOURISM") {
      pred <- last(DB$Historical_sa) * prod(exp(regarima$forecast[, 1]))
    }

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
