

env$PPI$data

build_data_regarima <- function(challenge, env, country) {
  selected_data <- Filter(function(x) (challenge %in% x$challenge) & ("REGARIMA" %in% x$model), env)
  
  code_variable_interest <- env[[challenge]]$filters[[names(env[[challenge]]$filters)[3]]][1]
  
  y <- env[[challenge]]$data %>%
    dplyr::filter((nace_r2 %in% code_variable_interest) & (geo == country))  %>%
    tsbox::ts_ts() 
  
  dy <- window(log(y) - stats::lag(log(y), -1), start = c(2010, 1))
  
  X <- create_regressors(challenge, selected_data, country)
  
  return(list("y" = dy, "X" = X, "Historical" = y))
}

create_regressors <- function (challenge, selected_data, country) {
  if (challenge == "PPI") {
    
    brent <- reshape_yahoo_data(selected_data) %>%
      mutate(BRENT = BRENT.Adjusted/EUR_USD.Adjusted) %>%
      select(time, BRENT) %>%
      tsbox::ts_ts()
    
    brent_1 <- stats::lag(brent, -1)
    brent_2 <- stats::lag(brent, -2)
    
    dlbrent <- log(brent) - stats::lag(log(brent), -1)
    dlbrent_1 <- stats::lag(dlbrent, -1)
    dlbrent_2 <- stats::lag(dlbrent, -2)
    
    # Liste de variables exogènes minimum
    X <- ts.union(dlbrent, dlbrent_1, dlbrent_2)
    
    ipi <- reshape_eurostat_data(selected_data, country) %>%
      select(time, paste(country, "IPI", "CPA_B-D", sep= "_")) %>%
      tidyr::drop_na() %>%
      tsbox::ts_ts()
    
    dispo <- length(ipi) > 0
    
    if (dispo) {
      dlipi <- log(ipi) - stats::lag(log(ipi), -1)
      dlipi_1 <- stats::lag(dlipi, -1)
      dlipi_2 <- stats::lag(dlipi, -2)
      dlipi_3 <- stats::lag(dlipi, -3)
      dlipi_4 <- stats::lag(dlipi, -4)
      # différence éventuelle entre dernière date ppi et dernière date prix d'imports
      ecart_dernier_mois <- lubridate::interval(date_to_pred, last(index(tsbox::ts_xts(ipi)))
      ) %/% months(1)
      
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
    
    IS <- reshape_eurostat_data(selected_data, country) %>%
      select(time, paste(country, "PSURVEY", "BS-ICI", sep= "_")) %>%
      tidyr::drop_na() %>%
      tsbox::ts_ts() / 100
    
    IPT <- reshape_eurostat_data(selected_data, country) %>%
      select(time, paste(country, "PSURVEY", "BS-IPT", sep= "_")) %>%
      tidyr::drop_na() %>%
      tsbox::ts_ts() /100
    
    dIPT <- diff(IPT)
    dIPT2 <- stats::lag(dIPT, -1)
    dIS <- diff(IS)
    dIS2 <- stats::lag(dIS, -1)
    
    X <- ts.union(IPT, dIPT)
    
    # Traitements particuliers pays sur les exogènes
    if (country %in% c("DE")) {
      X <- ts.union(IPT, dIPT, dIS)
    }
    if (country %in% c("IT")) {
      X <- ts.union(IS, dIPT, dIPT2)
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
        IS, dIS, dIS2
      )
    }
  } else {
    X <- NULL
  }
  return(X)
}

estimate_regarima <- function(challenge, data, start_sample, h) {
  
  if (challenge == "PPI") {
    # Gestion à la main des pays avec profondeur de données limitée
    if (country %in% c("EE")) {
      start_sample <- "2012-01-01"
    }
    if (country %in% c("LT")) {
      start_sample <- "2011-01-01"
    }
    if (country %in% c("LV")) {
      start_sample <- "2015-01-01"
    }
    
    
    specification <- RJDemetra::regarima_spec_tramoseats(
      transform.function = "None",
      estimate.from = start_sample,
      estimate.to = "2021-12-01",
      outlier.to = "2021-12-01",
      automdl.enabled = TRUE,
      outlier.enabled = TRUE,
      outlier.ao = TRUE,
      outlier.tc = FALSE,
      outlier.usedefcv = FALSE,
      outlier.cv = 3.5,
      usrdef.varEnabled = TRUE,
      usrdef.var = data$X,
      fcst.horizon = h
    )
    regarima <- RJDemetra::regarima(data$y, specification)
    
    if (any(is.na(regarima$forecast))) {
      specification <- RJDemetra::regarima_spec_tramoseats(
        transform.function = "None",
        estimate.from = start_sample,
        estimate.to = "2021-12-01",
        outlier.to = "2021-12-01",
        automdl.enabled = TRUE,
        outlier.enabled = TRUE,
        outlier.ao = TRUE,
        outlier.tc = FALSE,
        outlier.usedefcv = FALSE,
        outlier.cv = 3.5,
        fcst.horizon = h
      )
      regarima <- RJDemetra::regarima(data$y, specification)
    }
    
  } else if (challenge == "PVI") {
    specification <- RJDemetra::regarima_spec_tramoseats(
      transform.function = "None",
      estimate.from = start_sample,
      automdl.enabled = TRUE,
      usrdef.outliersEnabled = TRUE,
      usrdef.outliersType = c(
        "AO", "AO", "AO", "AO", "AO",
        "AO", "AO", "AO", "AO", "AO", "AO",
        "AO", "AO", "AO", "AO", "AO"
      ),
      usrdef.outliersDate = c(
        "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
        "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
        "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01"
      ),
      outlier.enabled = TRUE,
      outlier.ao = TRUE,
      outlier.tc = FALSE,
      outlier.ls = TRUE,
      outlier.usedefcv = FALSE,
      outlier.cv = 3.5,
      usrdef.varEnabled = TRUE,
      usrdef.var = data$X,
      fcst.horizon = h
    )
    regarima <- RJDemetra::regarima(data$y, specification)
    
    if (any(is.na(regarima$forecast))) {
      specification <- RJDemetra::regarima_spec_tramoseats(
        transform.function = "None",
        estimate.from = start_sample,
        automdl.enabled = TRUE,
        usrdef.outliersEnabled = TRUE,
        usrdef.outliersType = c(
          "AO", "AO", "AO", "AO", "AO",
          "AO", "AO", "AO", "AO", "AO", "AO",
          "AO", "AO", "AO", "AO", "AO", "AO"
        ),
        usrdef.outliersDate = c(
          "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
          "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
          "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01"
        ),
        outlier.enabled = TRUE,
        outlier.ao = TRUE,
        outlier.tc = FALSE,
        outlier.ls = TRUE,
        outlier.usedefcv = FALSE,
        outlier.cv = 3.5,
        fcst.horizon = h
      )
      regarima <- RJDemetra::regarima(data$y, specification)
    } else if (challenge == "TOURISM") {
      
      specification <- RJDemetra::regarima_spec_tramoseats(
        transform.function = "Auto",
        arima.coefType = "Undefined",
        arima.p = 0,
        arima.q = 1,
        arima.bp = 0,
        arima.bq = 1,
        arima.d = 1,
        arima.bd = 1,
        estimate.from = start_sample,
        estimate.to = "2019-01-01",
        automdl.enabled = FALSE,
        usrdef.outliersEnabled = TRUE,
        usrdef.outliersType = c(
          "AO", "AO", "AO", "AO", "AO", "AO",
          "AO", "AO", "AO", "AO", "AO", "AO",
          "AO", "AO", "AO", "AO", "AO", "AO",
          "AO", "AO", "AO", "AO", "AO", "AO",
          "AO", "AO", "AO"
        ),
        usrdef.outliersDate = c(
          "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
          "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
          "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01",
          "2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01",
          "2022-01-01", "2022-02-01", "2022-03-01"
        ),
        outlier.enabled = TRUE,
        outlier.ao = TRUE,
        outlier.tc = FALSE,
        outlier.ls = TRUE,
        outlier.usedefcv = FALSE,
        outlier.cv = 3.5,
        fcst.horizon = h
      )
      regarima <- RJDemetra::regarima(data$y, specification)
      
    }
    
  }
  return(regarima)
}

run_regarima <- function(challenge, env, countries, start_sample="2010-01-01") {
  
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
  
  for (country in countries) {
    
  data <- build_data_regarima(challenge, env, country)
  
  h = lubridate::interval(last(index(tsbox::ts_xts(data$y))), date_to_pred) %/% months(1)
  
  regarima <- estimate_regarima(challenge, data, start_sample, h) 
  
  pred <- last(data$Historical) * prod(exp(regarima$forecast[,1]))
  
  preds_regarima <- preds_regarima %>%
    add_row(
      Country = country,
      Date = date_to_pred,
      value = round(as.numeric(pred), 1)
    )
  
  resid_regarima <- rbind(
    resid_regarima,
    tsbox::ts_xts(data$Historical - exp(data$y - resid(regarima)) * (stats::lag(data$Historical, -1))) %>%
      as_tibble() %>%
      mutate(
        Date = zoo::index(tsbox::ts_xts(resid(regarima))),
        Country = country
      ) %>%
      select(Country, Date, value)
  )
  }
}

q<-list(
transform.function = "None",
estimate.from = start_sample,
estimate.to = "2021-12-01",
outlier.to = "2021-12-01",
automdl.enabled = TRUE,
outlier.enabled = TRUE,
outlier.ao = TRUE,
outlier.tc = FALSE,
outlier.usedefcv = FALSE,
outlier.cv = 3.5,
usrdef.varEnabled = TRUE
)
w<- c(q, list(usrdef.var = data$X, fcst.horizon = h))

specification <- do.call(RJDemetra::regarima_spec_tramoseats, w)

regarima <- RJDemetra::regarima(data$y, specification)
