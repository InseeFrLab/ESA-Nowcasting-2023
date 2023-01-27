

env$PPI$data

build_data_regarima <- function(challenge, env, country) {
  selected_data <- Filter(function(x) (challenge %in% x$challenge) & ("REGARIMA" %in% x$model), env)
  
  code_variable_interest <- env[[challenge]]$filters[[names(env[[challenge]]$filters)[3]]][1]
  
  y <- env[[challenge]]$data %>%
    dplyr::filter((nace_r2 %in% code_variable_interest) & (geo == country))  %>%
    tsbox::ts_ts() 
  
  dy <- window(log(y) - stats::lag(log(y), -1), start = c(2010, 1))
  
  
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
  
  return(list("y" = dy,"X" = X))
  }

w<- build_data_regarima("PPI", env, "FR")      
             








