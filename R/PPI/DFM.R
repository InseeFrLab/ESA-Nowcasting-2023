###############################################################################
#                  Time series models : DFMs                                  #
###############################################################################

#########################################
# Required packages
#########################################
library(dplyr)
library(purrr)
library(dfms)
library(xts)
library(lubridate)
options(dplyr.summarise.inform = FALSE)


build_data_dfms <- function(data) {
  
  ppi <- reshape_eurostat_data(data$PPI, "PPI", country, "nace_r2")
  ppi_nace2 <- reshape_eurostat_data(data$PPI_NACE2, "PPI", country, "nace_r2")
  ipi <- reshape_eurostat_data(data$IPI, "IPI", country, "cpa2_1")
  psurvey <- reshape_eurostat_data(data$PSURVEY, "PSURVEY", country, "indic")
  pvi <- reshape_eurostat_data(data$PVI, "PVI", country)
  hicp <- reshape_eurostat_data(data$HICP, "HICP", country, "coicop")
  
  brent <- data$brent %>%
    as_tibble() %>%
    mutate(
      month = month(time),
      year = year(time)
    ) %>%
    group_by(year, month) %>%
    summarise(
      brent_adjusted = mean(brent_adjusted, na.rm = T),
      brent_volume = mean(brent_volume, na.rm = T),
    ) %>%
    ungroup() %>%
    mutate(time = ymd(paste(year, month, "01", sep = "-"))) %>%
    select(time, brent_adjusted, brent_volume)
  
  eur_usd <- data$eur_usd %>%
    as_tibble() %>%
    select(time, eur_usd_adjusted) %>%
    mutate(
      month = month(time),
      year = year(time)
    ) %>%
    group_by(year, month) %>%
    summarise(
      eur_usd_adjusted = mean(eur_usd_adjusted, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(time = ymd(paste(year, month, "01", sep = "-"))) %>%
    select(time, eur_usd_adjusted)
  
  sp500 <- data$sp500 %>%
    as_tibble() %>%
    mutate(
      month = month(time),
      year = year(time)
    ) %>%
    group_by(year, month) %>%
    summarise(
      sp500_adjusted = mean(sp500_adjusted, na.rm = T),
      sp500_volume = mean(sp500_volume, na.rm = T),
    ) %>%
    ungroup() %>%
    mutate(time = ymd(paste(year, month, "01", sep = "-"))) %>%
    select(time, sp500_adjusted, sp500_volume)
  
  eurostoxx500 <- data$eurostoxx500 %>%
    as_tibble() %>%
    select(time, eurostoxx500_adjusted) %>%
    mutate(
      month = month(time),
      year = year(time)
    ) %>%
    group_by(year, month) %>%
    summarise(
      eurostoxx500_adjusted = mean(eurostoxx500_adjusted, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(time = ymd(paste(year, month, "01", sep = "-"))) %>%
    select(time, eurostoxx500_adjusted)
  
  cac40 <- data$cac40 %>%
    as_tibble() %>%
    mutate(
      month = month(time),
      year = year(time)
    ) %>%
    group_by(year, month) %>%
    summarise(
      cac40_adjusted = mean(cac40_adjusted, na.rm = T),
      cac40_volume = mean(cac40_volume, na.rm = T),
    ) %>%
    ungroup() %>%
    mutate(time = ymd(paste(year, month, "01", sep = "-"))) %>%
    select(time, cac40_adjusted, cac40_volume)
  
  DB <- list(ppi, ppi_nace2, psurvey, hicp, brent, eur_usd, sp500, eurostoxx500, cac40) %>%
    purrr::reduce(full_join, by = "time") %>%
    filter(time > as.Date("2000-01-01")) %>% # Max 2004-09 # 2003 ok BG
    arrange(time)
  
  DB <- xts(as.data.frame(DB[, -1]), order.by = as.Date(DB[, 1] %>% pull()))
  # replacing - by . in column names to avoid conflicts
  colnames(DB) <- gsub("-", ".", colnames(DB))
  
  return(DB)
}

run_DFMs <- function(data, countries, target_var, start_sample, SA = FALSE,
                     max_lags = 4,
                     max_factor = 2,
                     collinearity_threshold = 0.9999){
  
  preds_dfm <- tibble(
    Country = character(),
    Date = as.POSIXct(NA),
    value = numeric()
  )
  
  resid_dfm <- tibble(
    Country = character(),
    Date = as.POSIXct(NA),
    value = numeric()
  )
  
  for (country in countries) {
    cat(paste0("Running estimation for ", country, "\n"))
    var_to_predict <- paste(country, target_var, sep= "_")
    
    #########################################
    # Prepare data
    #########################################
    DB <- build_data_dfms(data)
  
    latest_dates <- sapply(names(DB), get_latest_dates, data = DB)
    
    #########################################
    # Seasonality removal
    #########################################
    if (SA) {
      
    }
  
    #########################################
    # Differenciate the series
    #########################################
    DB_diff <- diff(DB)
    DB_diff <- DB_diff[paste0(start_sample, "/", date_to_pred)]
  
    #########################################
    # Dealing with multiple NaNs columns
    #########################################
    range_3year <- paste(date_to_pred %m-% months(36 + 1), date_to_pred %m-% months(2), sep = "/")
    nan_cols <- as.double(which(colSums(is.na(DB_diff[range_3year])) > 0))
  
    if (!purrr::is_empty(nan_cols)) {
      cat("Removing", names(which(colSums(is.na(DB_diff[range_3year])) > 0)), "due to missing values.\n")
      DB_diff <- DB_diff[, -nan_cols]
    }
  
    #########################################
    # Dealing with collinearity
    #########################################
    # Creating a squared matrix to check collinearity
    # Now we could use last() for the interval
    range_square_mat <- paste(date_to_pred %m-% months(dim(DB_diff)[2] + 1), date_to_pred %m-% months(2), sep = "/")
  
    # Get the positions of collinear columns
    positions <- subset(as.data.frame(which(cor(DB_diff[range_square_mat]) > collinearity_threshold, arr.ind = TRUE)), row < col)
  
    # If necessary, remove collinear columns
    if (dim(positions)[1] > 0) {
      var_to_remove <- sapply(positions, function(x) names(DB_diff[range_square_mat])[x])["col"]
      DB_diff <- DB_diff[, -as.double(positions["col"])]
      cat("Removing", var_to_remove, "due to collinearity.\n")
    }
  
    #########################################
    # Determination of parameters
    #########################################
    # We determine the optimal number of factor and lags based on IC
    ic <- tryCatch(
      {
        ICr(DB_diff)
      },
      error = function(e) {
        cat(paste0("Failed for country ", country, ", not enough variables available \n"))
        e
      }
    )
  
    if (inherits(ic, "error")) next
  

    # Take the most optimal number of factor following Bain and NG (2002)
    r <- as.double(names(sort(table(ic$r.star), decreasing = TRUE)[1]))
    if (r > max_factor) r <- max_factor
  
    # We loop until we get a number of factor that allows convergence
    while (any(is.na((vars::VARselect(ic$F_pca[, 1:r])$criteria))) & r != 1) {
      r <- r - 1
    }
  
    lag <- as.double(names(sort(table(vars::VARselect(ic$F_pca[, 1:r])$selection), decreasing = TRUE)[1]))
    if (lag > max_lags) lag <- max_lags
  
    #########################################
    # Simulation of DFM
    #########################################
    # Sometimes when the number of factors is too high the model does not converge
    # Until we do not converge we re-simulate with less factors
    converged <- F
    while (!converged & r != 0) {
      model <- tryCatch(
        { # We try to simulate the model
          DFM(DB_diff,
            r = r, p = lag,
            em.method = "auto",
            na.rm.method = "LE",
            max.missing = 0.95,
            na.impute = "median.ma.spline"
          )
        },
        error = function(e) {
          # If it fails we print a message
          cat(paste0("Failed for country ", country, "\n"))
          e
        }
      )
      if (inherits(model, "error")) {
        # If it fails due to a singular or not positive definite matrix
        # we set convergence to FALSE
        converged <- FALSE
      } else {
        # Otherwise we use the output from the model to know
        converged <- model$converged
      }
      # We reduce the number of factor, so that we can resimulate when it has failed
      if (r > 1) {
        r <- r - 1
        lag <- as.double(names(sort(table(vars::VARselect(ic$F_pca[, 1:r])$selection), decreasing = TRUE)[1]))
        if (lag > max_lags) lag <- max_lags
      } else {
        lag <- lag - 1
      }
    }
  
    #########################################
    # Forecasting
    #########################################
    historical <- as.double(DB[, var_to_predict][latest_dates[[var_to_predict]]])
    if (date_to_pred > max(latest_dates)) {
      ### Out of sample forecast
  
      # Define the appropriate forecast horizon
      h <- max(latest_dates) %--% date_to_pred %/% months(1)
  
      # Forecast the model, pay attention to the standardized option
      fc <- predict(model, h = h, standardized = F)
  
      if (max(latest_dates) == latest_dates[[var_to_predict]]) {
        pred <- historical +
          sum(fc$X_fcst[, var_to_predict])
      } else {
        # Define the gap in month between our interest variable and the latest one
        gap <- latest_dates[[var_to_predict]] %--% max(latest_dates) %/% months(1)
        model$anyNA <- FALSE
        Y_hat <- fitted(model, orig.format = TRUE)
  
        pred <- historical +
          sum(tail(Y_hat[, var_to_predict], gap)) +
          sum(fc$X_fcst[, var_to_predict])
      }
    } else {
      ### In sample forecast
      
      # https://github.com/SebKrantz/dfms/issues/45
      model$anyNA <- FALSE
      Y_hat <- fitted(model, orig.format = TRUE)
      
      # Define the gap in month between our interest variable and the latest one
      gap <- latest_dates[[var_to_predict]] %--% max(latest_dates) %/% months(1)
      
      pred <- historical +
        sum(tail(Y_hat[, var_to_predict], gap))
    }
  
    #########################################
    # Storing the predictions
    #########################################
    preds_dfm <- preds_dfm %>%
      add_row(Country = country, Date = date_to_pred, value = round(pred, 1))
  
    #########################################
    # Storing the residuals
    #########################################
    resid_dfm <- rbind(
      resid_dfm,
      resid(model, orig.format = TRUE)[, var_to_predict] %>%
        as_tibble() %>%
        mutate(
          Date = zoo::index(resid(model, orig.format = TRUE)[, var_to_predict]),
          Country = country
        ) %>%
        rename(value = paste(var_to_predict)) %>%
        select(Country, Date, value)
    )
  }
  
  #########################################
  # Add missing countries in the list
  #########################################
  missing_countries <- setdiff(countries, preds_dfm$Country)
  for (country in missing_countries) {
    preds_dfm <- preds_dfm %>%
      add_row(Country = country, Date = date_to_pred)
  }
  
  # Re-arranging countries
  preds_dfm <- preds_dfm %>%
    mutate(Country = factor(Country, levels = countries)) %>%
    arrange(Country)

  return(preds_dfm)
}

preds_dfm <-run_DFMs(data, c("FR"), "PPI_B.E36", "2005-02-01")
