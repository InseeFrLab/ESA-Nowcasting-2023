library(dplyr)
library(purrr)
library(dfms)
library(xts)
library(lubridate)
options(dplyr.summarise.inform = FALSE)

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

build_data_dfms <- function(challenge, env, country) {
  selected_data <- Filter(function(x) (challenge %in% x$challenge) & "DFM" %in% x$model, env)

  DB <- list(reshape_eurostat_data(selected_data, country), reshape_yahoo_data(selected_data)) %>%
    purrr::reduce(full_join, by = "time") %>%
    filter(time > as.Date("2000-01-01")) %>% # Max 2004-09 # 2003 ok BG
    arrange(time)

  DB <- xts(as.data.frame(DB[, -1]), order.by = as.Date(DB[, 1] %>% pull()))

  # replacing - by . in column names to avoid conflicts
  colnames(DB) <- gsub("-", ".", colnames(DB))

  return(DB)
}

run_DFMs <- function(challenge, env, countries, start_sample, SA = FALSE,
                     max_lags = 4,
                     max_factor = 2,
                     collinearity_threshold = 0.9999) {
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
    code_variable_interest <- gsub("-", ".", env[[challenge]]$filters[[names(env[[challenge]]$filters)[3]]][1])
    
    var_to_predict <- paste(country, challenge, code_variable_interest, sep = "_")

    #########################################
    # Prepare data
    #########################################
    DB <- build_data_dfms(challenge, env, country)

    latest_dates <- sapply(names(DB), get_latest_dates, data = DB)

    #########################################
    # Seasonality removal
    #########################################
    if (SA) {
      sa <- RJDemetra::x13(tsbox::ts_ts(DB[, var_to_predict]), spec = c("RSA2"))
      sa_xts <- tsbox::ts_xts(sa$final$series[, "sa"])
      names(sa_xts) <- paste0(var_to_predict, "_SA")
      DB <- merge(DB, sa_xts)
      DB <- DB[, !(names(DB) %in% var_to_predict)]
    }

    #########################################
    # Differenciate the series
    #########################################
    DB_diff <- diff(DB)
    DB_diff <- DB_diff[paste0(start_sample, "/", date_to_pred)]

    #########################################
    # Dealing with multiple NaNs columns
    #########################################
    range_3year <- paste(date_to_pred %m-% months(36 + 4), date_to_pred %m-% months(5), sep = "/")
    nan_cols <- as.double(which(colSums(is.na(DB_diff[range_3year])) > 18))

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
    # Define the gap in month between our interest variable and the latest one
    gap <- latest_dates[[var_to_predict]] %--% max(latest_dates) %/% months(1)

    # Define last observed date of our interest variable
    last_observed <- latest_dates[[var_to_predict]]

    if (SA) {
      historical <- as.double(DB[, paste0(var_to_predict, "_SA")][last_observed])
      var_to_predict <- paste0(var_to_predict, "_SA")
    } else {
      historical <- as.double(DB[, var_to_predict][last_observed])
    }

    if (date_to_pred > max(latest_dates)) {
      ### Out of sample forecast

      # Define the appropriate forecast horizon
      h <- max(latest_dates) %--% date_to_pred %/% months(1)

      # Forecast the model, pay attention to the standardized option
      fc <- predict(model, h = h, standardized = F)

      if (max(latest_dates) == last_observed) {
        pred <- historical +
          sum(fc$X_fcst[, var_to_predict])
      } else {
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

      pred <- historical +
        sum(tail(Y_hat[, var_to_predict], gap))
    }

    if (SA) {
      # we add the seasonal component so that we obtain the initial variable
      pred <- pred + sa$final$forecasts[gap, "s_f"]
    }

    #########################################
    # Storing the predictions
    #########################################
    preds_dfm <- preds_dfm %>%
      add_row(Country = country, Date = date_to_pred, value = round(pred, 1))

    #########################################
    # Storing the residuals
    #########################################
    resids <- resid(model, orig.format = TRUE)[, var_to_predict]
    if (SA) {
      # we add the seasonal component
      resids <- resids + tsbox::ts_xts(sa$final$series[, "s"])
    }

    resid_dfm <- rbind(
      resid_dfm,
      resids %>%
        as_tibble() %>%
        mutate(
          Date = zoo::index(resids),
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

  return(list("preds" = preds_dfm,
              "resids" = resid_dfm))
}
