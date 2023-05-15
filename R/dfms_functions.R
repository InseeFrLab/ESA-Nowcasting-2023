#' Dynamic Factor Modeling functions
#'
#' This module provides a collection of functions for performing predictions 
#' using Dynamic Factor Models (DFMs). Dynamic Factor Models are statistical 
#' models that incorporate latent factors to forecast variables of interest. The
#' functions in this module facilitate the implementation and evaluation of 
#' DFMs for predictive modeling tasks.

build_data_dfms <- function(challenge, challenges_info, data_info, models_info, country) {
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)
  start_sample <- ymd(models_info$DFM[[challenge]]$start_sample)
  code_variable_interest <- gsub("-", ".", challenges_info[[challenge]]$principal_nace)
  var_to_predict <- paste(country, challenge, code_variable_interest, sep = "_")

  selected_data <- Filter(function(x) (challenge %in% x$challenge) & "DFM" %in% x$model, data_info)

  data_list <- list(
    reshape_eurostat_data(selected_data, country),
    reshape_daily_data(selected_data, "Yahoo"),
    reshape_gtrends_data(selected_data, country)
  )

  if (country == "DE" &
    !(purrr::is_empty(Filter(function(x) (x$source == "Destatis"), selected_data)))) {
    data_list <- c(data_list, list(reshape_daily_data(selected_data, "Destatis")))
  }

  DB <- data_list |>
    purrr::reduce(full_join, by = "time") |>
    filter(time > as.Date("2000-01-01")) |> # Max 2004-09 # 2003 ok BG
    arrange(time)

  DB <- xts(as.data.frame(DB[, -1]), order.by = as.Date(DB[, 1] |>
    pull()))

  # replacing - by . in column names to avoid conflicts
  colnames(DB) <- gsub("-", ".", colnames(DB))

  #########################################
  # Seasonality removal
  #########################################
  if (models_info$DFM[[challenge]]$SA) {
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
  collinearity_threshold <- models_info$DFM[[challenge]]$collinearity_threshold
  positions <- subset(as.data.frame(which(cor(DB_diff[range_square_mat]) > collinearity_threshold, arr.ind = TRUE)), row < col)

  # If necessary, remove collinear columns
  if (dim(positions)[1] > 0) {
    var_to_remove <- unique(sapply(positions, function(x) names(DB_diff[range_square_mat])[x], simplify = FALSE)$col)
    DB_diff <- subset(DB_diff, select = setdiff(names(DB_diff), var_to_remove))
    cat("Removing", var_to_remove, "due to collinearity.\n")
  }

  data <- list("y" = DB_diff, "Historical" = DB)
  if (models_info$DFM[[challenge]]$SA) data <- c(data, list("sa" = sa))

  return(data)
}

estimate_dfm <- function(data, country, max_factor, max_lags) {
  #########################################
  # Determination of parameters
  #########################################
  # We determine the optimal number of factor and lags based on IC
  ic <- tryCatch(
    {
      ICr(data)
    },
    error = function(e) {
      cat(paste0("Failed for country ", country, ", not enough variables available \n"))
      e
    }
  )

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
    dfm <- tryCatch(
      { # We try to simulate the model
        DFM(data,
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
    if (inherits(dfm, "error")) {
      # If it fails due to a singular or not positive definite matrix
      # we set convergence to FALSE
      converged <- FALSE
    } else {
      # Otherwise we use the output from the model to know
      converged <- dfm$converged
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

  return(dfm)
}

forecast_dfm <- function(challenge, data, model, challenges_info, models_info, country) {
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)
  code_variable_interest <- gsub("-", ".", challenges_info[[challenge]]$principal_nace)
  if (models_info$DFM[[challenge]]$SA) {
    var_to_predict <- paste(country, challenge, code_variable_interest, "SA", sep = "_")
  } else {
    var_to_predict <- paste(country, challenge, code_variable_interest, sep = "_")
  }

  latest_dates <- sapply(names(data$Historical), get_latest_dates, data = data$Historical)

  #########################################
  # Forecasting
  #########################################
  # Define the gap in month between our interest variable and the latest one
  gap <- latest_dates[[var_to_predict]] %--% max(latest_dates) %/% months(1)

  # Define last observed date of our interest variable
  last_observed <- latest_dates[[var_to_predict]]
  historical <- as.double(data$Historical[, var_to_predict][last_observed])

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

  if (models_info$DFM[[challenge]]$SA) {
    # we add the seasonal component so that we obtain the initial variable
    pred <- pred + data$sa$final$forecasts[gap, "s_f"]
  }

  return(pred)
}

run_DFMs <- function(challenge, challenges_info, data_info, models_info) {
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

  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)
  max_factor <- models_info$DFM[[challenge]]$max_factor
  max_lags <- models_info$DFM[[challenge]]$max_lags
  countries <- challenges_info[[challenge]]$countries

  for (country in countries) {
    cat(paste0("Running estimation for ", country, "\n"))

    code_variable_interest <- gsub("-", ".", challenges_info[[challenge]]$principal_nace)
    var_to_predict <- paste(country, challenge, code_variable_interest, sep = "_")
    #########################################
    # Prepare data
    #########################################
    DB <- build_data_dfms(challenge, challenges_info, data_info, models_info, country)


    dfm <- estimate_dfm(
      DB$y, country,
      max_factor,
      max_lags
    )

    pred <- forecast_dfm(
      challenge, DB,
      dfm,
      challenges_info,
      models_info,
      country
    )

    #########################################
    # Store the predictions
    #########################################
    preds_dfm <- preds_dfm |>
      add_row(Country = country, Date = date_to_pred, value = round(pred, 1))


    #########################################
    # Store the residuals
    #########################################
    if (models_info$DFM[[challenge]]$SA) {
      var_to_predict <- paste(var_to_predict, "SA", sep = "_")
      resids <- resid(dfm, orig.format = TRUE)[, var_to_predict]
      # we add the seasonal component
      resids <- resids + tsbox::ts_xts(DB$sa$final$series[, "s"])
    } else {
      resids <- resid(dfm, orig.format = TRUE)[, var_to_predict]
    }

    resid_dfm <- rbind(
      resid_dfm,
      resids |>
        as_tibble() |>
        mutate(
          Date = zoo::index(resids),
          Country = country
        ) |>
        rename(value = paste(var_to_predict)) |>
        select(Country, Date, value)
    )
  }

  #########################################
  # Arrange countries in the list
  #########################################

  # Re-arranging countries
  preds_dfm <- preds_dfm |>
    mutate(Country = factor(Country, levels = countries)) |>
    arrange(Country)

  return(list(
    "preds" = preds_dfm,
    "resids" = resid_dfm
  ))
}
