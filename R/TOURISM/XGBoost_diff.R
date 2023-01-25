###############################################################################
#            XGBoost model - Predict successive differences                   #
###############################################################################

#########################################
# Import packages and set-up
#########################################

library(dplyr)
library(tidyr)
library(lubridate)
library(mltools)
library(data.table)
library(xgboost)
options(dplyr.summarise.inform = FALSE)

source("R/utils/create_table_large_TOURISM.R")

#########################################
# Global variables
#########################################

do_grid_search <- FALSE

#########################################
# Make one model per country
#########################################

best_max_depth_per_country <- 3
best_nrounds_per_country <- 90
best_eta_per_country <- 0.3

preds_xgboost_per_country <- countries %>%
  select(geo) %>%
  rename(Country = geo) %>%
  mutate(
    Date = date_to_pred,
    value = 0
  )

residuals_xgboost_per_country <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("geo", "time", "TOURISM_diff_pred_residuals")
colnames(residuals_xgboost_per_country) <- x

i <- 1

for (country in countries$geo) {
  df_country <- df_large %>%
    filter(geo == country) %>%
    select(-geo)

  df_current_date <- df_country %>%
    filter(time == current_date)
  df_country <- df_country[c(
    rep(TRUE, 2),
    colSums(!is.na(df_current_date[-(1:2)])) > 0
  )]
  df_country <- df_country[c(
    rep(TRUE, 2),
    lapply(df_country[-(1:2)],
      var,
      na.rm = TRUE
    ) != 0
  )]

  # Passer en différences

  for (variable in colnames(df_country)) {
    if (!(variable %in% c("time", "month", "year")) &
      (is.numeric(df_country[[variable]]))) {
      df_country <- df_country %>%
        mutate(
          !!variable := UQ(rlang::sym(variable)) - lag(UQ(rlang::sym(variable)), n = 12)
        )
    }
  }

  # Scale the variables

  df_country <- df_country %>%
    mutate(across(c(where(is.numeric)), scale))

  mean_tourism_to_predict_country <- attr(df_country$TOURISM_to_predict, "scaled:center")
  scale_tourism_to_predict_country <- attr(df_country$TOURISM_to_predict, "scaled:scale")

  # One-hot encoding

  df_country <- as.data.table(df_country)

  list_categorical_variables <- c("month", "year")
  for (variable in list_categorical_variables) {
    df_country[[variable]] <- as.factor(df_country[[variable]])
  }
  df_country <- one_hot(df_country)

  # Train-pred split

  df_country_for_regression <- df_country %>%
    filter(time < current_date) %>%
    drop_na(TOURISM_to_predict)
  df_country_to_predict <- df_country %>%
    filter(time == current_date)

  X_train <- as.matrix(df_country_for_regression %>%
    select(-c(TOURISM_to_predict, time)))
  y_train <- df_country_for_regression$TOURISM_to_predict
  gb_train <- xgb.DMatrix(data = X_train, label = y_train)

  X_to_pred <- as.matrix(df_country_to_predict %>%
    select(-c(TOURISM_to_predict, time)))
  d_to_pred <- xgb.DMatrix(data = X_to_pred)

  # Compute model

  model <- xgb.train(
    data = gb_train,
    max_depth = best_max_depth_per_country,
    eta = best_eta_per_country,
    nrounds = best_nrounds_per_country
  )

  # Make predictions

  y_pred_next_month <- stats::predict(model, d_to_pred)
  y_pred_next_month <- y_pred_next_month * scale_tourism_to_predict_country +
    mean_tourism_to_predict_country
  # If "value" is the 3rd column
  preds_xgboost_per_country[i, 3] <- round(as.numeric(y_pred_next_month), 1)

  # Make predictions on training set for residuals

  y_pred_residuals <- stats::predict(model, xgb.DMatrix(data = X_train))
  y_pred_residuals <- y_pred_residuals * scale_tourism_to_predict_country +
    mean_tourism_to_predict_country

  y_pred_residuals_with_index <- df_country_for_regression %>%
    select(time) %>%
    mutate(
      geo = country,
      time = time %m+% months(1)
    )
  y_pred_residuals_with_index$TOURISM_pred_residuals <- round(y_pred_residuals, 1)

  residuals_xgboost_per_country <- residuals_xgboost_per_country %>%
    rbind(y_pred_residuals_with_index)

  print(i)
  i <- i + 1
}

#########################################
# Finalize results
#########################################

# Predictions

preds_xgboost_diff <- preds_xgboost_per_country %>%
  inner_join(df_large %>%
    filter(time == current_date) %>%
    select(geo, TOURISM_minus_11_months) %>%
    rename(Country = geo) %>%
    mutate(Date = date_to_pred)) %>%
  mutate(value = TOURISM_minus_11_months + value, .keep = "unused")


# Residuals

resid_xgboost_diff <- residuals_xgboost_per_country %>%
  left_join(df_large %>%
    select(time, geo, TOURISM, TOURISM_minus_12_months)) %>%
  mutate(value = TOURISM_pred_residuals - (TOURISM - TOURISM_minus_12_months)) %>%
  rename(Country = geo, Date = time) %>%
  select(Country, Date, value)
