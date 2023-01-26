###############################################################################
#                           XGBoost model                                     #
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

source("R/utils/create_table_large_PPI.R")

#########################################
# Global variables
#########################################

do_grid_search <- TRUE
do_full_dataset_model <- TRUE

nb_months_past_to_use <- 24
nb_months_past_to_use_others <- 4

#########################################
# Create the large table for PPI
#########################################

list_df <- create_table_large_ppi(nb_months_past_to_use,
                                  nb_months_past_to_use_others)
countries <- list_df$countries
df_large <- list_df$df_large
df_large_for_regression <- list_df$df_large_for_regression

#########################################
# Adapt the table for the regressions
#########################################

df_for_regression <- as.data.table(df_large_for_regression) %>%
  mutate(geo = "Europe") # EXPERIMENTATION SANS LA VARIABLE PAYS

# One-hot encoding of categorical variables

list_categorical_variables <- c("geo", "month", "year")

for (variable in list_categorical_variables) {
  df_for_regression[[variable]] <- as.factor(df_for_regression[[variable]])
}
df_for_regression <- one_hot(df_for_regression)

# Scale the variables

df_for_regression <- df_for_regression %>%
  mutate(across(c(where(is.numeric)), scale))

mean_ppi_to_predict <- attr(df_for_regression$PPI_to_predict, "scaled:center")
scale_ppi_to_predict <- attr(df_for_regression$PPI_to_predict, "scaled:scale")

# Separate what we will use for training and the rows to predict

df_for_regression_to_use <- df_for_regression %>%
  filter(time < current_date) %>%
  drop_na(PPI_to_predict)

df_for_regression_to_predict <- df_for_regression %>%
  filter(time == current_date)

# Train/test split

if (do_full_dataset_model) {
  df_xgboost_train <- df_for_regression_to_use %>%
    sample_frac(3 / 4)
} else {
  df_xgboost_train <- df_for_regression_to_use %>%
    sample_frac(1)
}

df_xgboost_test <- df_for_regression_to_use %>%
  anti_join((df_xgboost_train))

# Train/valid split

if (do_full_dataset_model || do_grid_search) {
  df_xgboost_train_train <- df_xgboost_train %>%
    sample_frac(3 / 4)
} else {
  df_xgboost_train_train <- df_xgboost_train %>%
    sample_frac(1)
}

df_xgboost_train_valid <- df_xgboost_train %>%
  anti_join((df_xgboost_train_train))

X_train <- as.matrix(df_xgboost_train_train %>%
  select(-c(PPI_to_predict, time)))
y_train <- df_xgboost_train_train$PPI_to_predict

X_valid <- as.matrix(df_xgboost_train_valid %>%
  select(-c(PPI_to_predict, time)))
y_valid <- df_xgboost_train_valid$PPI_to_predict

gb_train <- xgb.DMatrix(data = X_train, label = y_train)
gb_valid <- xgb.DMatrix(data = X_valid, label = y_valid)
watchlist <- list(train = gb_train, valid = gb_valid)

#########################################
# The grid search
#########################################

if (do_grid_search) {
  # The ranges of the parameters to check

  nrounds <- 50 * (1:6)
  etas <- 0.05 * (1:6)
  max_depths <- (3:10)
  subsamples <- 0.25 * (2:4)
  colsample_bytrees <- 0.25 * (2:4)
  
  count <- 1

  ## The effective grid search

  for (nround in nrounds) {
    for (eta in etas) {
      for (max_depth in max_depths) {
        for (subsample in subsamples) {
          for (colsample_bytree in colsample_bytrees) {
            model <- xgb.train(
              data = gb_train,
              objective='reg:squarederror',
              eval_metric='rmse',
              nrounds = nround,
              eta = eta,
              max_depth = max_depth,
              subsample = subsample,
              colsample_bytree = colsample_bytree,
              watchlist = watchlist,
              early_stopping_rounds = 25,
              print_every_n = 10
            )
            if (count == 1) {
              best_params <- model$params
              best_n_rounds <- nround
              best_score <- model$best_score
            } else if (model$best_score < best_score) {
              best_params <- model$params
              best_n_rounds <- nround
              best_score <- model$best_score
            }
            count <- count + 1
            print(count)
          }
        }
      }
    }
  }

  print(best_params)
  print(best_n_rounds)
  print(best_score)
}

best_nround <- 
best_eta <- 
best_max_depth <- 
best_subsample <- 
best_colsample_bytree <- 

#########################################
# Use the best model on the whole dataset
#########################################

if (do_full_dataset_model) {
  # The model with the best parameters

  best_model <- xgb.train(
    data = gb_train,
    objective='reg:squarederror',
    eval_metric='rmse',
    nrounds = best_nround,
    eta = best_eta,
    max_depth = best_max_depth,
    subsample = best_subsample,
    colsample_bytree = best_colsample_bytree,
    watchlist = watchlist,
    early_stopping_rounds = 25
  )

  importance_matrix <- xgb.importance(colnames(X_train), model = best_model)
  importance_matrix

  # Test & performances

  X_test <- as.matrix(df_xgboost_test %>%
    select(-c(PPI_to_predict, time)))
  y_test <- df_xgboost_test$PPI_to_predict

  dtest <- xgb.DMatrix(data = X_test)
  y_pred <- stats::predict(best_model, dtest)

  test_mse <- mean(((y_pred - y_test)^2))
  test_rmse <- sqrt(test_mse)
  test_rmse

  # Predictions for next month

  X_to_pred <- as.matrix(df_for_regression_to_predict %>%
    select(-c(PPI_to_predict, time)))
  d_to_pred <- xgb.DMatrix(data = X_to_pred)

  y_pred_next_month <- stats::predict(best_model, d_to_pred)
  y_pred_next_month <- y_pred_next_month * scale_ppi_to_predict +
    mean_ppi_to_predict

  preds_xgboost_europe <- countries %>%
    select(geo) %>%
    mutate(Date = date_to_pred) %>%
    bind_cols(round(as.numeric(y_pred_next_month), 1)) %>%
    rename(
      Country = geo,
      value = ...3
    )
}

#########################################
# Make one model per country
#########################################

best_nround_per_country <- 
best_eta_per_country <- 
best_max_depth_per_country <- 
best_subsample_per_country <- 
best_colsample_bytree_per_country <- 

preds_xgboost_per_country <- countries %>%
  select(geo) %>%
  rename(Country = geo) %>%
  mutate(
    Date = date_to_pred,
    value = 0
  )

residuals_xgboost_per_country <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("geo", "time", "PPI_pred_residuals")
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

  # Scale the variables

  df_country <- df_country %>%
    mutate(month = as.character(month),
           year = as.character(year),
           across(c(where(is.numeric)), scale))

  mean_ppi_to_predict_country <- attr(df_country$PPI_to_predict, "scaled:center")
  scale_ppi_to_predict_country <- attr(df_country$PPI_to_predict, "scaled:scale")

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
    drop_na(PPI_to_predict)
  df_country_to_predict <- df_country %>%
    filter(time == current_date)

  X_train <- as.matrix(df_country_for_regression %>%
    select(-c(PPI_to_predict, time)))
  y_train <- df_country_for_regression$PPI_to_predict
  gb_train <- xgb.DMatrix(data = X_train, label = y_train)

  X_to_pred <- as.matrix(df_country_to_predict %>%
    select(-c(PPI_to_predict, time)))
  d_to_pred <- xgb.DMatrix(data = X_to_pred)

  # Compute model

  model <- xgb.train(
    data = gb_train,
    objective='reg:squarederror',
    eval_metric='rmse',
    nrounds = best_nround_per_country,
    eta = best_eta_per_country,
    max_depth = best_max_depth_per_country,
    subsample = best_subsample_per_country,
    colsample_bytree = best_colsample_bytree_per_country
  )

  # Make predictions

  y_pred_next_month <- stats::predict(model, d_to_pred)
  y_pred_next_month <- y_pred_next_month * scale_ppi_to_predict_country +
    mean_ppi_to_predict_country
  # If "value" is the 3rd column
  preds_xgboost_per_country[i, 3] <- round(as.numeric(y_pred_next_month), 1)

  # Make predictions on training set for residuals

  y_pred_residuals <- stats::predict(model, xgb.DMatrix(data = X_train))
  y_pred_residuals <- y_pred_residuals * scale_ppi_to_predict_country +
    mean_ppi_to_predict_country

  y_pred_residuals_with_index <- df_country_for_regression %>%
    select(time) %>%
    mutate(
      geo = country,
      time = time %m+% months(1)
    )
  y_pred_residuals_with_index$PPI_pred_residuals <- round(y_pred_residuals, 1)

  residuals_xgboost_per_country <- residuals_xgboost_per_country %>%
    rbind(y_pred_residuals_with_index)

  print(i)
  i <- i + 1
}

#########################################
# Finalize results
#########################################

# Predictions

if (do_full_dataset_model) {
  # If possible, compare the predictions obtained with the 2 methods

  comparison_df <- preds_xgboost_per_country %>%
    rename(value_per_country = value) %>%
    left_join(preds_xgboost_europe %>%
      rename(value_europe = value))
}

preds_xgboost <- preds_xgboost_per_country
# preds_xgboost <- preds_xgboost_europe


# Residuals

resid_xgboost <- residuals_xgboost_per_country %>%
  left_join(df_large %>%
    select(time, geo, PPI)) %>%
  mutate(value = PPI_pred_residuals - PPI) %>%
  rename(Country = geo, Date = time) %>%
  select(Country, Date, value)
