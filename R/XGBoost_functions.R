###############################################################################
#                           XGBoost model                                     #
###############################################################################

# Two methods are implemented here:
# - XGBoost for Europe, ie using a df with the data from all of the countries
# - XGBoost per country, training a separate model for each country

# Until now, we chose to keep XGBoost per country as our main model

#########################################
# Set-up
#########################################

options(dplyr.summarise.inform = FALSE)

#########################################
# Build tables
#########################################

build_data_xgboost_europe <- function(large_data = build_data_ml(model = "XGBOOST"),
                                      config_env = yaml::read_yaml("challenges.yaml"),
                                      categorical_variables = c("geo", "month", "year")) {
  df <- data.frame(large_data)

  #########################################
  # Delete variables absent for the last month in at least 2/3 of the countries
  #########################################

  df_current_date <- df |>
    filter(time == ymd(config_env$DATES$current_date))
  df <- df[c(
    rep(TRUE, 3),
    colSums(
      !is.na(df_current_date[-(1:3)])
    ) > 2 / 3 * nrow(df_current_date)
  )]

  #########################################
  # "Remove" the country variable for experimentations
  #########################################

  df <- as.data.table(df |> mutate(geo = "Europe"))

  #########################################
  # One-hot encoding of categorical variables
  #########################################

  for (variable in categorical_variables) {
    df[[variable]] <- as.factor(df[[variable]])
  }
  df <- mltools::one_hot(df)

  #########################################
  # Return result
  #########################################

  return(df)
}


build_data_xgboost_one_country <- function(large_data = build_data_ml(model = "XGBOOST"),
                                           config_env = yaml::read_yaml("challenges.yaml"),
                                           categorical_variables = c("month", "year"),
                                           country = "FR") {
  df <- data.frame(large_data)

  #########################################
  # Filter the country
  #########################################

  df <- df |>
    filter(geo == country) |>
    select(-geo)

  #########################################
  # Delete variables absent for the last month and dummy ones
  #########################################

  df_current_date <- df |>
    filter(time == ymd(config_env$DATES$current_date))
  df <- df[c(
    rep(TRUE, 2),
    colSums(
      !is.na(df_current_date[-(1:2)])
    ) > 0
  )]

  cols_with_diversity <- c(
    rep(TRUE, 2),
    lapply(df[-(1:2)],
      var,
      na.rm = TRUE
    ) != 0
  )

  # to document
  df <- df[!is.na(cols_with_diversity) & cols_with_diversity]

  #########################################
  # One-hot encoding of categorical variables
  #########################################

  df <- as.data.table(df)

  for (variable in categorical_variables) {
    df[[variable]] <- as.factor(df[[variable]])
  }
  df <- mltools::one_hot(df)

  #########################################
  # Return result
  #########################################

  return(df)
}

#########################################
# Grid search
#########################################

grid_search_xgboost <- function(data_xgboost = build_data_xgboost_europe(), # Can be filtered on one country
                                config_env = yaml::read_yaml("challenges.yaml"),
                                challenge = "PPI") {
  df <- as.data.table(data_xgboost)
  challenge_to_predict <- paste(challenge, "to_predict", sep = "_")

  #########################################
  # Scale the variables
  #########################################

  df <- df |>
    mutate(across(c(where(is.numeric)), scale))

  mean_to_predict <- attr(df[[challenge_to_predict]], "scaled:center")
  scale_to_predict <- attr(df[[challenge_to_predict]], "scaled:scale")

  #########################################
  # Train-test split
  #########################################

  df_to_use <- df |>
    filter(time < ymd(config_env$DATES$current_date)) |>
    drop_na(!!challenge_to_predict)

  df_to_predict <- df |>
    filter(time == ymd(config_env$DATES$current_date))

  df_train <- df_to_use |>
    sample_frac(4 / 5)

  df_valid <- df_to_use |>
    anti_join((df_train))

  X_train <- as.matrix(df_train |>
    select(-c(!!challenge_to_predict, time)))
  y_train <- df_train[[challenge_to_predict]]

  X_valid <- as.matrix(df_valid |>
    select(-c(!!challenge_to_predict, time)))
  y_valid <- df_valid[[challenge_to_predict]]

  gb_train <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  gb_valid <- xgboost::xgb.DMatrix(data = X_valid, label = y_valid)
  watchlist <- list(train = gb_train, valid = gb_valid)

  #########################################
  # The grid search
  #########################################

  nrounds <- 50 * (1:6)
  etas <- 0.05 * (1:6)
  max_depths <- (3:10)
  subsamples <- 0.25 * (2:4)
  colsample_bytrees <- 0.25 * (2:4)

  count <- 1

  for (nround in nrounds) {
    for (eta in etas) {
      for (max_depth in max_depths) {
        for (subsample in subsamples) {
          for (colsample_bytree in colsample_bytrees) {
            model <- xgboost::xgb.train(
              data = gb_train,
              objective = "reg:squarederror",
              eval_metric = "rmse",
              nrounds = nround,
              eta = eta,
              max_depth = max_depth,
              subsample = subsample,
              colsample_bytree = colsample_bytree,
              watchlist = watchlist,
              early_stopping_rounds = 25,
              print_every_n = 25
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

  print(paste0("Best parameters: ", best_params))
  print(paste0("Best n_rounds: ", best_n_rounds))
  print(paste0("Best score obtained: ", best_score))
}

#########################################
# Model for all of Europe
#########################################

train_pred_xgboost_europe <- function(data_xgboost = build_data_xgboost_europe(),
                                      large_data = build_data_ml(model = "XGBOOST"),
                                      config_models = yaml::read_yaml("models.yaml"),
                                      config_env = yaml::read_yaml("challenges.yaml"),
                                      challenge = "PPI") {
  df <- as.data.table(data_xgboost)
  challenge_to_predict <- paste(challenge, "to_predict", sep = "_")

  #########################################
  # Scale the variables
  #########################################

  df <- df |>
    mutate(across(c(where(is.numeric)), scale))

  mean_to_predict <- attr(df[[challenge_to_predict]], "scaled:center")
  scale_to_predict <- attr(df[[challenge_to_predict]], "scaled:scale")

  #########################################
  # Train-pred split
  #########################################

  df_train <- df |>
    filter(time < ymd(config_env$DATES$current_date)) |>
    drop_na(!!challenge_to_predict)

  df_to_predict <- df |>
    filter(time == ymd(config_env$DATES$current_date))

  X_train <- as.matrix(df_train |>
    select(-c(!!challenge_to_predict, time)))
  y_train <- df_train[[challenge_to_predict]]

  X_pred <- as.matrix(df_to_predict |>
    select(-c(!!challenge_to_predict, time)))

  gb_train <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  d_to_pred <- xgboost::xgb.DMatrix(data = X_pred)

  #########################################
  # Train the model
  #########################################

  model <- xgboost::xgb.train(
    data = gb_train,
    objective = "reg:squarederror",
    eval_metric = "rmse",
    nrounds = config_models$XGBOOST[[challenge]]$hyperparameters_europe$best_nround,
    eta = config_models$XGBOOST[[challenge]]$hyperparameters_europe$best_eta,
    max_depth = config_models$XGBOOST[[challenge]]$hyperparameters_europe$best_max_depth,
    subsample = config_models$XGBOOST[[challenge]]$hyperparameters_europe$best_subsample,
    colsample_bytree = config_models$XGBOOST[[challenge]]$hyperparameters_europe$best_colsample_bytree
  )

  # Compute features importance in training
  importance_matrix <- xgboost::xgb.importance(colnames(X_train), model = model)

  #########################################
  # Predict
  #########################################

  y_pred <- stats::predict(model, d_to_pred)
  y_pred <- y_pred * scale_to_predict + mean_to_predict

  #########################################
  # Gather predictions
  #########################################

  countries <- large_data |>
    select(geo) |>
    unique()

  preds_xgboost_europe <- countries |>
    mutate(Date = ymd(config_env$DATES$date_to_pred)) |>
    bind_cols(round(as.numeric(y_pred), 1)) |>
    rename(
      Country = geo,
      value = ...3
    )

  #########################################
  # Return results
  #########################################

  return(list(
    "preds" = preds_xgboost_europe,
    "importance_matrix" = importance_matrix
  ))
}

#########################################
# Make one model per country
#########################################

train_pred_xgboost_one_country <- function(data_xgboost = build_data_xgboost_one_country(),
                                           config_models = yaml::read_yaml("models.yaml"),
                                           config_env = yaml::read_yaml("challenges.yaml"),
                                           challenge = "PPI",
                                           country = "FR") {
  df <- as.data.table(data_xgboost)
  challenge_to_predict <- paste(challenge, "to_predict", sep = "_")
  challenge_pred_residuals <- paste(challenge, "pred", "residuals", sep = "_")

  #########################################
  # Scale the variables
  #########################################

  df <- df |>
    mutate(across(c(where(is.numeric)), scale))

  mean_to_predict <- attr(df[[challenge_to_predict]], "scaled:center")
  scale_to_predict <- attr(df[[challenge_to_predict]], "scaled:scale")

  #########################################
  # Train-pred split
  #########################################

  df_train <- df |>
    filter(time < ymd(config_env$DATES$current_date)) |>
    drop_na(!!challenge_to_predict)

  df_to_predict <- df |>
    filter(time == ymd(config_env$DATES$current_date))

  X_train <- as.matrix(df_train |>
    select(-c(!!challenge_to_predict, time)))
  y_train <- df_train[[challenge_to_predict]]

  X_pred <- as.matrix(df_to_predict |>
    select(-c(!!challenge_to_predict, time)))

  gb_train <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  d_to_pred <- xgboost::xgb.DMatrix(data = X_pred)

  #########################################
  # Train the model
  #########################################

  model <- xgboost::xgb.train(
    data = gb_train,
    objective = "reg:squarederror",
    eval_metric = "rmse",
    nrounds = config_models$XGBOOST[[challenge]]$hyperparameters_per_country$best_nround,
    eta = config_models$XGBOOST[[challenge]]$hyperparameters_per_country$best_eta,
    max_depth = config_models$XGBOOST[[challenge]]$hyperparameters_per_country$best_max_depth,
    subsample = config_models$XGBOOST[[challenge]]$hyperparameters_per_country$best_subsample,
    colsample_bytree = config_models$XGBOOST[[challenge]]$hyperparameters_per_country$best_colsample_bytree
  )

  #########################################
  # Predict
  #########################################

  y_pred_next_month <- stats::predict(model, d_to_pred)
  y_pred_next_month <- y_pred_next_month * scale_to_predict + mean_to_predict

  y_pred_residuals <- stats::predict(model, xgboost::xgb.DMatrix(data = X_train))
  y_pred_residuals <- y_pred_residuals * scale_to_predict +
    mean_to_predict

  y_pred_residuals_with_index <- df_train |>
    select(time) |>
    mutate(
      geo = country,
      time = time %m+% months(1)
    ) |>
    relocate(geo, time)
  y_pred_residuals_with_index[[challenge_pred_residuals]] <- round(y_pred_residuals, 1)

  #########################################
  # Return results
  #########################################

  return(list(
    "pred_next_month" = y_pred_next_month,
    "pred_residuals" = y_pred_residuals_with_index
  ))
}

train_pred_xgboost_per_country <- function(large_data = build_data_ml(model = "XGBOOST"),
                                           config_models = yaml::read_yaml("models.yaml"),
                                           config_env = yaml::read_yaml("challenges.yaml"),
                                           categorical_variables = c("month", "year"),
                                           challenge = "PPI") {
  challenge_to_predict <- paste(challenge, "to_predict", sep = "_")
  challenge_pred_residuals <- paste(challenge, "pred", "residuals", sep = "_")

  #########################################
  # Initialize tables
  #########################################

  countries <- large_data |>
    select(geo) |>
    unique()

  preds_xgboost_per_country <- countries |>
    select(geo) |>
    rename(Country = geo) |>
    mutate(
      Date = ymd(config_env$DATES$date_to_pred),
      value = 0
    )

  preds_residuals_xgboost_per_country <- data.frame(matrix(ncol = 3, nrow = 0))
  x <- c("geo", "time", challenge_pred_residuals)
  colnames(preds_residuals_xgboost_per_country) <- x

  #########################################
  # Loop models over countries
  #########################################

  i <- 1

  for (country in countries |> pull()) {
    df_country <- build_data_xgboost_one_country(
      large_data = large_data,
      config_env = config_env,
      categorical_variables = categorical_variables,
      country = country
    )

    list_results_country <- train_pred_xgboost_one_country(
      data_xgboost = df_country,
      config_models = config_models,
      config_env = config_env,
      challenge = challenge,
      country = country
    )

    preds_xgboost_per_country[i, 3] <- round(as.numeric(
      list_results_country["pred_next_month"]
    ), 1)

    preds_residuals_xgboost_per_country <- preds_residuals_xgboost_per_country |>
      rbind(data.frame(list_results_country["pred_residuals"]))

    print(i)
    i <- i + 1
  }

  #########################################
  # Return results
  #########################################

  colnames(preds_residuals_xgboost_per_country) <- x

  residuals_xgboost_per_country <- preds_residuals_xgboost_per_country |>
    left_join(large_data |>
      select(time, geo, !!challenge)) |>
    mutate(value := !!rlang::sym(challenge_pred_residuals) - !!rlang::sym(challenge)) |>
    rename(Country = geo, Date = time) |>
    select(Country, Date, value)

  return(list(
    "preds" = preds_xgboost_per_country,
    "resids" = residuals_xgboost_per_country
  ))
}

#########################################
# Final run functions
#########################################

run_grid_search_xgboost <- function(data = get_data(yaml::read_yaml("data.yaml")),
                                    config_models = yaml::read_yaml("models.yaml"),
                                    config_env = yaml::read_yaml("challenges.yaml"),
                                    challenge = "PPI",
                                    per_country = FALSE,
                                    categorical_variables = c("geo", "month", "year"), # A changer selon per_country
                                    country = "FR") {
  large_data <- build_data_ml(
    data = data,
    config_models = config_models,
    config_env = config_env,
    challenge = challenge,
    model = "XGBOOST"
  )

  if (per_country) {
    data_xgboost <- build_data_xgboost_one_country(
      large_data = large_data,
      config_env = config_env,
      categorical_variables = categorical_variables,
      country = country
    )
  } else {
    data_xgboost <- build_data_xgboost_europe(
      large_data = large_data,
      config_env = config_env,
      categorical_variables = categorical_variables
    )
  }



  return(grid_search_xgboost(
    data_xgboost = data_xgboost,
    challenge = challenge
  ))
}

run_xgboost_europe <- function(data = get_data(yaml::read_yaml("data.yaml")),
                               config_models = yaml::read_yaml("models.yaml"),
                               config_env = yaml::read_yaml("challenges.yaml"),
                               categorical_variables = c("geo", "month", "year"),
                               challenge = "PPI") {
  large_data <- build_data_ml(
    data = data,
    config_models = config_models,
    config_env = config_env,
    challenge = challenge,
    model = "XGBOOST"
  )

  data_xgboost <- build_data_xgboost_europe(
    large_data = large_data,
    config_env = config_env,
    categorical_variables = categorical_variables
  )

  return(train_pred_xgboost_europe(
    large_data = large_data,
    config_models = config_models,
    config_env = config_env,
    categorical_variables = categorical_variables,
    challenge = challenge
  ))
}

run_xgboost_per_country <- function(data = get_data(yaml::read_yaml("data.yaml")),
                                    config_models = yaml::read_yaml("models.yaml"),
                                    config_env = yaml::read_yaml("challenges.yaml"),
                                    categorical_variables = c("month", "year"),
                                    challenge = "PPI") {
  large_data <- build_data_ml(
    data = data,
    config_models = config_models,
    config_env = config_env,
    challenge = challenge,
    model = "XGBOOST"
  )

  return(train_pred_xgboost_per_country(
    large_data = large_data,
    config_models = config_models,
    config_env = config_env,
    categorical_variables = categorical_variables,
    challenge = challenge
  ))
}
