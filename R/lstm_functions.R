###############################################################################
#                              LSTM model                                     #
###############################################################################

#########################################
# Import packages and set-up
#########################################

library(yaml)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(mltools)
library(rlang)
library(nowcastLSTM)
nowcastLSTM::initialize_session(python_path = "/opt/mamba/bin/python")
options(dplyr.summarise.inform = FALSE)

source("R/utils/data_retrieval.R")
source("R/build_data_ml.R")

#########################################
# Build tables
#########################################

build_data_lstm_one_country <- function(large_data = build_data_ml(),
                                        config_env = yaml::read_yaml("challenges.yaml"),
                                        country = 'FR') {
  
  df <- data.frame(large_data)
  
  #########################################
  # Filter the country
  #########################################
  
  df <- df %>%
    filter(geo == country) %>%
    select(-geo)
  
  #########################################
  # Delete variables we do not need
  #########################################
  
  # Variables absent for the last month
  df_current_date <- df %>%
    filter(time == ymd(config_env$DATES$current_date))
  df <- df[c(
    rep(TRUE, 2),
    colSums(
      !is.na(df_current_date[-(1:2)])
    ) > 0)]
  
  # Dummy variables in the country
  df <- df[c(
    rep(TRUE, 2),
    lapply(df[-(1:2)],
           var,
           na.rm = TRUE
    ) != 0
  )]
  
  # Uninteresting columns for LSTM
  challenge_to_predict <- paste(challenge, "to_predict", sep = "_")
  challenge_minus_1_month <- paste(challenge, "minus_1_months", sep = "_")
  challenge_minus_1_year <- paste(challenge, "minus_1_years", sep = "_")
  
  df <- df %>%
    select(-c(month, year,
              !!challenge_to_predict, !!challenge_minus_1_month))
  if (challenge == 'TOURISM'){
    df <- df %>% select(!!challenge_minus_1_year)
  }
  
  # Actually add one row
  
  df <- df %>% add_row(time = ymd(config_env$DATES$date_to_pred))
  
  #########################################
  # Return result
  #########################################
  
  return(df)
  
}












#########################################
# Make one model per country
#########################################

train_pred_xgboost_one_country <- function(data_xgboost = build_data_xgboost_one_country(),
                                           config_models = yaml::read_yaml("models.yaml"),
                                           config_env = yaml::read_yaml("challenges.yaml"),
                                           challenge = "PPI",
                                           country = 'FR') {
  
  df <- as.data.table(data_xgboost)
  challenge_to_predict <- paste(challenge, "to_predict", sep = "_")
  challenge_pred_residuals <- paste(challenge, "pred", "residuals", sep = "_")
  
  #########################################
  # Scale the variables
  #########################################
  
  df <- df %>%
    mutate(across(c(where(is.numeric)), scale))
  
  mean_to_predict <- attr(df[[challenge_to_predict]], "scaled:center")
  scale_to_predict <- attr(df[[challenge_to_predict]], "scaled:scale")
  
  #########################################
  # Train-pred split
  #########################################
  
  df_train <- df %>%
    filter(time < ymd(config_env$DATES$current_date)) %>%
    drop_na(!!challenge_to_predict)
  
  df_to_predict <- df %>%
    filter(time == ymd(config_env$DATES$current_date))
  
  X_train <- as.matrix(df_train %>%
                         select(-c(!!challenge_to_predict, time)))
  y_train <- df_train[[challenge_to_predict]]
  
  X_pred <- as.matrix(df_to_predict %>%
                        select(-c(!!challenge_to_predict, time)))
  
  gb_train <- xgb.DMatrix(data = X_train, label = y_train)
  d_to_pred <- xgb.DMatrix(data = X_pred)
  
  #########################################
  # Train the model
  #########################################
  
  model <- xgb.train(
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
  
  y_pred_residuals <- stats::predict(model, xgb.DMatrix(data = X_train))
  y_pred_residuals <- y_pred_residuals * scale_to_predict +
    mean_to_predict
  
  y_pred_residuals_with_index <- df_train %>%
    select(time) %>%
    mutate(
      geo = country,
      time = time %m+% months(1)
    ) %>%
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

train_pred_xgboost_per_country <- function(large_data = build_data_ml(),
                                           config_models = yaml::read_yaml("models.yaml"),
                                           config_env = yaml::read_yaml("challenges.yaml"),
                                           categorical_variables = c("month", "year"),
                                           challenge = "PPI") {
  
  challenge_to_predict <- paste(challenge, "to_predict", sep = "_")
  challenge_pred_residuals <- paste(challenge, "pred", "residuals", sep = "_")
  
  #########################################
  # Initialize tables
  #########################################
  
  countries <- large_data %>%
    select(geo) %>%
    unique()
  
  preds_xgboost_per_country <- countries %>%
    select(geo) %>%
    rename(Country = geo) %>%
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
  
  for (country in countries %>% pull()){
    
    df_country <- build_data_xgboost_one_country(large_data = large_data,
                                                 config_env = config_env,
                                                 categorical_variables = categorical_variables,
                                                 country = country)
    
    list_results_country <- train_pred_xgboost_one_country(data_xgboost=df_country,
                                                           config_models=config_models,
                                                           config_env = config_env,
                                                           challenge=challenge,
                                                           country=country)
    
    preds_xgboost_per_country[i, 3] <- round(as.numeric(
      list_results_country['pred_next_month']), 1)
    
    preds_residuals_xgboost_per_country <- preds_residuals_xgboost_per_country %>%
      rbind(data.frame(list_results_country['pred_residuals']))
    
    print(i)
    i <- i + 1
  }
  
  #########################################
  # Return results
  #########################################
  
  colnames(preds_residuals_xgboost_per_country) <- x
  
  residuals_xgboost_per_country <- preds_residuals_xgboost_per_country %>%
    left_join(large_data %>%
                select(time, geo, !!challenge)) %>%
    mutate(value := !!rlang::sym(challenge_pred_residuals) - !!rlang::sym(challenge)) %>%
    rename(Country = geo, Date = time) %>%
    select(Country, Date, value)
  
  return(list(
    "preds" = preds_xgboost_per_country,
    "resids" = residuals_xgboost_per_country
  ))
  
}

#########################################
# Final run functions
#########################################

run_xgboost_per_country <- function(data = get_data(yaml::read_yaml("data.yaml")),
                                    config_models = yaml::read_yaml("models.yaml"),
                                    config_env = yaml::read_yaml("challenges.yaml"),
                                    categorical_variables = c("month", "year"),
                                    challenge = "PPI") {
  
  large_data <- build_data_ml(data = data,
                              config_models = config_models,
                              config_env = config_env,
                              challenge = challenge,
                              model = "XGBOOST")
  
  return(train_pred_xgboost_per_country(large_data = large_data,
                                        config_models = config_models,
                                        config_env = config_env,
                                        categorical_variables = categorical_variables,
                                        challenge = challenge))
}
