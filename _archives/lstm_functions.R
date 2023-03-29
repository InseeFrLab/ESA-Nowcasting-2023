###############################################################################
#                              LSTM model                                     #
###############################################################################

#########################################
# Import packages and set-up
#########################################

# library(lubridate)
# library(dplyr)
# library(tidyr)
# library(data.table)

nowcastLSTM::initialize_session(python_path = Sys.which("python"))
options(dplyr.summarise.inform = FALSE)

# source("R/data_retrieval.R")
# source("R/data_preprocessing.R")

#########################################
# Build tables
#########################################

build_data_lstm_one_country <- function(large_data = build_data_ml(model = "LSTM"),
                                        config_env = yaml::read_yaml("challenges.yaml"),
                                        challenge = "PPI",
                                        country = "FR") {
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

  # Uninteresting columns for LSTM
  challenge_to_predict <- paste(challenge, "to_predict", sep = "_")
  challenge_minus_1_month <- paste(challenge, "minus_1_months", sep = "_")
  challenge_minus_1_year <- paste(challenge, "minus_1_years", sep = "_")

  df <- df %>%
    select(-c(
      month, year,
      !!challenge_to_predict, !!challenge_minus_1_month
    ))
  if (challenge == "TOURISM") {
    df <- df %>% select(-!!challenge_minus_1_year)
  }

  # Variables absent for the last month
  df_current_date <- df %>%
    filter(time == ymd(config_env$DATES$current_date))
  df <- df[c(
    rep(TRUE, 2),
    colSums(
      !is.na(df_current_date[-(1:2)])
    ) > 0
  )]

  # Dummy variables in the country
  df <- df[c(
    rep(TRUE, 2),
    lapply(df[-(1:2)],
      var,
      na.rm = TRUE
    ) != 0
  )]

  # Actually add one row
  df <- df %>%
    add_row(time = ymd(config_env$DATES$date_to_pred)) %>%
    rename(date = time)

  #########################################
  # Return result
  #########################################

  return(df)
}

#########################################
# Make one model per country
#########################################

train_pred_lstm_one_country <- function(data_lstm = build_data_lstm_one_country(),
                                        config_models = yaml::read_yaml("models.yaml"),
                                        config_env = yaml::read_yaml("challenges.yaml"),
                                        challenge = "PPI",
                                        country = "FR") {
  df <- as.data.table(data_lstm)

  #########################################
  # Scale the variables
  #########################################

  df_scaled <- df %>% mutate_at(c(2:length(colnames(df))), ~ (scale(.) %>% as.vector()))
  # rag = ragged_preds(mod, pub_lags=c(1,4), lag = -2, df_scaled)
  # df_scaled <- df_scaled[, colSums(is.na(df_scaled)) < nrow(df_scaled)] # Remove rows with NA only

  mean_challenge <- mean(df[[challenge]], na.rm = TRUE)
  std_challenge <- sd(df[[challenge]], na.rm = TRUE)

  #########################################
  # Train-pred split
  #########################################

  df_train <- df_scaled %>%
    filter(date < config_env$DATES$current_date)

  df_pred <- df_scaled %>%
    filter(date == config_env$DATES$current_date)

  #########################################
  # Hyperparameter selection
  #########################################

  # hyperparam = nowcastLSTM::hyperparameter_tuning(data = train_scaled,  target_variable = "PVI", n_timesteps_grid=c(10,24))
  # best_param = hyperparam$hyper_params
  # which.min(best_param$performance)

  #########################################
  # Train the model
  #########################################

  mod <- nowcastLSTM::LSTM(
    data = df_train,
    target_variable = challenge,
    n_timesteps = 12,
    fill_na_func = "median",
    # fill_ragged_edges_func = "median",
    n_models = 15,
    batch_size = 30,
    train_episodes = 50,
    python_model_name = "model"
  ) # default parameters with 12 timestep history

  #########################################
  # Predict
  #########################################

  predictions <- nowcastLSTM::predict(mod, df_scaled, only_actuals_obs = F)

  df_pred_next_month <- predictions %>%
    filter(date == config_env$DATES$date_to_pred)
  y_pred_next_month <- df_pred_next_month$predictions * std_challenge + mean_challenge

  # Compute residuals

  residuals <- predictions %>%
    transmute(
      Country = country,
      Date = date,
      value = (predictions - actuals) * std_challenge
    )

  #########################################
  # Return results
  #########################################

  return(list(
    "pred_next_month" = y_pred_next_month,
    "residuals" = residuals
  ))
}


train_pred_lstm_per_country <- function(large_data = build_data_ml(model = "LSTM"),
                                        config_models = yaml::read_yaml("models.yaml"),
                                        config_env = yaml::read_yaml("challenges.yaml"),
                                        challenge = "PPI") {
  #########################################
  # Initialize tables
  #########################################

  countries <- large_data %>%
    select(geo) %>%
    unique()

  preds_lstm_per_country <- countries %>%
    select(geo) %>%
    rename(Country = geo) %>%
    mutate(
      Date = ymd(config_env$DATES$date_to_pred),
      value = 0
    )

  residuals_lstm_per_country <- data.frame(matrix(ncol = 3, nrow = 0))
  x <- c("Country", "Date", "value")
  colnames(residuals_lstm_per_country) <- x

  #########################################
  # Loop models over countries
  #########################################

  i <- 1

  for (country in countries %>% pull()) {
    df_country <- build_data_lstm_one_country(
      large_data = large_data,
      config_env = config_env,
      challenge = challenge,
      country = country
    )

    list_results_country <- train_pred_lstm_one_country(
      data_lstm = df_country,
      config_models = config_models,
      config_env = config_env,
      challenge = challenge,
      country = country
    )

    preds_lstm_per_country[i, 3] <- round(as.numeric(
      list_results_country["pred_next_month"]
    ), 1)

    residuals_lstm_per_country <- residuals_lstm_per_country %>%
      rbind(data.frame(list_results_country["residuals"]))

    print(i)
    i <- i + 1
  }

  #########################################
  # Return results
  #########################################

  colnames(residuals_lstm_per_country) <- x

  return(list(
    "preds" = preds_lstm_per_country,
    "resids" = residuals_lstm_per_country
  ))
}

#########################################
# Final run functions
#########################################

run_lstm_per_country <- function(data = get_data(yaml::read_yaml("data.yaml")),
                                 config_models = yaml::read_yaml("models.yaml"),
                                 config_env = yaml::read_yaml("challenges.yaml"),
                                 challenge = "PPI") {
  large_data <- build_data_ml(
    data = data,
    config_models = config_models,
    config_env = config_env,
    challenge = challenge,
    model = "LSTM"
  )

  return(train_pred_lstm_per_country(
    large_data = large_data,
    config_models = config_models,
    config_env = config_env,
    challenge = challenge
  ))
}
