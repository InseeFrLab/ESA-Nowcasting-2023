###############################################################################
#                                   LSTM                                      #
###############################################################################

#########################################
# Import packages and set-up
#########################################

library(nowcastLSTM)
nowcastLSTM::initialize_session(python_path = "/opt/mamba/bin/python")
library(dplyr)
library(tidyr)
library(lubridate)
library(mltools)
library(data.table)
library(rlang)

#########################################
# Global variables
#########################################

nb_months_past_to_use <- 1 # Must be at least 1
nb_months_past_to_use_others <- 4
lags <- FALSE

#########################################
# Get data
#########################################

source("R/utils/create_table_large_PPI.R")

list_df <- create_table_large_ppi(
  nb_months_past_to_use,
  nb_months_past_to_use_others
)
countries <- list_df$countries
df_large <- list_df$df_large

#########################################
# Countries predictions
#########################################

preds_lstm <- countries %>%
  select(geo) %>%
  rename(Country = geo) %>%
  mutate(
    Date = date_to_pred,
    value = 0
  )

resid_lstm <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("Country", "Date", "value")
colnames(res_lstm) <- x

i <- 1

for (country in countries$geo) {
  df_country <- df_large %>%
    filter(geo == country) %>%
    select(-geo) %>%
    select(-PPI_minus_1_months) %>%
    select(-PPI_to_predict) %>%
    select(-month) %>%
    select(-year) %>%
    add_row(time = date_to_pred)

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

  names(df_country)[names(df_country) == "time"] <- "date"
  df_country_scaled <- df_country %>% mutate_at(c(2:62), ~ (scale(.) %>% as.vector()))
  # rag = ragged_preds(mod, pub_lags=c(1,4), lag = -2, df_country_scaled)
  df_country_scaled <- df_country_scaled[, colSums(is.na(df_country_scaled)) < nrow(df_country_scaled)] # Remove rows with NA only

  mean_ppi <- mean(df_country$PPI, na.rm = TRUE)
  std_ppi <- sd(df_country$PPI, na.rm = TRUE)

  # Train-pred split

  df_country_train <- df_country_scaled %>%
    filter(date < current_date)

  df_country_pred <- df_country_scaled %>%
    filter(date == current_date)

  # hyper param selection & fit model

  # hyperparam = nowcastLSTM::hyperparameter_tuning(data = train_scaled,  target_variable = "PVI", n_timesteps_grid=c(10,24))
  # best_param = hyperparam$hyper_params
  # which.min(best_param$performance)

  # best_param$n_models

  mod <- LSTM(
    data = df_country_train,
    target_variable = "PPI",
    n_timesteps = 12,
    fill_na_func = "median",
    # fill_ragged_edges_func = "median",
    n_models = 15,
    batch_size = 30,
    train_episodes = 50,
    python_model_name = "model"
  ) # default parameters with 12 timestep history


  # Make predictions
  predictions <- predict(mod, df_country, only_actuals_obs = F)

  df_pred_next_month <- predictions %>%
    filter(date == date_to_pred)
  pred_next_month <- df_pred_next_month$predictions * std_ppi + mean_ppi

  # If "value" is the 3rd column
  preds_lstm[i, 3] <- round(as.numeric(pred_next_month), 1)

  # Make predictions on training set for residuals

  y_pred_residuals_country <- predictions %>%
    mutate(predictions = predictions * std_ppi + mean_ppi) %>%
    transmute(
      Country = country,
      Date = date,
      value = predictions - actuals
    )

  resid_lstm <- resid_lstm %>%
    rbind(y_pred_residuals_country)

  print(i)
  i <- i + 1
}
