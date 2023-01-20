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

#########################################
# Global variables
#########################################

current_date = ymd('2022-12-01')
nb_years_past_to_use = 0
nb_months_past_to_use = 0
nb_months_past_to_use_pvi = 0
nb_months_past_to_use_tourism = 0
nb_months_past_to_use_others = 4
lags = FALSE
date_to_pred <- ymd("2023-01-01")
target_col = 'TOURISM'

#########################################
# Get data
#########################################

source("R/utils/create_table_large_TOURISM.R")

#########################################
# Countries predictions 
#########################################

preds_lstm_per_country = countries %>%
  select(geo) %>%
  rename(Country = geo) %>%
  mutate(
    Date = date_to_pred,
    value = 0
  )

res_lstm_per_country = data.frame(matrix(ncol = 3, nrow = 0))
x <- c("geo", "time", "TOURISM_pred_residuals")
colnames(res_lstm_per_country) <- x

i <- 1

for (country in countries$geo) {
  df_country = df_large %>%
    filter(geo == country) %>%
    select(-geo)%>%
    select(-TOURISM_minus_1_months)%>%
    select(-TOURISM_to_predict)%>%
    select(-month)%>%
    select(-year)
  
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
  
  names(df_country)[names(df_country)=="time"] <- "date"
  df_country_scaled = df_country%>% mutate_at(c(2:62), ~(scale(.) %>% as.vector))
  #rag = ragged_preds(mod, pub_lags=c(1,4), lag = -2, df_country_scaled)
  df_country_scaled <- df_country_scaled[ , colSums(is.na(df_country_scaled)) < nrow(df_country_scaled)]  # Remove rows with NA only
  
  mean_tourism <- mean(df_country$TOURISM, na.rm = TRUE)
  std_tourism <- sd(df_country$TOURISM, na.rm = TRUE)
  
  # Train-pred split
  
  df_country_train <- df_country_scaled %>%
    filter(date < current_date) 
  
  df_country_pred <- df_country_scaled %>%
    filter(date == current_date)
  
  # hyper param selection & fit model
  
  library(rlang)
  #hyperparam = nowcastLSTM::hyperparameter_tuning(data = train_scaled,  target_variable = "PVI", n_timesteps_grid=c(10,24))
  #best_param = hyperparam$hyper_params 
  #which.min(best_param$performance)
  
  #best_param$n_models
  
  mod <- LSTM(data = df_country_train, 
              target_variable = "TOURISM", 
              n_timesteps = 12, 
              fill_na_func = "median", 
              # fill_ragged_edges_func = "median",
              n_models = 15, 
              batch_size = 30,
              train_episodes = 50, 
              python_model_name = "model") # default parameters with 12 timestep history
  
  
  # Make predictions
  predictions = predict(mod, df_country, only_actuals_obs = F)
  pred = predictions %>%
    filter(date == date_to_pred)
  pred_next_month <- pred$predictions * std_tourism + mean_tourism
  
  # If "value" is the 3rd column
  preds_lstm_per_country[i, 3] <- round(as.numeric(pred_next_month), 1)
  
  # Make predictions on training set for residuals
  
  predictions
  
  pred_residuals <- predictions$predictions * std_tourism + mean_tourism
  
  y_pred_residuals <- df_country$TOURISM - pred_residuals
  
  res_lstm_per_country <- res_lstm_per_country %>%
    rbind(y_pred_residuals)
  
  print(i)
  i <- i + 1
  
}
