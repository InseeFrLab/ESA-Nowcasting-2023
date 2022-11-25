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
library(xgboost)

#########################################
# Global variables
#########################################

current_date = c('2022-11-01')
nb_months_past_to_use <- 0
nb_months_past_to_use_pvi <- 0

#########################################
# Get data
#########################################

source("R/utils/create_table_large_PVI.R")
country = 'DE'
df_country = df_large %>%
  filter(geo == country) %>%
  select(-geo)%>%
  select(-PVI_minus_1_months)%>%
  select(-PVI_to_predict)%>%
  select(-month)%>%
  select(-year)

# df_country[is.na(df_country)] <- 0
# Arnaque à solutionner -- on remplace les Na par 0

#### Run the different models ####
date_to_pred <- ymd("2022-11-01")
current_date <- "2022-10-01"


df_current_date <- df_country %>%
  filter(time == current_date)
df_country <- df_country[c(
  rep(TRUE, 2),
  colSums(!is.na(df_current_date[-(1:2)])) > 0
)]

#range_3year <- paste(date_to_pred %m-% months(36 + 1), date_to_pred %m-% months(2), sep = "/")
#nan_cols <- as.double(which(colSums(is.na(df_country[range_3year])) > 0))

target_col = 'PVI'
end_training = "2022-10-01"
train = df_country%>%
  filter(time<=end_training)
names(train)[names(train)=="time"] <- "date"

mod <- LSTM(data = df_country, 
            target_variable = "PVI", 
            n_timesteps=12, 
            fill_na_func = "median", 
            python_model_name = "model") # default parameters with 12 timestep history

train_pred = predict(mod, train, only_actuals_obs = F)
df

nowcastLSTM::predict(model, df_country)
ragged_preds(model, pub_lags=c(1,4), lag = -2, train)


