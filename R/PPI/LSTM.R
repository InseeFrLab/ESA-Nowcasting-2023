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

current_date = c('2022-10-01')
nb_months_past_to_use = 0
nb_months_past_to_use_pvi = 0
nb_months_past_to_use_ppi = 0
lags = FALSE
date_to_pred <- ymd("2022-11-01")
target_col = 'PVI'
country = 'DE'

#########################################
# Get data
#########################################

source("R/utils/create_table_large_PVI.R")

df_country = df_large %>%
  filter(geo == country) %>%
  select(-geo)%>%
  select(-PVI_minus_1_months)%>%
  select(-PVI_minus_0_months)%>%
  select(-PVI_to_predict)%>%
  select(-month)%>%
  select(-year)

#########################################
# Fit model
#########################################

end_training = "2020-09-01"
train = df_country%>%
  filter(time<=end_training)
names(train)[names(train)=="time"] <- "date"

test = df_country%>%
  filter(time>end_training)
test = test%>%
  filter(time<current_date)
names(test)[names(test)=="time"] <- "date"

### Scale the variables
train_scaled <- train%>% mutate_at(c(2:62), ~(scale(.) %>% as.vector))
test_scaled <- test%>% mutate_at(c(2:62), ~(scale(.) %>% as.vector))

### Fit model 
mod <- LSTM(data = train_scaled, 
            target_variable = "PVI", 
            n_timesteps = 12, 
            fill_na_func = "median", 
            n_models = 15, 
            train_episodes = 80, 
            python_model_name = "model") # default parameters with 12 timestep history

### Predict in sample to get RMSE measure
train_pred = predict(mod, train_scaled, only_actuals_obs = F)
test_pred = predict(mod, test_scaled, only_actuals_obs = F)
RMSE_is = mean((train_pred[,3]-train_pred[,2]))^2
RMSE_oss = mean((test_pred[,3]-test_pred[,2]))^2

#########################################
# Predictions
#########################################

names(df_country)[names(df_country)=="time"] <- "date"
df_country_scaled = df_country%>% mutate_at(c(2:62), ~(scale(.) %>% as.vector))
#rag = ragged_preds(mod, pub_lags=c(1,4), lag = -2, df_country_scaled)

pred = predict(mod, df_country_scaled, only_actuals_obs = F)
pred['level'] = pred[,3]*sd(df_country$PVI[1:61]) + mean(df_country$PVI[1:61])
pred['actual'] = df_country['PVI']

#########################################
# RMSE computation 
#########################################

