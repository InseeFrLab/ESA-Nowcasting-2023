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

source("R/utils/globalVariables.R")
source("R/PVI/create_table_large.R")

#########################################
# Global variables
#########################################

do_grid_search = FALSE
do_full_dataset_model = TRUE

#########################################
# Adapt the table for the regressions
#########################################

df_for_regression <- as.data.table(df_large_for_regression)

# One-hot encoding of categorical variables

list_categorical_variables <- c('geo', 'month', 'year')

for (variable in list_categorical_variables){
  df_for_regression[[variable]] <- as.factor(df_for_regression[[variable]])
}
df_for_regression <- one_hot(df_for_regression)

# Scale the variables

df_for_regression <-df_for_regression %>%
  mutate(across(c(where(is.numeric)), scale))

mean_pvi_to_predict <- attr(df_for_regression$PVI_to_predict,"scaled:center")
scale_pvi_to_predict <- attr(df_for_regression$PVI_to_predict,"scaled:scale")

# Separate what we will use for training and the rows to predict

df_for_regression_to_use <- df_for_regression %>%
  filter(time < current_date) %>%
  drop_na(PVI_to_predict)

df_for_regression_to_predict <- df_for_regression %>%
  filter(time == current_date)

# Train/test split

if (do_full_dataset_model){
  df_xgboost_train <- df_for_regression_to_use %>%
    sample_frac(3/4)
}else{
  df_xgboost_train <- df_for_regression_to_use %>%
    sample_frac(1)
}

df_xgboost_test <- df_for_regression_to_use %>%
  anti_join((df_xgboost_train))

# Train/valid split

if (do_full_dataset_model || do_grid_search){
  df_xgboost_train_train <- df_xgboost_train %>%
    sample_frac(3/4)
}else{
  df_xgboost_train_train <- df_xgboost_train %>%
    sample_frac(1)
}

df_xgboost_train_valid <- df_xgboost_train %>%
  anti_join((df_xgboost_train_train))

X_train = as.matrix(df_xgboost_train_train %>%
                      select(-c(PVI_to_predict, time)))
y_train = df_xgboost_train_train$PVI_to_predict

X_valid = as.matrix(df_xgboost_train_valid %>%
                      select(-c(PVI_to_predict, time)))
y_valid = df_xgboost_train_valid$PVI_to_predict

gb_train = xgb.DMatrix(data = X_train, label = y_train)
gb_valid = xgb.DMatrix(data = X_valid, label = y_valid)
watchlist = list(train = gb_train, valid = gb_valid)

#########################################
# The grid search
#########################################

if (do_grid_search){
  
  # The ranges of the parameters to check
  
  nrounds = 100 # nrounds = 25 * (1:6)  # Can also be tried with x100
  max_depths = (3:9)
  etas = 0.025 * (1:20)
  count = 1
  
  ## The effective grid search
  
  for (nround in nrounds){
    for (depth in max_depths){
      for (eta in etas){
        model = xgb.train(data = gb_train,
                          max_depth = depth, 
                          eta = eta, 
                          nrounds = nround, 
                          watchlist = watchlist, 
                          early_stopping_rounds = 25,
                          print_every_n = 10)
        if(count == 1){
          best_params = model$params
          best_n_rounds = nround
          best_score = model$best_score
        }
        else if( model$best_score < best_score){
          best_params = model$params
          best_n_rounds = nround
          best_score = model$best_score
        }
        count = count + 1
        print(count)
      }
    }
  }
  
  print(best_params)
  print(best_n_rounds)
  print(best_score)
  
}

best_max_depth = 5
best_nrounds = 100
best_eta = 0.25

#########################################
# Use the best model on the whole dataset
#########################################

if (do_full_dataset_model){
  
  # The model with the best parameters
  
  best_model = xgb.train(data = gb_train,
                         max_depth = best_max_depth, 
                         eta = best_eta, 
                         nrounds = best_nrounds, 
                         watchlist = watchlist,
                         early_stopping_rounds = 25)
  
  importance_matrix = xgb.importance(colnames(X_train), model = best_model)
  importance_matrix
  
  # Test & performances
  
  X_test = as.matrix(df_xgboost_test %>%
                       select(-c(PVI_to_predict, time)))
  y_test = df_xgboost_test$PVI_to_predict
  
  dtest = xgb.DMatrix(data = X_test)
  y_pred <- predict(best_model, dtest)
  
  test_mse = mean(((y_pred - y_test)^2))
  test_rmse = sqrt(test_mse)
  test_rmse
  
  # Predictions for next month
  
  X_to_pred = as.matrix(df_for_regression_to_predict %>%
                          select(-c(PVI_to_predict, time)))
  d_to_pred = xgb.DMatrix(data = X_to_pred)
  
  y_pred_next_month <- predict(best_model, d_to_pred)
  y_pred_next_month <- y_pred_next_month * scale_pvi_to_predict +
    mean_pvi_to_predict
  
  preds_xgboost_europe <- countries %>%
    select(geo) %>%
    mutate(Date = ymd(date_to_predict)) %>%
    bind_cols(y_pred_next_month) %>%
    rename(Country = geo,
           value = ...3)
  
}

#########################################
# Make one model per country
#########################################

best_max_depth_per_country = 3
best_nrounds_per_country = 100
best_eta_per_country = 0.3

preds_xgboost_per_country <- countries %>%
  select(geo) %>%
  rename(Country = geo) %>%
  mutate(Date = ymd(date_to_predict),
         value = 0)

i <- 1

for (country in countries$geo){
  df_country <- df_large %>%
    filter(geo == country) %>%
    select(-geo)
  
  df_current_date <- df_country %>%
    filter(time == current_date)
  df_country <- df_country[c(rep(TRUE, 2),
                             colSums(!is.na(df_current_date[-(1:2)])) > 0)]
  df_country <- df_country[c(rep(TRUE, 2),
                             lapply(df_country[-(1:2)],
                                    var, na.rm = TRUE) != 0)] 
  
  # Scale the variables
  
  df_country <- df_country %>%
    mutate(across(c(where(is.numeric)), scale))
  
  mean_pvi_to_predict_country <- attr(df_country$PVI_to_predict,"scaled:center")
  scale_pvi_to_predict_country <- attr(df_country$PVI_to_predict,"scaled:scale")
  
  df_country <- as.data.table(df_country)
  
  list_categorical_variables <- c('month', 'year')
  for (variable in list_categorical_variables){
    df_country[[variable]] <- as.factor(df_country[[variable]])
  }
  df_country <- one_hot(df_country)
  
  df_country_for_regression <- df_country %>%
    filter(time < current_date) %>%
    drop_na(PVI_to_predict)
  df_country_to_predict <- df_country %>%
    filter(time == current_date)
  
  X_train = as.matrix(df_country_for_regression %>%
                        select(-c(PVI_to_predict, time)))
  y_train = df_country_for_regression$PVI_to_predict
  gb_train = xgb.DMatrix(data = X_train, label = y_train)
  
  X_to_pred = as.matrix(df_country_to_predict %>%
                          select(-c(PVI_to_predict, time)))
  d_to_pred = xgb.DMatrix(data = X_to_pred)
  
  model = xgb.train(data = gb_train,
                    max_depth = best_max_depth_per_country, 
                    eta = best_eta_per_country, 
                    nrounds = best_nrounds_per_country)
  
  y_pred_next_month <- predict(model, d_to_pred)
  y_pred_next_month <- y_pred_next_month * scale_pvi_to_predict_country +
    mean_pvi_to_predict_country
  # If "value" is the 3rd column
  preds_xgboost_per_country[i, 3] <- y_pred_next_month
  print(i)
  i <- i+1
}

#########################################
# Finalize results
#########################################

if (do_full_dataset_model){
  # If possible, compare the predictions obtained with the 2 methods
  
  comparison_df = preds_xgboost_per_country %>%
    rename(value_per_country = value) %>%
    left_join(preds_xgboost_europe %>%
                rename(value_europe = value))
}

preds_xgboost <- preds_xgboost_per_country
