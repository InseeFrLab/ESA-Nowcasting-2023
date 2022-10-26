###############################################################################
#                             Data import                                     #  
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
source("R/utils/getData.R")

#########################################
# Global variables
#########################################

current_date <- "2022-09-01"

nb_months_past_to_use = 24

list_eurostat_tables <- c('PPI_NACE2',
                          'IPI',
                          'PSURVEY')
list_yahoo_finance <- c('brent', 'eur_usd', 'sp500', 'eurostoxx500', 'cac40')

db <- getData("PPI")

do_grid_search = TRUE
do_full_dataset_model = TRUE

#########################################
# Create the tables for the regression
#########################################

# A) Initialize table

countries <- db$PPI %>%
  select(geo) %>% unique() %>%
  mutate(dummy = 1)

dates <- db$PPI %>%
  select(time) %>% unique() %>%
  filter(year(time) >= 2007) %>%
  mutate(dummy = 1)

df <- dates %>%
  full_join(countries) %>%
  select(-dummy) %>%
  arrange(geo, time)

df_PPI <- db$PPI %>%
  full_join(df) %>%
  select(-nace_r2) %>%
  rename(PPI = values) %>%
  group_by(geo) %>%
  mutate(PPI_to_predict = lead(PPI))
for (i in 1:nb_months_past_to_use){
  variable <- paste("PPI", "minus", i, "months", sep = "_")
  df_PPI <- df_PPI %>%
    mutate(!!variable := lag(PPI, n = i))
}
df_PPI <- df_PPI %>%
  ungroup()

df <- df %>%
  left_join(df_PPI) %>%
  mutate(month = month(time),
         year = year(time)) %>%
  relocate(time, geo, PPI_to_predict)

# B) Add Eurostat data

for (table in list_eurostat_tables){
  df_table <- db[[table]] %>%
    pivot_wider(id_cols = c(geo, time),
                names_from = setdiff(colnames(db[[table]]),
                                     c("geo", "time", "values")),
                values_from = values,
                names_prefix = paste0(table, "_"))
  df <- df %>%
    left_join(df_table,
              by = c('geo', 'time'))
} 

# B) Add PVI

df <- df %>%
    left_join(db$PVI %>%
                rename(PVI = values) %>%
                group_by(geo) %>%
                mutate(PVI_minus_1_month = lag(PVI)) %>%
                ungroup(),
              by = c('geo', 'time'))

# C) Add Yahoo Finance data

for (table in list_yahoo_finance){
  
  df_table <- db[[table]] %>%
    mutate(day = day(time),
           month = month(time),
           year = year(time)) %>%
    group_by(month, year)
  
    for (i in 1:4){
      
      max_day <- if (i<4) {7*i} else {31}
      adjusted_string <- paste(table, 'adjusted', sep = "_")
      volume_string <- paste(table, 'volume', sep = "_")
      mean_adjusted_string <- paste('mean', adjusted_string, 'week', i,
                                    sep = "_")
      mean_volume_string <- paste('mean', volume_string, 'week', i,
                                  sep = "_")
      
      df_table_weekly <- df_table %>%
        filter(day > 7*(i-1),
               day < max_day) %>%
        summarise(
          !!mean_adjusted_string := mean((!!rlang::sym(adjusted_string)),
                                         na.rm=TRUE),
          !!mean_volume_string := mean((!!rlang::sym(volume_string)),
                                       na.rm=TRUE)
        )
      df <- df %>%
        left_join(df_table_weekly)
    }
} 

# Delete dummy columns and sort by country and date

df_current_date <- df %>%
  filter(time == current_date)
df_for_regression <- df[c(rep(TRUE, 3),
                          colSums(!is.na(df_current_date[-(1:3)])) > 3/4 * nrow(df_current_date))]
df_for_regression <- df[c(rep(TRUE, 3),
                          lapply(df[-(1:3)], var, na.rm = TRUE) != 0)] 

df_for_regression <- as.data.table(df_for_regression)

#########################################
# Adapt the table for the regressions
#########################################

# One-hot encoding of categorical variables

list_categorical_variables <- c('geo', 'month', 'year')

for (variable in list_categorical_variables){
  df_for_regression[[variable]] <- as.factor(df_for_regression[[variable]])
}

df_for_regression <- one_hot(df_for_regression) %>%
  select(-PPI)

# Separate what we will use for training and the rows to predict

df_for_regression_to_use <- df_for_regression %>%
  filter(time < current_date) %>%
  drop_na(PPI_to_predict)

df_for_regression_to_predict <- df_for_regression %>%
  filter(time == current_date)

# Train/test split

df_xgboost_train <- df_for_regression_to_use %>%
  sample_frac(3/4)

df_xgboost_test <- df_for_regression_to_use %>%
  anti_join((df_xgboost_train))

# Train/valid split

df_xgboost_train_train <- df_xgboost_train %>%
  sample_frac(3/4)

df_xgboost_train_valid <- df_xgboost_train %>%
  anti_join((df_xgboost_train_train))

X_train = as.matrix(df_xgboost_train_train %>%
                      select(-c(PPI_to_predict, time)))
y_train = df_xgboost_train_train$PPI_to_predict

X_valid = as.matrix(df_xgboost_train_valid %>%
                      select(-c(PPI_to_predict, time)))
y_valid = df_xgboost_train_valid$PPI_to_predict

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
  etas = 0.05 * (1:10)
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
                          early_stopping_rounds = 20,
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
  
  best_params
  best_n_rounds
  best_score

}

#########################################
# Use the best model on the whole dataset
#########################################

if (do_full_dataset_model){
  
  # The model with the best parameters
  
  best_model = xgb.train(data = gb_train,
                         max_depth = 5, 
                         eta = 0.15, 
                         nrounds = 100, 
                         watchlist = watchlist,
                         early_stopping_rounds = 20)
  
  importance_matrix = xgb.importance(colnames(X_train), model = best_model)
  importance_matrix
  
  # Test & performances
  
  X_test = as.matrix(df_xgboost_test %>%
                        select(-c(PPI_to_predict, time)))
  y_test = df_xgboost_test$PPI_to_predict
  
  dtest = xgb.DMatrix(data = X_test)
  y_pred <- predict(best_model, dtest)
  
  test_mse = mean(((y_pred - y_test)^2))
  test_rmse = sqrt(test_mse)
  test_rmse
  
  # Predictions for next month
  
  X_to_pred = as.matrix(df_for_regression_to_predict %>%
                          select(-c(PPI_to_predict, time)))
  d_to_pred = xgb.DMatrix(data = X_to_pred)
  
  y_pred_next_month <- predict(best_model, d_to_pred)
  
  preds_xgboost_full_dataset <- countries %>%
    select(geo) %>%
    mutate(Date = ymd(date_to_predict)) %>%
    bind_cols(y_pred_next_month) %>%
    rename(Country = geo,
           value = ...3)

}

#########################################
# Make one model per country
#########################################

preds_xgboost_per_country <- countries %>%
  select(geo) %>%
  rename(Country = geo) %>%
  mutate(Date = ymd(date_to_predict),
         value = 0)

i <- 1

for (country in countries$geo){
  df_country <- df %>%
    filter(geo == country) %>%
    select(-geo)
  
  df_current_date <- df_country %>%
    filter(time == current_date)
  df_country <- df_country[c(rep(TRUE, 2),
                             colSums(!is.na(df_current_date[-(1:2)])) > 0)]
  df_country <- df_country[c(rep(TRUE, 2),
                             lapply(df_country[-(1:2)],
                                    var, na.rm = TRUE) != 0)] 
  
  df_country <- as.data.table(df_country)
  
  list_categorical_variables <- c('month', 'year')
  for (variable in list_categorical_variables){
    df_country[[variable]] <- as.factor(df_country[[variable]])
  }
  df_country <- one_hot(df_country)
  
  df_country_for_regression <- df_country %>%
    filter(time < current_date) %>%
    drop_na(PPI_to_predict)
  df_country_to_predict <- df_country %>%
    filter(time == current_date)
  
  X_train = as.matrix(df_country_for_regression %>%
                        select(-c(PPI_to_predict, time)))
  y_train = df_country_for_regression$PPI_to_predict
  gb_train = xgb.DMatrix(data = X_train, label = y_train)
  
  X_to_pred = as.matrix(df_country_to_predict %>%
                          select(-c(PPI_to_predict, time)))
  d_to_pred = xgb.DMatrix(data = X_to_pred)
  
  model = xgb.train(data = gb_train,
                    max_depth = 4, 
                    eta = 0.15, 
                    nrounds = 100)
  
  y_pred_next_month <- predict(model, d_to_pred)
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
    left_join(preds_xgboost_full_dataset %>%
                rename(value_full = value))
}

preds_xgboost <- preds_xgboost_full_dataset



