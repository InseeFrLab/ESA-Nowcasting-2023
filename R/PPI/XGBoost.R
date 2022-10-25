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

#########################################
# Create the table for the regression
#########################################

# A) Initialize table

countries <- db$PPI %>%
  select(geo) %>% unique() %>%
  mutate(dummy = 1)

dates <- db$PPI %>%
  select(time) %>% unique() %>%
  filter(year(time) >= 2007) %>%
  mutate(dummy = 1,
         month = month(time),
         year = year(time))

df <- dates %>%
  full_join(countries) %>%
  select(-dummy) %>%
  arrange(geo, time)

df_PPI <- db$PPI %>%
  select(-nace_r2) %>%
  rename(PPI = values) %>%
  group_by(geo) %>%
  mutate(PPI_to_predict = lead(PPI))
for (i in 1:nb_months_past_to_use){
  variable <- paste("PPI", "minus", i, "months", sep = "_")
  df_PPI <- df_PPI %>%
    mutate(!!variable := lag(PPI, n = i))
}
df_PPI <- db$PPI %>%
  ungroup()

df <- df %>%
  left_join(df_PPI)

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
df <- df[colSums(!is.na(df)) > nrow(df)/2]
df <- df[c(TRUE, TRUE, lapply(df[-(1:2)], var, na.rm = TRUE) != 0)] 




# One-hot encoding of categorical variables

df_for_regression <- as.data.table(df) %>%
  select(-PPI) # Biaise les résultats en l'absence de PPI pour le dernier mois

list_categorical_variables <- c('geo', 'month', 'year')

for (variable in list_categorical_variables){
  df_for_regression[[variable]] <- as.factor(df_for_regression[[variable]])
}

df_for_regression <- one_hot(df_for_regression)

# Separate what we will use for training and the rows to predict

df_for_regression_to_use <- df_for_regression %>%
  filter(time < current_date) %>%
  drop_na(PPI_to_predict)

df_for_regression_to_predict <- df_for_regression %>%
  filter(time == current_date)

#########################################
# The model
#########################################

# Train/test split

df_xgboost_train <- df_for_regression_to_use %>%
  sample_frac(3/4)

df_xgboost_test <- df_for_regression_to_use %>%
  anti_join((df_xgboost_train))

# Grid search to find best optimal parameters

## Train / valid split

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

## The ranges of the parameters to check

nrounds = c(50) # 10 * (2:10)  # Can also be tried with x100
max_depths = (5:10)
etas = 0.05 * (1:10)
count = 1

if (FALSE){
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
        best_n_rounds = n_round
        best_score = model$best_score
      }
      else if( model$best_score < best_score){
        best_params = model$params
        best_n_rounds = n_round
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

# The model with the best parameters

best_model = xgb.train(data = gb_train,
                       max_depth = 6, 
                       eta = 0.3, 
                       nrounds = 50, 
                       watchlist = watchlist,
                       early_stopping_rounds = 20)

importance_matrix = xgb.importance(colnames(X_train), model = best_model)
importance_matrix

#########################################
# Test & performances
#########################################

X_test = as.matrix(df_xgboost_test %>%
                      select(-c(PPI_to_predict, time)))
y_test = df_xgboost_test$PPI_to_predict

dtest = xgb.DMatrix(data = X_test)
y_pred <- predict(best_model, dtest)

test_mse = mean(((y_pred - y_test)^2))
test_rmse = sqrt(test_mse)
test_rmse

#########################################
# Predictions for next month
#########################################

X_to_pred = as.matrix(df_for_regression_to_predict %>%
                        select(-c(PPI_to_predict, time)))
d_to_pred = xgb.DMatrix(data = X_to_pred)

y_pred_next_month <- predict(best_model, d_to_pred)

preds_xgboost <- countries %>%
  select(geo) %>%
  mutate(Date = ymd(date_to_predict)) %>%
  bind_cols(y_pred_next_month) %>%
  rename(Country = geo,
         value = ...3)
