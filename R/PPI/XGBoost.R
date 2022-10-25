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

date_test <- "2022-01-01"

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
  mutate(dummy = 1)

df <- dates %>%
  full_join(countries) %>%
  select(-dummy) %>%
  arrange(geo, time)

df <- df %>%
  left_join(db$PPI) %>%
  select(-nace_r2) %>%
  rename(PPI = values) %>%
  mutate(month = month(time),
         year = year(time)) %>%
  group_by(geo) %>%
  mutate(PPI_previous_month = lag(PPI),
         PPI_to_predict = lead(PPI)) %>%
  ungroup()

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

df_for_regression <- as.data.table(df)

list_categorical_variables <- c('geo', 'month', 'year')

for (variable in list_categorical_variables){
  df_for_regression[[variable]] <- as.factor(df_for_regression[[variable]])
}

df_for_regression <- one_hot(df_for_regression)


#########################################
# The model
#########################################

# The variables to feed to the model

df_for_regression_train <- df_for_regression %>%
  filter(time < date_test) %>%
  drop_na(PPI_to_predict)

df_for_regression_test <- df_for_regression %>%
  filter(time >= date_test) %>%
  drop_na(PPI_to_predict)

X_train = as.matrix(df_for_regression_train %>%
                        select(-c(PPI_to_predict, time)))
y_train = df_for_regression_train$PPI_to_predict

X_test = as.matrix(df_for_regression_test %>%
                        select(-c(PPI_to_predict, time)))
y_test = df_for_regression_test$PPI_to_predict

model <- xgboost(data = X_train,
                 label = y_train,
                 nrounds = 30,
                 max_depth = 10)

preds <- predict(model, X_test)
mean_error <- mean(abs(as.matrix(y_test - preds)))
mean_error
