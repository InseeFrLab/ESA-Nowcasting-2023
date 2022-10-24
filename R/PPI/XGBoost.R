###############################################################################
#                             Data import                                     #  
###############################################################################

#########################################
# Import packages and set-up
#########################################

library(dplyr)
library(tidyr)
library(xgboost)

source("R/utils/globalVariables.R")
source("R/utils/getData.R")

#########################################
# Create the large table
#########################################

list_eurostat_tables <- c('PPI_NACE2',
                          'IPI',
                          'PSURVEY')
list_eurostat_tables <- c()
list_yahoo_finance <- c('brent', 'eur_usd', 'sp500', 'eurostoxx500', 'cac40')

db <- getData("PPI")

df <- db$PPI %>%
  select(-nace_r2) %>%
  rename(PPI = values) %>%
  mutate(month = month(time),
         year = year(time),
         PPI_to_predict = lead(PPI)) %>%
  drop_na(PPI_to_predict)

for (table in list_eurostat_tables){
  df <- df %>%
    left_join(db[[table]] %>%
                select(geo, time, values) %>%
                rename(!!table := values),
              by = c('geo', 'time'))
} 

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
      mean_adjusted_string <- paste('mean', adjusted_string, 'week', i, sep = "_")
      mean_volume_string <- paste('mean', volume_string, 'week', i, sep = "_")
      
      df_table_weekly <- df_table %>%
        filter(day > 7*(i-1),
               day < max_day) %>%
        summarise(
          !!mean_adjusted_string := mean((!!rlang::sym(adjusted_string))),
          !!mean_volume_string := mean((!!rlang::sym(volume_string)))
        )
      df <- df %>%
        left_join(df_table_weekly)
    }
} 

df_for_regression <- df[c(TRUE, lapply(df[-1], var, na.rm = TRUE) != 0)]

X = data.matrix(df %>%
                  select(-c(PPI_to_predict, time)))
y = df$PPI_to_predict

model <- xgboost(data = X,
                 label = y,
                 max_depth = 15, 
                 nround=25,
                 seed = 1)
