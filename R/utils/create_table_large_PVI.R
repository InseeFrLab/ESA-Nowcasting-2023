###############################################################################
#                       Create large table for ML                             #
###############################################################################

# Returns two important tables:
# df is the most common one, to prepare for regressions
# df_regression

#########################################
# Import packages and set-up
#########################################

library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

source("R/utils/globalVariables.R")
source("R/utils/getData.R")

#########################################
# Global variables
#########################################

#nb_months_past_to_use <- 24
#nb_months_past_to_use_others <- 4

list_eurostat_tables <- c("PSURVEY", "IPI", "HICP")
list_yahoo_finance <- c("brent", "eur_usd", "sp500", "eurostoxx500", "cac40")

db <- getData("PVI")

#########################################
# Create the tables for the regression
#########################################

# A) Initialize table

countries <- db$PVI %>%
  select(geo) %>%
  unique() %>%
  mutate(dummy = 1)

dates <- db$PVI %>%
  select(time) %>%
  add_row(time = current_date) %>%
  add_row(time = date_to_pred) %>%
  unique() %>%
  filter(
    year(time) >= 2007,
    day(time) == 1
  ) %>%
  mutate(dummy = 1)

df <- dates %>%
  full_join(countries) %>%
  select(-dummy) %>%
  arrange(geo, time)

df_PVI <- db$PVI %>%
  full_join(df) %>%
  rename(PVI = values) %>%
  group_by(geo) %>%
  mutate(PVI_to_predict = lead(PVI))

for (i in 1:nb_months_past_to_use) {
  variable <- paste("PVI", "minus", i, "months", sep = "_")
  df_PVI <- df_PVI %>%
    mutate(!!variable := lag(PVI, n = i))
}
df_PVI <- df_PVI %>%
  ungroup()

df <- df %>%
  left_join(df_PVI) %>%
  mutate(
    month = month(time),
    year = year(time)
  ) %>%
  relocate(time, geo, PVI_to_predict)

# B) Add Eurostat data

for (table in list_eurostat_tables) {
  df_table <- db[[table]] %>%
    pivot_wider(
      id_cols = c(geo, time),
      names_from = setdiff(
        colnames(db[[table]]),
        c("geo", "time", "values")
      ),
      values_from = values,
      names_prefix = paste0(table, "_")
    )
  df <- df %>%
    left_join(df_table,
              by = c("geo", "time")
    )
}

# B) Add PPI

df <- df %>%
  left_join(
    db$PPI %>%
      select(-nace_r2) %>%
      rename(PPI = values),
    by = c("geo", "time")
  ) %>%
  group_by(geo)

# If available, let's use the last months of the other variables as well
list_other_variables <- colnames(df)[
  (7 + nb_months_past_to_use):(length(colnames(df)) - 1)
]

for (i in 1:nb_months_past_to_use_others) {
  variable <- paste("PPI", "minus", i, "months", sep = "_")
  df <- df %>%
    mutate(!!variable := lag(PPI, n = i))
  
  for (other_variable in list_other_variables) {
    variable <- paste(other_variable, "minus", i, "months", sep = "_")
    df <- df %>%
      mutate(!!variable := lag(UQ(rlang::sym(other_variable)), n = i))
  }
}
df <- df %>%
  ungroup()

# C) Add Yahoo Finance data

for (table in list_yahoo_finance) {
  df_table <- db[[table]] %>%
    mutate(
      day = day(time),
      month = month(time),
      year = year(time)
    ) %>%
    group_by(month, year)
  
  for (i in 1:4) {
    max_day <- if (i < 4) {
      7 * i
    } else {
      31
    }
    adjusted_string <- paste(table, "adjusted", sep = "_")
    volume_string <- paste(table, "volume", sep = "_")
    mean_adjusted_string <- paste("mean", adjusted_string, "week", i,
                                  sep = "_"
    )
    mean_volume_string <- paste("mean", volume_string, "week", i,
                                sep = "_"
    )
    
    df_table_weekly <- df_table %>%
      filter(
        day > 7 * (i - 1),
        day < max_day
      ) %>%
      summarise(
        !!mean_adjusted_string := mean((!!rlang::sym(adjusted_string)),
                                       na.rm = TRUE
        ),
        !!mean_volume_string := mean((!!rlang::sym(volume_string)),
                                     na.rm = TRUE
        )
      )
    
    df <- df %>%
      left_join(df_table_weekly)
  }
}

# D) Add electricity data

df_electricity <- db[['electricity_prices']] %>%
  mutate(
    day = day(time),
    month = month(time),
    year = year(time)
  ) %>%
  group_by(geo, month, year)

for (i in 1:4) {
  max_day <- if (i < 4) {
    7 * i
  } else {
    31
  }
  mean_price <- paste("mean_electricity_price_week", i,
                      sep = "_"
  )

  df_weekly <- df_electricity %>%
    filter(
      day > 7 * (i - 1),
      day < max_day
    ) %>%
    summarise(
      !!mean_price := mean(electricity_price,
                           na.rm = TRUE)
    )

  df <- df %>%
    left_join(df_weekly)
}

# E) Add number of weeekend days per month

df <- df %>%
  left_join(db[['nb_weekend_days']])

# Delete dummy columns (to do by country if models specific to countries)

df <- df[colSums(!is.na(df)) > 0]

df <- df[c(
  rep(TRUE, 3),
  lapply(df[-(1:3)],
         var,
         na.rm = TRUE
  ) != 0
)]

df_large <- data.frame(df)

# Now let's go a bit further and delete variables that we don't have for the
# last month in at least 2/3 of the countries

df_for_regression <- data.frame(df)

df_current_date <- df %>%
  filter(time == current_date)
df_for_regression <- df_for_regression[c(
  rep(TRUE, 3),
  colSums(
    !is.na(df_current_date[-(1:3)])
  ) > 2 / 3 * nrow(df_current_date)
)]


df_large_for_regression <- as.data.table(df_for_regression)
