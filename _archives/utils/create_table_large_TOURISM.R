###############################################################################
#                       Create large table for ML                             #
###############################################################################

# Returns two important tables:
# df_large
# df_large_for_regression, where unuseful variables have been removed

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

# nb_months_past_to_use <- 10
# nb_years_past_to_use <- 5
# nb_months_past_to_use_others <- 5

list_eurostat_tables <- c("PSURVEY", "HICP")
list_yahoo_finance <- c("brent", "eur_usd")

db <- getData("TOURISM")

create_table_large_tourism <- function(nb_months_past_to_use = 10,
                                       nb_past_years_to_use = 5,
                                       nb_months_past_to_use_others = 5) {
  #########################################
  # Create the tables for the regression
  #########################################

  # A) Initialize table

  countries <- db$TOURISM %>%
    select(geo) %>%
    unique() %>%
    mutate(dummy = 1)

  dates <- db$TOURISM %>%
    select(time) %>%
    add_row(time = current_date) %>%
    # add_row(time = date_to_pred) %>%
    add_row(time = date(current_date %m-% months(1))) %>%
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

  df_TOURISM <- db$TOURISM %>%
    full_join(df) %>%
    rename(TOURISM = values) %>%
    group_by(geo) %>%
    mutate(TOURISM_to_predict = lead(TOURISM))
  for (i in 1:nb_months_past_to_use) {
    variable <- paste("TOURISM", "minus", i, "months", sep = "_")
    df_TOURISM <- df_TOURISM %>%
      mutate(!!variable := lag(TOURISM, n = i))
  }
  for (i in 1:nb_years_past_to_use) {
    variable <- paste("TOURISM", "minus", i, "years", sep = "_")
    df_TOURISM <- df_TOURISM %>%
      mutate(!!variable := lag(TOURISM, n = 12 * i - 1))
  }
  df_TOURISM <- df_TOURISM %>%
    ungroup()

  df <- df %>%
    left_join(df_TOURISM) %>%
    mutate(
      month = month(time),
      year = year(time)
    ) %>%
    relocate(time, geo, TOURISM_to_predict)

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

  # B - bis) Add history of Eurostat data

  df <- df %>%
    group_by(geo)

  list_other_variables <- colnames(df)[
    (7 + nb_months_past_to_use + nb_years_past_to_use):(length(colnames(df)))
  ]

  for (i in 1:nb_months_past_to_use_others) {
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
        time = time - months(1),
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
      mean_adjusted_string <- paste("mean", adjusted_string, "week", i, "next_month",
        sep = "_"
      )
      mean_volume_string <- paste("mean", volume_string, "week", i, "next_month",
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

  # D) Add number of weeekend days per month

  df <- df %>%
    left_join(db[["nb_weekend_days"]] %>%
      mutate(weekends = lead(weekends)) %>%
      rename(weekends_next_month = weekends))

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

  df_current_date <- df %>%
    filter(time == current_date)
  df_large_for_regression <- df_large[c(
    rep(TRUE, 3),
    colSums(
      !is.na(df_current_date[-(1:3)])
    ) > 2 / 3 * nrow(df_current_date)
  )]

  df_large_for_regression <- as.data.table(df_large_for_regression)

  return(list(
    countries = countries,
    df_large = df_large,
    df_large_for_regression = df_large_for_regression
  ))
}
