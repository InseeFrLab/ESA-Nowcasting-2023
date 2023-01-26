################################################################################
#          Analyze residuals between past predictions and actual values        #
################################################################################

##################################
# Imports
##################################

### Packages

library(jsonlite)
library(dplyr)
library(data.table)
library(tibble)
library(tidyr)
library(lubridate)
library(eurostat)

### Other files

source("R/utils/globalVariables.R")
source("R/TOURISM/results_analysis/models_submissions.R")
source("R/utils/functions.R")

### Global variables

start_date <- as.Date("2022-06-01")
date <- start_date
months_past <- c("september", "october", "november", "december", "january")
# This list must be linear in time

##################################
# Construct the analysis table
##################################

### Submissions

df_submissions_tourism <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Country", "Date", "value", "Entry")

for (month in months_past) {
  while (tolower(format(date, format = "%B")) != month) {
    date <- date + months(1)
    print(month)
  }

  json_data <- jsonlite::fromJSON(
    paste0("Submissions/TOURISM/results_", month, ".json")
  )
  list_df_entries <- lapply(json_data, as.data.frame)

  for (entry in names(list_df_entries)) {
    if (length(list_df_entries[[entry]]) > 0) {
      df_entry <- as.data.frame(unlist(list_df_entries[[entry]])) %>%
        rename(value = 1) %>%
        rownames_to_column(var = "Country") %>%
        mutate(
          Date = date,
          value = as.numeric(value),
          Entries = correspondance_entries[[month]][[entry]]
        ) %>%
        relocate(Country, Date, value, Entries)

      df_submissions_tourism <- df_submissions_tourism %>%
        rbind(df_entry)
    }
  }
}

### Actual values

df_tourism <- get_eurostat("tour_occ_nim",
  select_time = "M",
  filters = list(
    geo = countries_tourism,
    c_resid = "TOTAL",
    nace_r2 = "I551-I553",
    unit = "NR"
  ),
  time_format = "date"
) %>%
  select(geo, time, values) %>%
  drop_na(values)

# Restrict to the recent dates

df_recent_tourism <- df_tourism %>%
  filter(time >= start_date)

##################################
# Compare the results
##################################

plot_preds(df_recent_tourism, df_submissions_tourism,
  countries_tourism[1:9],
  xlim = start_date,
  ncol = 3
)

plot_preds(df_recent_tourism, df_submissions_tourism,
  countries_tourism[10:18],
  xlim = start_date,
  ncol = 3
)

plot_preds(df_recent_tourism, df_submissions_tourism,
  countries_tourism[-1:-18],
  xlim = start_date,
  ncol = 3
)

##################################
# Comparison in a dataframe
##################################

df_compare_tourism <- df_submissions_tourism %>%
  rename(Prediction = value) %>%
  left_join(df_recent_tourism %>%
    rename(
      Date = time,
      Country = geo,
      TrueValue = values
    )) %>%
  mutate(
    Diff = Prediction - TrueValue,
    AbsoluteDiff = abs(Diff)
  ) %>%
  arrange(Country, Date)


### The residuals grouped by country

df_MAE_by_country_tourism <- df_compare_tourism %>%
  filter(!is.na(Diff)) %>%
  group_by(Country, Entries) %>%
  summarise(
    MAE_country = mean(AbsoluteDiff, na.rm = TRUE),
    n_predictions = n()
  ) %>%
  arrange(Country, MAE_country) %>%
  ungroup() %>%
  group_by(Country) %>%
  mutate(rank = rank(MAE_country))

### The residuals grouped by month

df_MAE_by_month_tourism <- df_compare_tourism %>%
  filter(!is.na(Diff)) %>%
  group_by(Date, Entries) %>%
  summarise(
    MAE_month = mean(AbsoluteDiff, na.rm = TRUE),
    n_predictions = n()
  ) %>%
  arrange(Date, MAE_month) %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(rank = rank(MAE_month))

### The residuals grouped by model only

df_MAE_by_model_tourism <- df_compare_tourism %>%
  filter(!is.na(Diff)) %>%
  group_by(Entries) %>%
  summarise(
    MAE_model = mean(AbsoluteDiff, na.rm = TRUE),
    n_predictions = n()
  ) %>%
  arrange(MAE_model) %>%
  ungroup() %>%
  mutate(rank = rank(MAE_model))
