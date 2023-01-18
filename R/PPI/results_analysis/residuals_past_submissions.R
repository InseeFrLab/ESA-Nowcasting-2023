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
source("R/PPI/results_analysis/models_submissions.R")
source("R/utils/functions.R")

### Global variables

start_date <- as.Date("2022-06-01")
date <- start_date
months_past <- c('september', 'october', 'november', 'december', 'january')
# This list must be linear in time

##################################
# Construct the analysis table
##################################

### Submissions

df_submissions_ppi <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Country", "Date", "value", "Entry")

for (month in months_past){
  
  while (tolower(format(date, format = "%B")) != month){
    date <- date + months(1)
    print(month)
  }
  
  json_data <- jsonlite::fromJSON(
    paste0("Submissions/PPI/results_", month,".json"))
  list_df_entries <- lapply(json_data, as.data.frame)
  
  for (entry in names(list_df_entries)){
    
    if (length(list_df_entries[[entry]]) > 0){
      df_entry <- as.data.frame(unlist(list_df_entries[[entry]])) %>%
        rename(value = 1) %>%
        rownames_to_column(var='Country') %>%
        mutate(Date = date,
               value = as.numeric(value),
               Entries = correspondance_entries[[month]][[entry]]) %>%
        relocate(Country, Date, value, Entries)
      
      df_submissions_ppi <- df_submissions_ppi %>%
        rbind(df_entry)
    }
  }
}

### Actual values

df_ppi <- get_eurostat("sts_inppd_m",
                       select_time = "M",
                       filters = list(
                         geo = countries_PPI,
                         indic_bt = "PRIN",
                         nace_r2 = "B-E36",
                         s_adj = "NSA",
                         unit = "I15"
                       ),
                       time_format = "date"
) %>%
  select(geo, nace_r2, time, values) %>%
  drop_na(values)

# Restrict to the recent dates

df_recent_ppi <- df_ppi %>%
  filter(time >= start_date)

##################################
# Compare the results
##################################

plot_preds(df_recent_ppi, df_submissions_ppi,
           countries_PPI[1:9], xlim = start_date,
           ncol = 3)

plot_preds(df_recent_ppi, df_submissions_ppi,
           countries_PPI[10:18], xlim = start_date,
           ncol = 3)

plot_preds(df_recent_ppi, df_submissions_ppi,
           countries_PPI[-1:-18], xlim = start_date,
           ncol = 3)

##################################
# Comparison in a dataframe
##################################

df_compare_ppi <- df_submissions_ppi %>%
  rename(Prediction = value) %>%
  left_join(df_recent_ppi %>%
              select(-nace_r2) %>%
              rename(Date = time,
                     Country = geo,
                     TrueValue = values)) %>%
  mutate(Diff = Prediction - TrueValue) %>%
  arrange(Country, Date)

            