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

start_date <- as.Date("2022-07-01")
date <- start_date
months_past <- c('september', 'october', 'november', 'december')
# This list must be linear in time

##################################
# Construct the analysis table
##################################

### Submissions

df_submissions <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Country", "Date", "value", "Entry")

for (month in months_past){
  
  while (tolower(format(date, format = "%B")) != month){
    date <- date + months(1)
    print(month)
  }
  
  json_data <- jsonlite::fromJSON(
    paste0("Submissions/TOURISM/results_", month,".json"))
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
      
      df_submissions <- df_submissions %>%
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

plot_preds(df_recent_tourism, df_submissions,
           countries_tourism[1:9], xlim = start_date,
           ncol = 3)

plot_preds(df_recent_tourism, df_submissions,
           countries_tourism[10:18], xlim = start_date,
           ncol = 3)

plot_preds(df_recent_tourism, df_submissions,
           countries_tourism[-1:-18], xlim = start_date,
           ncol = 3)

