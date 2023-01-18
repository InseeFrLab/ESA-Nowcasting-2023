################################################################################
#          Analyze residuals between past predictions and actual values        #
################################################################################

##################################
# Imports
##################################

### Packages

library(jsonlite)
library(dplyr)

### Other files

source("R/PPI/results_analysis/models_submissions.R")

### Global variables

start_date <- as.Date("2022-09-01")
date <- as.Date("2022-09-01")
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
  
  json_data <- fromJSON(paste0("Submissions/PPI/results_",month,".json"))
  list_df_entries <- lapply(json_data, as.data.frame)
  
  for (entry in names(list_df_entries)){
    
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

plot_preds(df_recent_ppi, df_submissions,
           countries_PPI[1:9], xlim = start_date,
           ncol = 3)



