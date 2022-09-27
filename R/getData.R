###############################################################################
#                             Data import                                     #  
###############################################################################

#########################################
# 0.a) Import packages
#########################################

library(eurostat)
library(dplyr)
library(tidyr)
library(ggplot2)

#########################################
# 0.b) Import global variables
#########################################

source("R/globalVariables.R")

#########################################
# 1. Get the data and create the dataframes
#########################################

ppi_data <- get_eurostat("sts_inppd_m",
                         select_time = "M",
                         filters = list(
                           geo = countries_PPI, #How about keeping all countries here?
                           indic_bt="PRIN",
                           nace_r2="B-E36", 
                           s_adj="NSA",
                           unit="I15"
                         ),
                         time_format = "date"
  ) %>%
  select(geo, time, values) %>%
  drop_na() %>%
  pivot_wider(names_from = geo, values_from = values) %>%
  arrange(time)

pvi_data <- get_eurostat("sts_inpr_m",
                         select_time = "M",
                         filters = list(
                           geo = countries_PVI, #How about keeping all countries here?
                           indic_bt="PROD",
                           nace_r2="B-D", 
                           s_adj="SCA",
                           unit="I15"
                         ),
                         time_format = "date"
  ) %>%
  select(geo, time, values) %>%
  drop_na() %>%
  pivot_wider(names_from = geo, values_from = values) %>%
  arrange(time)

tourism_data <- get_eurostat("tour_occ_nim",
                         select_time = "M",
                         filters = list(
                           geo = countries_tourism, #How about keeping all countries here?
                           c_resid="TOTAL",
                           nace_r2="I551-I553", 
                           unit="NR"
                         ),
                         time_format = "date"
  ) %>%
  select(geo, time, values) %>%
  drop_na() %>%
  pivot_wider(names_from = geo, values_from = values) %>%
  arrange(time)


#########################################
# 2. Basic visualizations
#########################################

#A) Give global variables
country = 'GE'

#B) Compute plots
ppi_plots <- ggplot(ppi_data, aes(time, FR)) + geom_point()
pvi_plots <- ggplot(pvi_data, aes(time, FR)) + geom_point()
tourism_plots <- ggplot(tourism_data, aes(time, FR)) + geom_line()

#C) Show plots
#tourism_plots

