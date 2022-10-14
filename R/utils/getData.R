###############################################################################
#                             Data import                                     #  
###############################################################################

#########################################
# Import packages and set-up
#########################################

library(eurostat)
library(dplyr)
library(tidyr)

source("R/utils/globalVariables.R")

#########################################
# Fonction that retrieves data
#########################################

getData <- function(case){
  db = list()
  switch(case,
         # Retrieve data for PPI challenge
         PPI={
           data <- get_eurostat("sts_inppd_m",
                                select_time = "M",
                                filters = list(
                                  geo = countries_PPI,
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
          
           db[["PPI"]] = data
         },
         
         # Retrieve data for PVI challenge
         PVI={
           data <- get_eurostat("sts_inpr_m",
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
           
           db[["PVI"]] = data
           
         },
         
         # Retrieve data for TOUR challenge
         TOUR={
           data <- get_eurostat("tour_occ_nim",
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
           
           db[["TOUR"]] = data
         },
         
         stop("Enter one of the 3 following chalenges : PPI, PVI, TOUR")
  )
  return(db)
}
