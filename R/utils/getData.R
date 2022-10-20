###############################################################################
#                             Data import                                     #  
###############################################################################

#########################################
# Import packages and set-up
#########################################

library(eurostat)
library(dplyr)
library(tidyr)
library(stringr)

source("R/utils/globalVariables.R")

#########################################
# Define a function that retrieves data
#########################################

getData <- function(case){
  defaultW <- getOption("warn") 
  options(warn = -1) 
  
  db = list()
  switch(case,
         # Retrieve data for PPI challenge
         PPI={
           # Import PPI serie
           data <- get_eurostat("sts_inppd_m",
                                select_time = "M",
                                filters = list(
                                  geo = countries_PPI,
                                  indic_bt="PRIN",
                                  nace_r2="B-E36", 
                                  s_adj="NSA",
                                  unit="I15"
                                ),
                                time_format = "date")%>%
             select(geo, nace_r2, time, values) %>%
             drop_na(values)
          
           db[["PPI"]] = data
           
           # Producer price index for all subcategories of level 2 
           data <- get_eurostat("sts_inppd_m",
                                select_time = "M",
                                filters = list(
                                  geo = countries_PPI,
                                  indic_bt="PRIN",
                                  nace_r2= c(paste0("B", str_pad(5:9, 2, pad = "0")),paste0("C", 10:33), "D35", "E36"), 
                                  s_adj="NSA",
                                  unit="I15"
                                ),
                                time_format = "date")%>%
             select(geo, nace_r2, time, values) %>%
             drop_na(values) 
           
           db[["PPI_NACE2"]] = data
           
          # Import price index for all subcategories of level 2 + two aggregates
           data <- get_eurostat("sts_inpi_m",
                        select_time = "M",
                        filters = list(
                          geo = countries_PPI,
                          indic_bt="IMPR",
                          nace_r2=c(paste0("B", str_pad(5:8, 2, pad = "0")),paste0("C", 10:32), "D35", "B-E36", "B-D"), 
                          s_adj="NSA",
                          unit="I15"
                        ),
                        time_format = "date")%>%
             select(geo, nace_r2, time, values) %>%
             drop_na(values)

           db[["IPI"]] = data

           # Retrieve several surveys on production prices (confidence, price expectations, employment expectations)
           data <- get_eurostat("ei_bsin_m_r2",
                                select_time = "M",
                                filters = list(
                                  geo = countries_PVI,
                                  indic= c("BS-ICI", "BS-ISPE","BS-IEME"),
                                  s_adj="NSA",
                                  unit="BAL"
                                ),
                                time_format = "date"
           ) %>%
             select(geo, indic, time, values) %>%
             drop_na(values)
           
           db[["PSURVEY"]] = data
           
         },
         
         # Retrieve data for PVI challenge
         PVI={
           # Import PVI serie
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
             drop_na(values)
           
           db[["PVI"]] = data
           
           # Retrieve several surveys on production (confidence, production expectations,stocks, orders)
           data <- get_eurostat("ei_bsin_m_r2",
                                select_time = "M",
                                filters = list(
                                  geo = countries_PVI,
                                  indic= c("BS-ICI", "BS-IPT","BS-IOB","BS-ISFP","BS-IPE"),
                                  s_adj="SA",
                                  unit="BAL"
                                ),
                                time_format = "date"
           ) %>%
             select(geo, indic, time, values) %>%
             drop_na(values)
           
           db[["PSURVEY"]] = data
           
         },
         
         # Retrieve data for TOUR challenge
         TOURISM={
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
             drop_na(values)
           
           db[["TOURISM"]] = data
         },
         
         stop("Enter one of the 3 following chalenges : PPI, PVI, TOURISM")
  )
  options(warn = defaultW)
  return(db)
}
