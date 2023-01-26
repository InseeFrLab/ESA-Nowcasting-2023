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
library(quantmod)
library(lubridate)
library(readr)

source("R/utils/globalVariables.R")

#########################################
# Import tables that can be useful for all other data imports
#########################################

# Matching table between country names and ISO codes

countries_codes <- read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv") %>%
  rename(
    country_name = Country,
    geo = `Alpha-2 code`,
    geo_3_letters = `Alpha-3 code`
  ) %>%
  select(country_name, geo, geo_3_letters)

#########################################
# Define a function that retrieves data
#########################################

getData <- function(case) {
  defaultW <- getOption("warn")
  options(warn = -1)

  db <- list()
  switch(case,
    # Retrieve data for PPI challenge
    PPI = {
      # Import PPI serie
      data <- get_eurostat("sts_inppd_m",
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

      db[["PPI"]] <- data

      # Producer price index for all subcategories of level 2
      data <- get_eurostat("sts_inppd_m",
        select_time = "M",
        filters = list(
          geo = countries_PPI,
          indic_bt = "PRIN",
          nace_r2 = c(paste0("B", str_pad(5:9, 2, pad = "0")), paste0("C", 10:33), "D35", "E36"),
          s_adj = "NSA",
          unit = "I15"
        ),
        time_format = "date"
      ) %>%
        select(geo, nace_r2, time, values) %>%
        drop_na(values)

      db[["PPI_NACE2"]] <- data

      # Import price index for all subcategories of level 2 + two aggregates
      data <- get_eurostat("sts_inpi_m",
        select_time = "M",
        filters = list(
          geo = countries_PPI,
          indic_bt = "IMPR",
          cpa2_1 = c(
            paste0("CPA_B", str_pad(5:8, 2, pad = "0")), paste0("CPA_C", 10:32),
            "CPA_D35", "CPA_B-E36", "CPA_B-D",
            "CPA_MIG_ING", "CPA_MIG_CAG", "CPA_MIG_NRG_X_E"
          ),
          s_adj = "NSA",
          unit = "I15"
        ),
        time_format = "date"
      ) %>%
        select(geo, cpa2_1, time, values) %>%
        drop_na(values)

      db[["IPI"]] <- data

      # Retrieve several surveys on production prices (confidence, price expectations, employment expectations)
      data <- get_eurostat("ei_bsin_m_r2",
        select_time = "M",
        filters = list(
          geo = countries_PVI,
          indic = c("BS-ICI", "BS-ISPE", "BS-IEME"),
          s_adj = "NSA",
          unit = "BAL"
        ),
        time_format = "date"
      ) %>%
        select(geo, indic, time, values) %>%
        drop_na(values)

      db[["PSURVEY"]] <- data

      # Import PVI serie
      data <- get_eurostat("sts_inpr_m",
        select_time = "M",
        filters = list(
          geo = countries_PVI, # How about keeping all countries here?
          indic_bt = "PROD",
          nace_r2 = "B-D",
          s_adj = "SCA",
          unit = "I15"
        ),
        time_format = "date"
      ) %>%
        select(geo, time, values) %>%
        drop_na(values)

      db[["PVI"]] <- data

      # Import inflation data
      data <- get_eurostat("prc_hicp_midx",
        select_time = "M"
      ) %>%
        filter(
          geo %in% countries_PPI,
          unit == "I15",
          nchar(coicop) == 4
        ) %>%
        select(-unit)

      db[["HICP"]] <- data

      # Retrieve daily BRENT index from Yahoo Finance
      brent_id <- "BZ=F"
      getSymbols(brent_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`BZ=F`), coredata(`BZ=F`)) %>%
        rename(
          time = date,
          brent_adjusted = BZ.F.Adjusted,
          brent_volume = BZ.F.Volume
        ) %>%
        select(time, brent_adjusted, brent_volume)
      db[["brent"]] <- data

      # Retrieve daily euro/dollar exchange rate from Yahoo Finance
      eur_usd_id <- "EURUSD=X"
      getSymbols(eur_usd_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`EURUSD=X`), coredata(`EURUSD=X`)) %>%
        rename(
          time = date,
          eur_usd_adjusted = EURUSD.X.Adjusted,
          eur_usd_volume = EURUSD.X.Volume
        ) %>%
        select(time, eur_usd_adjusted, eur_usd_volume)
      db[["eur_usd"]] <- data

      # Retrieve daily SP&500 index from Yahoo Finance
      sp500_id <- "^GSPC"
      getSymbols(sp500_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`GSPC`), coredata(`GSPC`)) %>%
        rename(
          time = date,
          sp500_adjusted = GSPC.Adjusted,
          sp500_volume = GSPC.Volume
        ) %>%
        select(time, sp500_adjusted, sp500_volume)
      db[["sp500"]] <- data

      # Retrieve daily EUROSTOXX500 index from Yahoo Finance
      eurostoxx500_id <- "^STOXX50E"
      getSymbols(eurostoxx500_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`STOXX50E`), coredata(`STOXX50E`)) %>%
        rename(
          time = date,
          eurostoxx500_adjusted = STOXX50E.Adjusted,
          eurostoxx500_volume = STOXX50E.Volume
        ) %>%
        select(time, eurostoxx500_adjusted, eurostoxx500_volume)
      db[["eurostoxx500"]] <- data

      # Retrieve daily CAC40 index from Yahoo Finance
      cac40_id <- "^FCHI"
      getSymbols(cac40_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`FCHI`), coredata(`FCHI`)) %>%
        rename(
          time = date,
          cac40_adjusted = FCHI.Adjusted,
          cac40_volume = FCHI.Volume
        ) %>%
        select(time, cac40_adjusted, cac40_volume)
      db[["cac40"]] <- data

      data <- read_csv("https://ember-climate.org/app/uploads/2022/09/european_wholesale_electricity_price_data_daily-5.csv") %>%
        rename(
          country_name = Country,
          time = Date,
          electricity_price = `Price (EUR/MWhe)`
        ) %>%
        inner_join(countries_codes %>% select(-geo_3_letters)) %>%
        select(geo, time, electricity_price)
      db[["electricity_prices"]] <- data
    },

    # Retrieve data for PVI challenge
    PVI = {
      # Import PVI serie
      data <- get_eurostat("sts_inpr_m",
        select_time = "M",
        filters = list(
          geo = countries_PVI, # How about keeping all countries here?
          indic_bt = "PROD",
          nace_r2 = "B-D",
          s_adj = "SCA",
          unit = "I15"
        ),
        time_format = "date"
      ) %>%
        select(geo, time, values) %>%
        drop_na(values)

      db[["PVI"]] <- data

      # Retrieve several surveys on production (confidence, production expectations,stocks, orders)
      data <- get_eurostat("ei_bsin_m_r2",
        select_time = "M",
        filters = list(
          geo = countries_PVI,
          indic = c("BS-ICI", "BS-IPT", "BS-IOB", "BS-ISFP", "BS-IPE"),
          s_adj = "SA",
          unit = "BAL"
        ),
        time_format = "date"
      ) %>%
        select(geo, indic, time, values) %>%
        drop_na(values)

      db[["PSURVEY"]] <- data

      # Import price index for all subcategories of level 2 + two aggregates
      data <- get_eurostat("sts_inpi_m",
        select_time = "M",
        filters = list(
          geo = countries_PPI,
          indic_bt = "IMPR",
          cpa2_1 = c(
            paste0("CPA_B", str_pad(5:8, 2, pad = "0")), paste0("CPA_C", 10:32),
            "CPA_D35", "CPA_B-E36", "CPA_B-D",
            "CPA_MIG_ING", "CPA_MIG_CAG", "CPA_MIG_NRG_X_E"
          ),
          s_adj = "NSA",
          unit = "I15"
        ),
        time_format = "date"
      ) %>%
        select(geo, cpa2_1, time, values) %>%
        drop_na(values)

      db[["IPI"]] <- data

      # Retrieve daily BRENT index from Yahoo Finance
      brent_id <- "BZ=F"
      getSymbols(brent_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`BZ=F`), coredata(`BZ=F`)) %>%
        rename(
          time = date,
          brent_adjusted = BZ.F.Adjusted,
          brent_volume = BZ.F.Volume
        ) %>%
        select(time, brent_adjusted, brent_volume)
      db[["brent"]] <- data

      data <- get_eurostat("sts_inppd_m",
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

      db[["PPI"]] <- data

      # Import inflation data
      data <- get_eurostat("prc_hicp_midx",
        select_time = "M"
      ) %>%
        filter(
          geo %in% countries_PPI,
          unit == "I15",
          nchar(coicop) == 4
        ) %>%
        select(-unit)

      db[["HICP"]] <- data

      # Retrieve daily euro/dollar exchange rate from Yahoo Finance
      eur_usd_id <- "EURUSD=X"
      getSymbols(eur_usd_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`EURUSD=X`), coredata(`EURUSD=X`)) %>%
        rename(
          time = date,
          eur_usd_adjusted = EURUSD.X.Adjusted,
          eur_usd_volume = EURUSD.X.Volume
        ) %>%
        select(time, eur_usd_adjusted, eur_usd_volume)
      db[["eur_usd"]] <- data

      # Retrieve daily SP&500 index from Yahoo Finance
      sp500_id <- "^GSPC"
      getSymbols(sp500_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`GSPC`), coredata(`GSPC`)) %>%
        rename(
          time = date,
          sp500_adjusted = GSPC.Adjusted,
          sp500_volume = GSPC.Volume
        ) %>%
        select(time, sp500_adjusted, sp500_volume)
      db[["sp500"]] <- data

      # Retrieve daily EUROSTOXX500 index from Yahoo Finance
      eurostoxx500_id <- "^STOXX50E"
      getSymbols(eurostoxx500_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`STOXX50E`), coredata(`STOXX50E`)) %>%
        rename(
          time = date,
          eurostoxx500_adjusted = STOXX50E.Adjusted,
          eurostoxx500_volume = STOXX50E.Volume
        ) %>%
        select(time, eurostoxx500_adjusted, eurostoxx500_volume)
      db[["eurostoxx500"]] <- data

      # Retrieve daily CAC40 index from Yahoo Finance
      cac40_id <- "^FCHI"
      getSymbols(cac40_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`FCHI`), coredata(`FCHI`)) %>%
        rename(
          time = date,
          cac40_adjusted = FCHI.Adjusted,
          cac40_volume = FCHI.Volume
        ) %>%
        select(time, cac40_adjusted, cac40_volume)
      db[["cac40"]] <- data

      data <- read_csv("https://ember-climate.org/app/uploads/2022/09/european_wholesale_electricity_price_data_daily-5.csv") %>%
        rename(
          country_name = Country,
          time = Date,
          electricity_price = `Price (EUR/MWhe)`
        ) %>%
        inner_join(countries_codes %>% select(-geo_3_letters)) %>%
        select(geo, time, electricity_price)
      db[["electricity_prices"]] <- data

      dates <- seq(as.Date("2007-01-01"), date_to_pred, by = "month")
      nb_weekend_days <- data.frame(month = month(dates), year = year(dates), weekends = numeric(length(dates)))
      for (i in 1:length(dates)) {
        month_start <- as.Date(paste(nb_weekend_days$year[i], nb_weekend_days$month[i], 1, sep = "-"))
        month_end <- as.Date(paste(nb_weekend_days$year[i], nb_weekend_days$month[i], days_in_month(month_start), sep = "-"))
        nb_weekend_days$weekends[i] <- sum(wday(seq(month_start, month_end, by = "day")) %in% c(6, 7))
      }
      db[["nb_weekend_days"]] <- nb_weekend_days
    },

    # Retrieve data for TOUR challenge
    TOURISM = {
      data <- get_eurostat("tour_occ_nim",
        select_time = "M",
        filters = list(
          geo = countries_tourism, # How about keeping all countries here?
          c_resid = "TOTAL",
          nace_r2 = "I551-I553",
          unit = "NR"
        ),
        time_format = "date"
      ) %>%
        select(geo, time, values) %>%
        drop_na(values)

      db[["TOURISM"]] <- data

      # Retrieve several surveys on production prices (confidence, price expectations, employment expectations)
      data <- get_eurostat("ei_bsin_m_r2",
        select_time = "M",
        filters = list(
          geo = countries_PVI,
          indic = c("BS-ICI", "BS-ISPE", "BS-IEME"),
          s_adj = "NSA",
          unit = "BAL"
        ),
        time_format = "date"
      ) %>%
        select(geo, indic, time, values) %>%
        drop_na(values)

      db[["PSURVEY"]] <- data

      # Import inflation data
      data <- get_eurostat("prc_hicp_midx",
        select_time = "M"
      ) %>%
        filter(
          geo %in% countries_PPI,
          unit == "I15",
          nchar(coicop) == 4
        ) %>%
        select(-unit)

      db[["HICP"]] <- data

      # Retrieve daily BRENT index from Yahoo Finance
      brent_id <- "BZ=F"
      getSymbols(brent_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`BZ=F`), coredata(`BZ=F`)) %>%
        rename(
          time = date,
          brent_adjusted = BZ.F.Adjusted,
          brent_volume = BZ.F.Volume
        ) %>%
        select(time, brent_adjusted, brent_volume)
      db[["brent"]] <- data

      # Retrieve daily euro/dollar exchange rate from Yahoo Finance
      eur_usd_id <- "EURUSD=X"
      getSymbols(eur_usd_id, src = "yahoo")
      data <- data.frame(date = zoo::index(`EURUSD=X`), coredata(`EURUSD=X`)) %>%
        rename(
          time = date,
          eur_usd_adjusted = EURUSD.X.Adjusted,
          eur_usd_volume = EURUSD.X.Volume
        ) %>%
        select(time, eur_usd_adjusted, eur_usd_volume)
      db[["eur_usd"]] <- data

      dates <- seq(as.Date("2007-01-01"), date_to_pred, by = "month")
      nb_weekend_days <- data.frame(month = month(dates), year = year(dates), weekends = numeric(length(dates)))
      for (i in 1:length(dates)) {
        month_start <- as.Date(paste(nb_weekend_days$year[i], nb_weekend_days$month[i], 1, sep = "-"))
        month_end <- as.Date(paste(nb_weekend_days$year[i], nb_weekend_days$month[i], days_in_month(month_start), sep = "-"))
        nb_weekend_days$weekends[i] <- sum(wday(seq(month_start, month_end, by = "day")) %in% c(6, 7))
      }
      db[["nb_weekend_days"]] <- nb_weekend_days
    },
    stop("Enter one of the 3 following challenges: PPI, PVI, TOURISM")
  )
  options(warn = defaultW)
  return(db)
}
