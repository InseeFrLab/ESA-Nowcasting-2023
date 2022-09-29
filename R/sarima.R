###############################################################################
#                  Time series models : (S)ARIMA                              #  
###############################################################################

#########################################
# 0.a) Import packages
#########################################

library(dplyr)
library(astsa)

#########################################
# 0.b) Import global variables
#########################################

source("R/globalVariables.R")
source("R/getData.R")

#########################################
# 1. Test the time series
#########################################

country = 'DE'

# 1.a) PPI

ppi_ts <- ts(ppi_data %>%
               select(country))
plot(ppi_ts)
acf(ppi_ts, na.action = na.pass)
pacf(ppi_ts, na.action = na.pass)
sarima(ppi_ts, 0, 1, 1, details = FALSE)

# 1.b) PVI

pvi_ts <- ts(pvi_data %>%
               select(country))
plot(pvi_ts)
acf(pvi_ts, na.action = na.pass)
pacf(pvi_ts, na.action = na.pass)
sarima(pvi_ts, 0, 1, 1, details = FALSE)

# 1.c) Tourism

tourism_ts <- ts(tourism_data %>%
               select(country))
plot(tourism_ts)
acf(tourism_ts, na.action = na.pass)
pacf(tourism_ts, na.action = na.pass)
sarima(tourism_ts, 0, 1, 1, 0, 1, 1, 12, details = FALSE)


#########################################
# 2. Implement the predictions
#########################################

# 2.a) PPI

ppi_preds_arima <- data.frame(matrix(ncol = length(countries_PPI), nrow = 1))
colnames(ppi_preds_arima) <- countries_PPI

for (country in countries_PPI){
  pred <- sarima.for(ppi_data %>%
                       select(country), 1,
                     0, 1, 1, plot = FALSE)
  ppi_preds_arima[[country]][1] <- pred
}

ppi_preds_arima

# 2.b) PVI

pvi_preds_arima <- data.frame(matrix(ncol = length(countries_PVI), nrow = 1))
colnames(pvi_preds_arima) <- countries_PVI

for (country in countries_PVI){
  pred <- sarima.for(pvi_data %>%
                       select(country), 1,
                     0, 1, 1, plot = FALSE)
  pvi_preds_arima[[country]][1] <- pred
}

pvi_preds_arima

# 2.c) Tourism

tourism_preds_sarima <- data.frame(matrix(ncol = length(countries_tourism), nrow = 1))
colnames(tourism_preds_sarima) <- countries_tourism

for (country in countries_tourism){
  pred <- sarima.for(tourism_data %>%
                       select(country), 1,
                     0, 1, 1, 0, 1, 1, 12, plot = FALSE)
  tourism_preds_sarima[[country]][1] <- pred
}

tourism_preds_sarima





