###############################################################################
#                  Time series models : (S)ARIMA                              #  
###############################################################################

#########################################
# Required packages
#########################################

library(dplyr)
library(astsa)
library(lubridate)

#########################################
# Estimate a SARIMA
#########################################

preds_sarima <- tibble(Country=character(),
                       Date=as.POSIXct(NA),
                       value=numeric()
)

for (country in countries_tourism){
  
  n_forward <- interval(early_date_to_stop, date_to_pred) %/% months(1) %% 12
  
  pred <- sarima.for(data$TOURISM %>%
                       filter(geo == country & time <= early_date_to_stop) %>%
                       pull(values),
                     n_forward,
                     0, 1, 1, 
                     0, 1, 1, 12, plot = FALSE)$pred %>%
    tail(1)
  
  preds_sarima <- preds_sarima %>%
    add_row(Country=country,
            Date=date_to_pred,
            value=round(as.numeric(pred),1))
  
}
