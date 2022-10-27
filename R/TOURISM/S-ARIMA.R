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

date_to_predict <- ymd(date_to_predict)

preds_sarima <- tibble(Country=character(),
                       Date=as.POSIXct(NA),
                       value=numeric()
)

for (country in countries_tourism){
  
  print(country)
  
  last_month_given <- data$TOURISM %>%
    filter(geo %in% country) %>%
    arrange(time) %>%
    tail(1) %>%
    pull(time)
  
  n_forward <- interval(last_month_given, date_to_predict)
  
  pred <- sarima.for(data$TOURISM %>%
                       filter(geo %in% country & time <= early_date_to_stop) %>%
                       pull(values),
                     n_forward,
                     0, 1, 1, 
                     0, 1, 1, 12, plot = FALSE)$pred[1]
  
  preds_sarima <- preds_sarima %>%
    add_row(Country=country,
            Date=date_to_predict,
            value=round(as.numeric(pred),1))

}
