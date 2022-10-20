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

for (country in countries_PVI){
  pred <- sarima.for(data$PVI%>%filter(geo %in% country)%>%pull(values),
                     1,
                     0, 1, 1, plot = FALSE)$pred[1]
  
  date_to_pred <-  data$PVI%>%filter(geo %in% country)%>%arrange(time)%>%tail(1)%>%pull(time) %m+% months(1)
  
  preds_sarima <- preds_sarima %>%
    add_row(Country=country, Date=as.POSIXct(date_to_pred), value=round(as.numeric(pred),1))
  
}
