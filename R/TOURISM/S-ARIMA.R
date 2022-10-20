###############################################################################
#                  Time series models : (S)ARIMA                              #  
###############################################################################

#########################################
# Required packages
#########################################
library(dplyr)
library(astsa)

#########################################
# Estimate a SARIMA
#########################################

preds_sarima <- c()
for (country in countries_tourism){
  
  pred <- sarima.for(data$TOURISM%>%filter(geo %in% country & time <= early_date_to_stop)%>%pull(values),
                     1,
                     0, 1, 1, 
                     0, 1, 1, 12,
                     plot = FALSE)$pred[1]
  
  preds_sarima <- c(preds_sarima, round(pred, 1))
}
preds_sarima <- setNames(preds_sarima, nm = countries_tourism)
