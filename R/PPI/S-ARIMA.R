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
for (country in countries_PPI){
  
  pred <- sarima.for(data$PPI%>%filter(geo %in% country)%>%pull(values),
                     1,
                     0, 1, 1, plot = FALSE)$pred[1]
  
  preds_sarima <- c(preds_sarima, pred)
}
preds_sarima <- setNames(preds_sarima, nm = countries_PPI)
