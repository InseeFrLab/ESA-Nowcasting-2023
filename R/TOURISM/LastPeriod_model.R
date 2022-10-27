###############################################################################
#             Naive model based on the last observed value                    #  
###############################################################################

#########################################
# Required packages
#########################################

library(dplyr)
library(lubridate)

#############################################
# Use the value of preceding year 
#############################################

date_to_predict <- ymd(date_to_predict)

preds_naive_1y <- tibble(Country=character(),
                                        Date=as.POSIXct(NA),
                                        value=numeric()
)

for (country in countries_tourism){
  pred <- data$TOURISM %>%
    filter(geo %in% country) %>%
    arrange(time) %>%
    filter(month(time) == month(date_to_predict)) %>%
    drop_na() %>%
    tail(1) %>%
    pull(values, time)
  
  preds_naive_1y <- preds_naive_1y %>%
    add_row(Country=country, Date = date_to_predict, value=as.numeric(pred))

}
