###############################################################################
#             Naive model based on the last observed value                    #  
###############################################################################

#########################################
# Required packages
#########################################

library(dplyr)
library(lubridate)

#############################################
# Use the value of the last available month
#############################################

preds_naive_1m <- tibble(Country=character(),
                         Date=as.POSIXct(NA),
                         value=numeric()
                         )

for (country in countries_PPI){
  pred <- data$PPI%>%
                  filter(geo %in% country)%>%
                  arrange(time)%>%
                  tail(1)%>%
                  pull(values, time)
                
  preds_naive_1m <- preds_naive_1m%>%
    add_row(Country=country, Date=as.POSIXct(names(pred)) %m+% months(1), value=as.numeric(pred))

}
