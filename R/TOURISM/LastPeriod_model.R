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

preds_naive_1y <- tibble(Country=character(),
                                        Date=as.POSIXct(NA),
                                        value=numeric()
)

for (country in countries_tourism){
  pred <- data$TOURISM%>%
                filter(geo %in% country)%>%
                arrange(time)%>%
                tail(12)%>% # take the last 12 months
                head(1)%>% # select the value of reference 1-year before
                pull(values, time)
  
  preds_naive_1y <- preds_naive_1y%>%
    add_row(Country=country, Date=as.POSIXct(names(pred)) %m+% months(1), value=as.numeric(pred))

}
