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

preds_naive_1y <- tibble(
  Country = character(),
  Date = as.POSIXct(NA),
  value = numeric()
)

resid_naive_1y <- tibble(
  Country = character(),
  Date = as.POSIXct(NA),
  value = numeric()
)

for (country in countries_tourism) {
  pred <- data$TOURISM %>%
    filter(geo %in% country) %>%
    arrange(time) %>%
    filter(month(time) == month(date_to_pred)) %>%
    drop_na() %>%
    tail(1) %>%
    pull(values, time)

  preds_naive_1y <- preds_naive_1y %>%
    add_row(
      Country = country,
      Date = date_to_pred,
      value = as.numeric(pred)
    )
  
  resid_naive_1y <- rbind(resid_naive_1y, 
                          ww<-data$TOURISM %>%
                            filter(geo %in% country) %>%
                            arrange(time) %>% mutate(value = c(rep(NA,12), diff(values,12)))%>%
                            rename(Country = geo, Date = time)%>%
                            select(Country, Date, value))
}
