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

preds_naive_1m <- tibble(
  Country = character(),
  Date = as.POSIXct(NA),
  value = numeric()
)

resid_naive_1m <- tibble(
  Country = character(),
  Date = as.POSIXct(NA),
  value = numeric()
)

for (country in countries_PPI) {
  sample <- data$PPI %>%
    filter(geo %in% country) %>%
    arrange(time) %>%
    drop_na()
    
  pred <- sample %>%
    tail(1) %>%
    pull(values, time)

  preds_naive_1m <- preds_naive_1m %>%
    add_row(Country = country, Date = date_to_pred, value = as.numeric(pred))
  
  resid_naive_1m <- rbind(resid_naive_1m, sample %>% mutate(value = c(NA,diff(values)))%>%
                            rename(Country = geo, Date = time)%>%
                            select(Country, Date, value))
}
