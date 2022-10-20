###############################################################################
#             Naive model based on the last observed value                    #  
###############################################################################

#########################################
# Required packages
#########################################
library(dplyr)

#############################################
# Use the value of preceding year 
#############################################

preds_naive_1y <- c()
for (country in countries_tourism){
  
  preds_naive_1y <- c(preds_naive_1y, data$TOURISM%>%
                                filter(geo %in% country)%>%
                                arrange(time)%>%
                                tail(12)%>% # take the last 12 months
                                head(1)%>% # select the value of reference 1-year before
                                pull(values))
  
}
preds_naive_1y <- setNames(preds_naive_1y, nm = countries_tourism)
