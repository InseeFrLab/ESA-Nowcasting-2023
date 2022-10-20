###############################################################################
#             Naive model based on the last observed value                    #  
###############################################################################

#########################################
# Required packages
#########################################
library(dplyr)

#############################################
# Use the value of the last available month
#############################################

preds_naive_1m <- c()
for (country in countries_PPI){
  
  preds_naive_1m <- c(preds_naive_1m, data$PPI%>%
                            filter(geo %in% country)%>%
                            arrange(time)%>%
                            tail(1)%>%
                            pull(values))
  
  
}
preds_naive_1m <- setNames(preds_naive_1m, nm = countries_PPI)
