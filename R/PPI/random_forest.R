###############################################################################
#                             Data import                                     #  
###############################################################################

#########################################
# Import packages and set-up
#########################################

library(dplyr)
library(tidyr)
library(stringr)

source("R/utils/globalVariables.R")

#########################################
# Define a function that retrieves data
#########################################

df = db$PPI %>%
  select
for (data in db)
  