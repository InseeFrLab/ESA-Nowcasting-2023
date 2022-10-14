###############################################################################
#                             Data import                                     #  
###############################################################################

#########################################
# 0) Import packages
#########################################

library(dplyr)
library(quantmod)

#########################################
# 1) Import variables
#########################################

brent_id <- "BZ=F"
eur_usd_id <- "EURUSD=X"
sp500_id <- "^GSPC"

#########################################
# 2) Import variables
#########################################

getSymbols(brent_id, src="yahoo")
brent_data <- data.frame(date=index(`BZ=F`), coredata(`BZ=F`))

getSymbols(eur_usd_id, src="yahoo")
eur_usd_data <- data.frame(date=index(`EURUSD=X`), coredata(`EURUSD=X`))

getSymbols(eur_usd_id, src="yahoo")
eur_usd_data <- data.frame(date=index(`EURUSD=X`), coredata(`EURUSD=X`))



