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
cac40_id <- "^FCHI"
eurostoxx500_id <- "^STOXX50E"

#########################################
# 2) Import variables
#########################################

getSymbols(brent_id, src="yahoo")
brent_data <- data.frame(date=index(`BZ=F`), coredata(`BZ=F`))

getSymbols(eur_usd_id, src="yahoo")
eur_usd_data <- data.frame(date=index(`EURUSD=X`), coredata(`EURUSD=X`))

getSymbols(sp500_id, src="yahoo")
sp500_data <- data.frame(date=index(`GSPC`), coredata(`GSPC`))

getSymbols(cac40_id, src="yahoo")
cac40_data <- data.frame(date=index(`FCHI`), coredata(`FCHI`))

getSymbols(eurostoxx500_id, src="yahoo")
eurostoxx500_data <- data.frame(date=index(`STOXX50E`), coredata(`STOXX50E`))

