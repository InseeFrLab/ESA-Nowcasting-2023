###############################################################################
#                  Time series models : DFMs                                  #  
###############################################################################

#########################################
# Required packages
#########################################
library(dplyr)
library(tidyverse)
library(dfms)
library(tsibble)
library(xts)
data <- getData("PPI")
install.packages(
  "tsibble"
)

#########################################
# Estimate a DFS
#########################################

ppi <- data$PPI%>%
  filter(geo %in% c("FR"))%>%
  mutate(var = "PPI")%>%
  pivot_wider(names_from =  c(geo, var, nace_r2), values_from = values)

ppi_nace2 <- data$PPI_NACE2%>%
  filter(geo %in% c("FR"))%>%
  mutate(var = "PPI")%>%
  pivot_wider(names_from =  c(geo, var, nace_r2), values_from = values)

ipi <- data$IPI%>%
  filter(geo %in% c("FR"))%>%
  mutate(var = "IPI")%>%
  pivot_wider(names_from =  c(geo, var, nace_r2), values_from = values)

psurvey <- data$PSURVEY%>%
  filter(geo %in% c("FR"))%>%
  mutate(var = "PSURVEY")%>%
  pivot_wider(names_from =  c(geo, var, indic), values_from = values)

DB <- list(ppi, ppi_nace2, ipi, psurvey)%>%
  reduce(full_join, by="time")%>%
  arrange(time)

DB <- xts(as.data.frame(DB[,-1]), order.by=as.Date(DB[,1]%>%pull()))
plot(scale(DB), lwd = 1)

DB_diff = diff(DB)
plot(scale(DB_diff), lwd = 1)

ic = ICr(DB_diff)

plot(ic)
screeplot(ic)

r <- as.double(names(sort(table(ic$r.star),decreasing=TRUE)[1]))
lag <- as.double(names(sort(table(vars::VARselect(ic$F_pca[, 1:5])$selection),decreasing=TRUE)[1]))

## If we want to remove seasonnality it's here

model1 <- DFM(DB_diff, r = r, p = lag)
print(model1)
plot(model1)

dfm_summary <- summary(model1)
print(dfm_summary)

plot(resid(model1, orig.format = TRUE)$`FR_PPI_B-E36`)
plot(fitted(model1, orig.format = TRUE)$`FR_PPI_B-E36`)


xx <- scale(DB)
attr(xx,"scaled:center")
plot(DB$`FR_PPI_B-E36`)
plot(resid(model1, orig.format = TRUE)$`FR_PPI_B-E36`)
plot(fitted(model1, orig.format = TRUE)$`FR_PPI_B-E36`)
DB_diff$`FR_PPI_B-E36` - fitted(model1, orig.format = TRUE)$`FR_PPI_B-E36`

# 12-period ahead DFM forecast
if (is.na(DB["2022-09-01"]$`FR_PPI_B-E36`)) {
  h = 2
}else{
  h = 1
}

fc = predict(model1, h = h)

sum(fc$X_fcst[,"FR_PPI_B-E36"])
DB_diff[,"FR_PPI_B-E36"]

DB[,"FR_PPI_B-E36"]["2022-09-01"] - DB[,"FR_PPI_B-E36"]["2022-08-01"]

# use lubridate to switch among dates 

DB["2020-08-01/2022-08-01","FR_PPI_B-E36"] - lag(DB["2020-08-01/2022-08-01","FR_PPI_B-E36"])
diff(DB["2020-08-01/2022-08-01","FR_PPI_B-E36"],lag=1,differences=1)

pred <- as.double(DB[,"FR_PPI_B-E36"]["2022-08-01"] + sum(fc$X_fcst[,"FR_PPI_B-E36"]))


