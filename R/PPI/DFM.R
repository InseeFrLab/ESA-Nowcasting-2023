###############################################################################
#                  Time series models : DFMs                                  #  
###############################################################################

#########################################
# Required packages
#########################################
library(dplyr)
library(tidyverse)
library(dfms)
library(xts)
library(lubridate)
library(progress)
#data <- getData("PPI")

date_to_pred <- "2022-10-01"

#########################################
# Loop
#########################################
preds_dfm <- tibble(Country=character(),
                    Date=as.POSIXct(NA),
                    value=numeric()
)

pb <- progress_bar$new(
  format = " [:bar] :percent eta: :eta \n",
  total = 26, clear = FALSE)

for (country in setdiff(countries_PPI, "IE")) {
  cat(paste0("Running estimation for ", country, "\n"))
  
  pb$tick()
    
  #########################################
  # Reshaping the data
  #########################################  
  ppi <- data$PPI%>%
    mutate(var = "PPI")%>%
    filter(geo %in% country)%>%
    pivot_wider(names_from =  c(geo, var, nace_r2), values_from = values)
  
  ppi_nace2 <- data$PPI_NACE2%>%
    mutate(var = "PPI")%>%
    filter(geo %in% country)%>%
    pivot_wider(names_from =  c(geo, var, nace_r2), values_from = values)
  
  ipi <- data$IPI%>%
    mutate(var = "IPI")%>%
    filter(geo %in% country)%>%
    pivot_wider(names_from =  c(geo, var, nace_r2), values_from = values)
  
  psurvey <- data$PSURVEY%>%
    mutate(var = "PSURVEY")%>%
    filter(geo %in% country)%>%
    pivot_wider(names_from =  c(geo, var, indic), values_from = values)
  
  DB <- list(ppi, ppi_nace2, ipi, psurvey)%>%
    reduce(full_join, by="time")%>%
    filter(time>as.Date("2000-01-01"))%>% # Max 2004-09 # 2003 ok BG
    arrange(time)
  
  if (last(DB$time) %--% ymd(date_to_pred) %/% months(1) > 1){
    line_to_add <- last(DB$time) %--% ymd(date_to_pred) %/% months(1) - 1
    for (i in 1:line_to_add) {
      cat("No data in", as.character(last(DB$time) %m+% months(1)),"for country ", country, "\n")
      DB <- DB %>%
        add_row(time = last(DB$time) %m+% months(1))
    }
  }
  
  DB <- xts(as.data.frame(DB[,-1]), order.by=as.Date(DB[,1]%>%pull()))
#  plot(scale(DB), lwd = 1)
  
  # We differenciate the series
  DB_diff = diff(DB)
#  plot(scale(DB_diff), lwd = 1)
  
  #########################################
  # Determination of parameters
  #########################################
  
  # We determine the optimal number of factor and lags
  ic = ICr(DB_diff)
#  plot(ic)
#  screeplot(ic)
  r <- as.double(names(sort(table(ic$r.star),decreasing=TRUE)[1]))
  lag <- as.double(names(sort(table(vars::VARselect(ic$F_pca[, 1:r])$selection),decreasing=TRUE)[1]))
  
  #########################################
  # Seasonality removal (TO BE ADD HERE)
  #########################################
  
  #########################################
  # Simulation of DFM
  #########################################
  
  model <- tryCatch({
    DFM(DB_diff, r = r, p = lag)
    
  }
  ,
  error=function(e) {
    cat(paste0("Failed for country ", country, "\n"))
    e
  }
  )
  
  if(inherits(model, "error")) next
  
  #########################################
  # Forecasting
  #########################################
  current_month <- ymd(date_to_pred) %m-% months(1)
  if (is.na(DB[current_month][,paste0(country, "_PPI_B-E36")])) {
    h = 2
  }else{
    h = 1
  }
  
  fc = predict(model, h = h)
  
  #########################################
  # Storing the predictions
  #########################################
  
  last_available_date<- ymd(date_to_pred) %m-% months(h)
  pred <- as.double(DB[,paste0(country, "_PPI_B-E36")][last_available_date] + 
            sum(fc$X_fcst[, paste0(country, "_PPI_B-E36")]))
  
  preds_dfm <- preds_dfm %>%
    add_row(Country=country, Date=as.POSIXct(date_to_pred), value=round(pred,1))
  
}
