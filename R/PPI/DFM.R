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

#########################################
# Loop
#########################################
preds_dfm <- tibble(Country=character(),
                    Date=as.POSIXct(NA),
                    value=numeric()
)

pb <- progress_bar$new(
  format = " [:bar] :percent eta: :eta \n",
  total = length(setdiff(countries_PPI, "IE")), clear = FALSE)

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
  
  pvi <- data$PVI%>%
    mutate(var = "PVI")%>%
    filter(geo %in% country)%>%
    pivot_wider(names_from =  c(geo, var), values_from = values)
  
  brent <- data$brent%>%
    as_tibble()%>%
    mutate(month = month(time), 
           year = year(time))%>%
    group_by(year, month)%>%
    summarise(
      brent_adjusted = mean(brent_adjusted,na.rm=T),
      brent_volume = mean(brent_volume,na.rm=T),
    )%>%
    ungroup()%>%
    mutate(time = ymd(paste(year, month,"01", sep="-")))%>%
    select(time, brent_adjusted, brent_volume)
  
  eur_usd <- data$eur_usd%>%
    as_tibble()%>%
    select(time, eur_usd_adjusted)%>%
    mutate(month = month(time), 
           year = year(time))%>%
    group_by(year, month)%>%
    summarise(
      eur_usd_adjusted = mean(eur_usd_adjusted,na.rm=T)
    )%>%
    ungroup()%>%
    mutate(time = ymd(paste(year, month,"01", sep="-")))%>%
    select(time, eur_usd_adjusted)
    
  sp500 <- data$sp500%>%
    as_tibble()%>%
    mutate(month = month(time), 
           year = year(time))%>%
    group_by(year, month)%>%
    summarise(
      sp500_adjusted = mean(sp500_adjusted,na.rm=T),
      sp500_volume = mean(sp500_volume,na.rm=T),
    )%>%
    ungroup()%>%
    mutate(time = ymd(paste(year, month,"01", sep="-")))%>%
    select(time, sp500_adjusted, sp500_volume)
  
  eurostoxx500 <- data$eurostoxx500%>%
    as_tibble()%>%
    select(time, eurostoxx500_adjusted)%>%
    mutate(month = month(time), 
           year = year(time))%>%
    group_by(year, month)%>%
    summarise(
      eurostoxx500_adjusted = mean(eurostoxx500_adjusted,na.rm=T)
    )%>%
    ungroup()%>%
    mutate(time = ymd(paste(year, month,"01", sep="-")))%>%
    select(time, eurostoxx500_adjusted)
  
  cac40 <- data$cac40%>%
    as_tibble()%>%
    mutate(month = month(time), 
           year = year(time))%>%
    group_by(year, month)%>%
    summarise(
      cac40_adjusted = mean(cac40_adjusted,na.rm=T),
      cac40_volume = mean(cac40_volume,na.rm=T),
    )%>%
    ungroup()%>%
    mutate(time = ymd(paste(year, month,"01", sep="-")))%>%
    select(time, cac40_adjusted, cac40_volume)
    
  DB <- list(ppi, ppi_nace2, ipi, psurvey, brent, eur_usd, sp500, eurostoxx500, cac40)%>%
    reduce(full_join, by="time")%>%
    filter(time>as.Date("2000-01-01"))%>% # Max 2004-09 # 2003 ok BG
    arrange(time)
  
  if (last(DB$time) %--% date_to_pred %/% months(1) > 1){
    line_to_add <- last(DB$time) %--% date_to_pred %/% months(1) - 1
    for (i in 1:line_to_add) {
      cat("No data in", as.character(last(DB$time) %m+% months(1)),"for country", country, "\n")
      DB <- DB %>%
        add_row(time = last(DB$time) %m+% months(1))
    }
  }
  
  DB <- xts(as.data.frame(DB[,-1]), order.by=as.Date(DB[,1]%>%pull()))

  # We differenciate the series
  DB_diff = diff(DB)

  #########################################
  # Dealing with multiple NaNs columns
  #########################################
  range_3year <- paste(date_to_pred %m-% months(36+1), date_to_pred %m-% months(2), sep="/")
  nan_cols <- as.double(which(colSums(is.na(DB_diff[range_3year])) > 0))

  if (!is_empty(nan_cols)) {
    cat("Removing", names(which(colSums(is.na(DB_diff[range_3year])) > 0)), "due to missing values.\n")
    DB_diff <- DB_diff[,-nan_cols]
  }
  
  #########################################
  # Dealing with collinearity
  #########################################
  
  # Creating a squared matrix to check collinearity
  range_square_mat <- paste(date_to_pred %m-% months(dim(DB_diff)[2]+1), date_to_pred %m-% months(2), sep="/")
  #diff(dim(DB_diff[range_square_mat]))

  # Get the positions of collinear columns
  positions <- subset(as.data.frame(which(cor(DB_diff[range_square_mat]) > 0.9999, arr.ind=TRUE)), row < col)
  
  # If necessary, remove collinear columns
  if (dim(positions)[1] > 0) {
    var_to_remove <- sapply(positions, function(x) names(DB_diff[range_square_mat])[x])["col"]
    DB_diff <- DB_diff[,-as.double(positions["col"])]
    cat("Removing", var_to_remove, "due to collinearity.\n")
  }
  
  #########################################
  # Determination of parameters
  #########################################
  
  # We determine the optimal number of factor and lags
  ic <- tryCatch({
    ICr(DB_diff)
    
  }
  ,
  error=function(e) {
    cat(paste0("Failed for country ", country, ", too little variables available \n"))
    e
  }
  )
  
  if(inherits(ic, "error")) next

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
  current_month <- date_to_pred %m-% months(1)
  if (is.na(DB[current_month][,paste0(country, "_PPI_B-E36")])) {
    h = 2
  }else{
    h = 1
  }
  
  fc = predict(model, h = h)
  
  #########################################
  # Storing the predictions
  #########################################
  
  last_available_date<- date_to_pred %m-% months(h)
  pred <- as.double(DB[,paste0(country, "_PPI_B-E36")][last_available_date] + 
            sum(fc$X_fcst[, paste0(country, "_PPI_B-E36")]))
  
  preds_dfm <- preds_dfm %>%
    add_row(Country=country, Date=date_to_pred, value=round(pred,1))
  
}

#########################################
# Add missing countries in the list
#########################################
missing_countries <- setdiff(countries_PPI, preds_dfm$Country)
for (country in missing_countries) {
  preds_dfm <- preds_dfm %>%
    add_row(Country = country, Date = date_to_pred)
}

# Re-arranging countries
preds_dfm <- preds_dfm%>%
  mutate(Country = factor(Country, levels = countries_PPI))%>%
  arrange(Country)
