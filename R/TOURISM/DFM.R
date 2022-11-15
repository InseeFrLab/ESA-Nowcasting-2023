###############################################################################
#                  Time series models : DFMs                                  #
###############################################################################

#########################################
# Required packages
#########################################
library(dplyr)
library(purrr)
library(RJDemetra)
library(dfms)
library(xts)
library(lubridate)
library(progress)
library(tsbox)

#########################################
# Loop
#########################################
preds_dfm <- tibble(
  Country = character(),
  Date = as.POSIXct(NA),
  value = numeric()
)

pb <- progress_bar$new(
  format = " [:bar] :percent eta: :eta \n",
  total = length(countries_tourism), clear = FALSE
)

for (country in countries_tourism) {
  cat(paste0("Running estimation for ", country, "\n"))
  pb$tick()
  var_to_predict <- paste0(country, "_TOURISM")
  #########################################
  # Reshaping the data
  #########################################
  tourism <- data$TOURISM %>%
    mutate(var = "TOURISM") %>%
    filter(geo %in% country) %>%
    pivot_wider(names_from = c(geo, var), values_from = values)

  brent <- data$brent %>%
    as_tibble() %>%
    mutate(
      month = month(time),
      year = year(time)
    ) %>%
    group_by(year, month) %>%
    summarise(
      brent_adjusted = mean(brent_adjusted, na.rm = T),
      brent_volume = mean(brent_volume, na.rm = T),
    ) %>%
    ungroup() %>%
    mutate(time = ymd(paste(year, month, "01", sep = "-"))) %>%
    select(time, brent_adjusted, brent_volume)

  eur_usd <- data$eur_usd %>%
    as_tibble() %>%
    select(time, eur_usd_adjusted) %>%
    mutate(
      month = month(time),
      year = year(time)
    ) %>%
    group_by(year, month) %>%
    summarise(
      eur_usd_adjusted = mean(eur_usd_adjusted, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(time = ymd(paste(year, month, "01", sep = "-"))) %>%
    select(time, eur_usd_adjusted)

  DB <- list(tourism, brent, eur_usd) %>%
    purrr::reduce(full_join, by = "time") %>%
    filter(time > as.Date("2000-01-01")) %>% # Max 2004-09 # 2003 ok BG
    arrange(time)

  #########################################
  # Lagging the most recent variables
  #########################################
  DB <- xts(as.data.frame(DB[, -1]), order.by = as.Date(DB[, 1] %>% pull()))
  
  latest_dates <- sapply(names(DB), get_latest_dates, data = DB) 
  
  for (variable in setdiff(names(DB), var_to_predict)) {
    # Check gap in months between Y and Xs 
    gap <- as.Date(latest_dates[[var_to_predict]]) %--%  as.Date(latest_dates[[variable]]) %/% months(1)
    
    # We lag the variable with a window equal to the gap
    if (gap > 0) {
      lagged_var <- stats::lag(DB[,variable],-gap)
      names(lagged_var) <- paste(variable, "LAG", gap, sep = "_")
      DB <-merge(DB, lagged_var)
      }
  }
  
  #########################################
  # Seasonality removal (TO BE ADD HERE)
  #########################################
  sa <- RJDemetra::x13(ts_ts(DB[,var_to_predict]), spec = c("RSA2"))
  sa_xts <- tsbox::ts_xts(sa$final$series[,"sa"])
  names(sa_xts) <- paste0(var_to_predict, "_SA")
  DB <-merge(DB, sa_xts)
  DB <- DB[, !(names(DB) %in% var_to_predict)]
  
  #########################################
  # Differenciate the series
  #########################################
  DB_diff <- diff(DB)
  # restrict the dataset to the last available value of Y
  # We might want to extent the starting value depending on the data that we will use
  DB_diff <- DB_diff[paste0("2007-05-01/",latest_dates[[var_to_predict]])]
  
  #########################################
  # Dealing with multiple NaNs columns
  #########################################
  range_3year <- paste(date_to_pred %m-% months(36 + 4), date_to_pred %m-% months(5), sep = "/")
  nan_cols <- as.double(which(colSums(is.na(DB_diff[range_3year])) > 18))

  if (!is_empty(nan_cols)) {
    cat("Removing", names(which(colSums(is.na(DB_diff[range_3year])) > 18)), "due to missing values.\n")
    DB_diff <- DB_diff[, -nan_cols]
  }

  #########################################
  # Dealing with collinearity
  #########################################

  # Creating a squared matrix to check collinearity
  # Now we could use last() for the interval
  range_square_mat <- paste(date_to_pred %m-% months(dim(DB_diff)[2] + 1), date_to_pred %m-% months(2), sep = "/")
  # diff(dim(DB_diff[range_square_mat]))

  # Get the positions of collinear columns
  positions <- subset(as.data.frame(which(cor(DB_diff[range_square_mat]) > 0.9999, arr.ind = TRUE)), row < col)

  # If necessary, remove collinear columns
  if (dim(positions)[1] > 0) {
    var_to_remove <- sapply(positions, function(x) names(DB_diff[range_square_mat])[x])["col"]
    DB_diff <- DB_diff[, -as.double(positions["col"])]
    cat("Removing", var_to_remove, "due to collinearity.\n")
  }

  #########################################
  # Determination of parameters
  #########################################

  # We determine the optimal number of factor and lags
  ic <- tryCatch(
    {
      ICr(DB_diff)
    },
    error = function(e) {
      cat(paste0("Failed for country ", country, ", too little variables available \n"))
      e
    }
  )

  if (inherits(ic, "error")) next

  r <- as.double(names(sort(table(ic$r.star), decreasing = TRUE)[1]))
  r <- ifelse(r>2, 2, r)
  lag <- as.double(names(sort(table(vars::VARselect(ic$F_pca[, 1:r])$selection), decreasing = TRUE)[1]))

    #########################################
  # Simulation of DFM
  #########################################

  model <- tryCatch(
    {
      DFM(DB_diff, r = r, p = lag)
    },
    error = function(e) {
      cat(paste0("Failed for country ", country, "\n"))
      e
    }
  )

  if (inherits(model, "error")) next

  #########################################
  # Forecasting
  #########################################
  # Define the appropriate forecast horizon
  h <- as.Date(latest_dates[[var_to_predict]]) %--%  date_to_pred %/% months(1)
  # Forecast the model, pay attention to the standardized option
  fc <- predict(model, h = h, standardized = F)

  #########################################
  # Storing the predictions
  #########################################
  pred_sa <- as.double(DB[, paste0(var_to_predict, "_SA")][latest_dates[[var_to_predict]]] +
    sum(fc$X_fcst[, paste0(var_to_predict, "_SA")])) 

  # we add the seasonal component so that we obtain the initial variable
  pred <- pred_sa +  sa$final$forecasts[h, "s_f"] 
  
  preds_dfm <- preds_dfm %>%
    add_row(Country = country, Date = date_to_pred, value = round(pred, 1))
}

#########################################
# Add missing countries in the list
#########################################
missing_countries <- setdiff(countries_tourism, preds_dfm$Country)
for (country in missing_countries) {
  preds_dfm <- preds_dfm %>%
    add_row(Country = country, Date = date_to_pred)
}

# Re-arranging countries
preds_dfm <- preds_dfm %>%
  mutate(Country = factor(Country, levels = countries_tourism)) %>%
  arrange(Country)
