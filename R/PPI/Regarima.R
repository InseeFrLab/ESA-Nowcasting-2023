###############################################################################
#                  Time series models : REGARIMA                              #
###############################################################################

#########################################
# Required packages
#########################################

library(dplyr)
library(astsa)
library(lubridate)
library(RJDemetra)

#########################################
# Estimate a REGARIMA
#########################################

preds_regarima <- tibble(
  Country = character(),
  Date = as.POSIXct(NA),
  value = numeric()
)

for (country in countries_PPI) {
  # série cible + vérif date de début et de fin
  
  #print(country)
  
  debut <- data$PPI %>%
    filter(geo %in% country) %>%
    slice(1:1) %>%
    pull(time)
  debut <- c(year(debut), month(debut))

  fin <- data$PPI %>%
    filter(geo %in% country) %>%
    last() %>%
    pull(time)

  n_forward <- interval(fin, date_to_pred) %/% months(1)

  ppi <- ts(data$PPI %>% filter(geo %in% country) %>% pull(values), start = debut, frequency = 12)
  dlppi <- window(log(ppi) - stats::lag(log(ppi), -1), start = c(2010, 1))

  # Exogène 1 : pétrole avec moyenne des jours présents sur le dernier mois
  brent <- data$brent %>%
    mutate(time = floor_date(time, unit = "month")) %>%
    group_by(time) %>%
    summarize(brent_adjusted = mean(brent_adjusted, na.rm = TRUE)) %>%
    select(c(time, brent_adjusted))
  debut <- brent %>%
    slice(1:1) %>%
    pull(time)
  debut <- c(year(debut), month(debut))
  brent <- ts(brent %>% pull(brent_adjusted), start = debut, frequency = 12)
  
  #Correction du brent par le taux de change
  eur_usd <- data$eur_usd %>%
    mutate(time = floor_date(time, unit = "month")) %>%
    group_by(time) %>%
    summarize(eur_usd_adjusted = mean(eur_usd_adjusted, na.rm = TRUE)) %>%
    select(c(time, eur_usd_adjusted))
  debut <- eur_usd %>%
    slice(1:1) %>%
    pull(time)
  debut <- c(year(debut), month(debut))
  eur_usd <- ts(eur_usd %>% pull(eur_usd_adjusted), start = debut, frequency = 12)
  
  brent <- brent/eur_usd
  
  brent_1 <- stats::lag(brent, -1)
  brent_2 <- stats::lag(brent, -2)

  dlbrent <- log(brent) - stats::lag(log(brent), -1)
  dlbrent_1 <- stats::lag(dlbrent, -1)
  dlbrent_2 <- stats::lag(dlbrent, -2)
  var <- ts.union(dlbrent, dlbrent_1, dlbrent_2)

  # Autres exogènes : prix d'importation industrie + retards
  debut <- data$IPI %>%
    filter(geo %in% country) %>%
    slice(1:1) %>%
    pull(time)
  debut <- c(year(debut), month(debut))
  
  #Liste de variables exogènes minimum
  var <- ts.union(dlbrent, dlbrent_1, dlbrent_2)
  
  dispo <- length(data$IPI %>% filter(geo %in% country & cpa2_1 == "CPA_B-D") %>%
                    pull(values)) > 0
  if (dispo) {
    ipi <- ts(data$IPI %>% filter(geo %in% country & cpa2_1 == "CPA_B-D") %>%
                pull(values), start = debut, frequency = 12)
    dlipi <- log(ipi) - stats::lag(log(ipi), -1)
    dlipi_1 <- stats::lag(dlipi, -1)
    dlipi_2 <- stats::lag(dlipi, -2)
    dlipi_3 <- stats::lag(dlipi, -3)
    dlipi_4 <- stats::lag(dlipi, -4)
    # différence éventuelle entre dernière date ppi et dernière date prix d'imports
    ecart_dernier_mois <- interval(date_to_pred, data$IPI %>% filter(geo %in% country & cpa2_1 == "CPA_B-D") %>% last() %>% pull(time)) %/% months(1)
    
    
    if (ecart_dernier_mois == -1) {
      var <- ts.union(dlbrent, dlipi_1, dlipi_2, dlipi_3, dlipi_4)
    }
    if (ecart_dernier_mois == -2) {
      var <- ts.union(dlbrent, dlbrent_1, dlipi_2, dlipi_3, dlipi_4)
    }
    if (ecart_dernier_mois == -3) {
      var <- ts.union(dlbrent, dlbrent_1, dlbrent_2, dlipi_3, dlipi_4)
    }
  }

  debut_estim <- "2010-01-01"
  #Gestion à la main des pays avec profondeur de données limitée
  if (country %in% c("EE")) {debut_estim <- "2012-01-01"}
  if (country %in% c("LT")) {debut_estim <- "2011-01-01"}
  if (country %in% c("LV")) {debut_estim <- "2015-01-01"}
  
  ppi_spec <- regarima_spec_tramoseats(
    transform.function = "None",
    estimate.from = debut_estim,
    estimate.to = "2021-12-01",
    automdl.enabled = TRUE,
    outlier.enabled = TRUE,
    outlier.ao = TRUE,
    outlier.tc = FALSE,
    outlier.usedefcv = FALSE,
    outlier.cv = 3.5,
    usrdef.varEnabled = TRUE,
    usrdef.var = var,
    fcst.horizon = n_forward
  )
  ppi_regarima <- regarima(dlppi, ppi_spec)

  # Deux cas : pred à 1 ou 2 horizons
  if (n_forward == 1) {
    pred <- ppi %>% tail(1) * exp(ppi_regarima$forecast[1])
  }
  if (n_forward == 2) {
    pred <- ppi %>% tail(1) * exp(ppi_regarima$forecast[1]) * exp(ppi_regarima$forecast[2])
  }
  if (n_forward == 3) {
    pred <- ppi %>% tail(1) * exp(ppi_regarima$forecast[1]) * exp(ppi_regarima$forecast[2]) * exp(ppi_regarima$forecast[3])
  }

  if (is.na(pred))
  {
    ppi_spec <- regarima_spec_tramoseats(
      transform.function = "None",
      estimate.from = debut_estim,
      estimate.to = "2021-12-01",
      automdl.enabled = TRUE,
      outlier.enabled = TRUE,
      outlier.ao = TRUE,
      outlier.tc = FALSE,
      outlier.usedefcv = FALSE,
      outlier.cv = 3.5,
      fcst.horizon = n_forward
    )
    ppi_regarima <- regarima(dlppi, ppi_spec)
    
    if (n_forward == 1) {
      pred <- ppi %>% tail(1) * exp(ppi_regarima$forecast[1])
    }
    if (n_forward == 2) {
      pred <- ppi %>% tail(1) * exp(ppi_regarima$forecast[1]) * exp(ppi_regarima$forecast[2])
    }
    if (n_forward == 3) {
      pred <- ppi %>% tail(1) * exp(ppi_regarima$forecast[1]) * exp(ppi_regarima$forecast[2]) * exp(ppi_regarima$forecast[3])
    }
  }
  
  preds_regarima <- preds_regarima %>%
    add_row(
      Country = country,
      Date = date_to_pred,
      value = round(as.numeric(pred), 1)
    )
}
