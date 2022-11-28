###############################################################################
#                  Time series models : REGARIMA                              #
###############################################################################

#########################################
# Required packages
#########################################

# install.packages("RJDemetra")
library(dplyr)
library(astsa)
library(lubridate)
library(RJDemetra)
library(xts)

#########################################
# Estimate a REGARIMA
#########################################

preds_regarima <- tibble(
  Country = character(),
  Date = as.POSIXct(NA),
  value = numeric()
)

resid_regarima <- tibble(
  Country = character(),
  Date = as.POSIXct(NA),
  value = numeric()
)

for (country in countries_PVI) {
  current_date_country <- data$PVI %>%
    filter(geo == country) %>%
    select(time) %>%
    arrange() %>%
    tail(1) %>%
    pull()

  n_forward <- interval(current_date_country, date_to_pred) %/% months(1)

  # série cible + vérif date de début
  debut <- data$PVI %>%
    filter(geo %in% country) %>%
    slice_head(n=1) %>%
    pull(time)
  debut <- c(year(debut), month(debut))
  pvi_ts <- ts(data$PVI %>% filter(geo %in% country) %>% pull(values), start = debut, frequency = 12)
  dlpvi <- log(pvi_ts) - stats::lag(log(pvi_ts), -1)
  # Exogènes
  debut <- data$PSURVEY %>%
    filter(geo %in% country) %>%
    filter(indic == "BS-ICI") %>%
    slice_head(n=1) %>%
    pull(time)
  debut <- c(year(debut), month(debut))
  IS <- ts(data$PSURVEY %>% filter(geo %in% country) %>% filter(indic == "BS-ICI") %>% pull(values), start = debut, frequency = 12)
  debut <- data$PSURVEY %>%
    filter(geo %in% country) %>%
    filter(indic == "BS-IPT") %>%
    slice_head(n=1) %>%
    pull(time)
  debut <- c(year(debut), month(debut))
  IPT <- ts(data$PSURVEY %>% filter(geo %in% country) %>% filter(indic == "BS-IPT") %>% pull(values), start = debut, frequency = 12)
  IPT <- IPT/100
  dIPT <- diff(IPT)
  dIPT2 <- stats::lag(dIPT,-1)
  IS <- IS/100
  dIS <- diff(IS)
  dIS2 <- stats::lag(dIS,-1)
  
  var <- ts.union(IPT / 100, diff(IPT) / 100)
  
  #Traitements particuliers pays sur les exogènes
  if (country %in% c('DE'))
  {var <- ts.union(IPT , dIPT , dIS)}
  if (country %in% c('IT'))
  {var <- ts.union(IS, dIPT, dIPT2)}
  if (country %in% c('ES'))
  {var <- ts.union(IPT ,dIS)}
  if (country %in% c('FR'))
  {var <- ts.union(IPT ,  dIPT, IS, dIS)}
  if (country %in% c('PL'))
  {var <- ts.union()}
  if (country %in% c('AT','PT'))
  {var <- ts.union(IPT ,
                   IS,dIS,dIS2)}
  
  #if (last(data$PSURVEY %>% filter(geo %in% country) %>% filter(indic == "BS-IPT") %>% pull(time))<ymd(date_to_pred))
  #{var <- ts.union()}

  pvi_spec <- regarima_spec_tramoseats(
    transform.function = "None",
    estimate.from = "2010-01-01",
    automdl.enabled = TRUE,
    usrdef.outliersEnabled = TRUE,
    usrdef.outliersType = c(
       "AO", "AO", "AO", "AO", "AO",
      "AO", "AO", "AO", "AO", "AO", "AO",
      "AO", "AO", "AO", "AO", "AO", "AO"
    ),
    usrdef.outliersDate = c(
       "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
      "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
      "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01"
    ),
    outlier.enabled = TRUE,
    outlier.ao = TRUE,
    outlier.tc = FALSE,
    outlier.ls = TRUE,
    outlier.usedefcv = FALSE,
    outlier.cv = 3.5,
    usrdef.varEnabled = TRUE,
    usrdef.var = var,
    fcst.horizon = n_forward
  )
  pvi_regarima <- regarima(dlpvi, pvi_spec)

  # Deux cas : pred à 1 ou 2 horizons
  if (n_forward == 1) {
    pred <- pvi_ts %>% tail(1) * exp(pvi_regarima$forecast[1])
  }
  if (n_forward == 2) {
    pred <- pvi_ts %>% tail(1) * exp(pvi_regarima$forecast[1]) * exp(pvi_regarima$forecast[2])
  }
  if (n_forward == 3) {
    pred <- pvi_ts %>% tail(1) * exp(pvi_regarima$forecast[1]) * exp(pvi_regarima$forecast[2]) * exp(pvi_regarima$forecast[3])
  }

  #Si valeur manquante, on supprime les exogènes du modèle
  if (is.na(pred))
  {
    pvi_spec <- regarima_spec_tramoseats(
      transform.function = "None",
      estimate.from = "2010-01-01",
      automdl.enabled = TRUE,
      usrdef.outliersEnabled = TRUE,
      usrdef.outliersType = c(
         "AO", "AO", "AO", "AO", "AO",
        "AO", "AO", "AO", "AO", "AO", "AO",
        "AO", "AO", "AO", "AO", "AO", "AO"
      ),
      usrdef.outliersDate = c(
         "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
        "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
        "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01"
      ),
      outlier.enabled = TRUE,
      outlier.ao = TRUE,
      outlier.tc = FALSE,
      outlier.ls = TRUE,
      outlier.usedefcv = FALSE,
      outlier.cv = 3.5,
      fcst.horizon = n_forward
    )
    pvi_regarima <- regarima(dlpvi, pvi_spec)
    
    # Deux cas : pred à 1 ou 2 horizons
    if (n_forward == 1) {
      pred <- pvi_ts %>% tail(1) * exp(pvi_regarima$forecast[1])
    }
    if (n_forward == 2) {
      pred <- pvi_ts %>% tail(1) * exp(pvi_regarima$forecast[1]) * exp(pvi_regarima$forecast[2])
    }
    if (n_forward == 3) {
      pred <- pvi_ts %>% tail(1) * exp(pvi_regarima$forecast[1]) * exp(pvi_regarima$forecast[2]) * exp(pvi_regarima$forecast[3])
    }
  }
  
  #Sécurisation en prenant modèle simple si problème de valeurs manquantes plus haut avec la procédure Regarima
  if (is.na(pred))
  {
    pred <- sarima.for(
      data$PVI %>%
        filter(geo == country) %>%
        pull(values),
      n_forward,
      0, 1, 1,
      plot = FALSE
    )$pred %>%
      tail(1)
    print(paste0("attention, prediction dégradée sur ",country))
  }
  
  preds_regarima <- preds_regarima %>%
    add_row(
      Country = country,
      Date = date_to_pred,
      value = round(as.numeric(pred), 1)
    )
  
}
