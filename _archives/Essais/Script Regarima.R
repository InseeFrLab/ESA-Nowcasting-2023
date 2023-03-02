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
library(xts)
library(tsbox)

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

country <- "DE"

#for (country in countries_PVI) {

    current_date_country <- data$PVI$data %>%
    filter(geo == country) %>%
    select(time) %>%
    arrange() %>%
    tail(1) %>%
    pull()
  
  #n_forward <- lubridate::interval(current_date_country, date_to_pred) %/% months(1)
  
  # série cible + vérif date de début
  debut <- data$PVI$data %>%
    filter(geo %in% country) %>%
    dplyr::slice_head(n = 1) %>%
    pull(time)
  debut <- c(year(debut), month(debut))
  pvi_ts <- ts(data$PVI$data %>% filter(geo %in% country) %>% pull(values), start = debut, frequency = 12)
  dlpvi <- log(pvi_ts) - stats::lag(log(pvi_ts), -1)
  # Exogènes
  debut <- data$PSURVEY$data %>%
    filter(geo %in% country) %>%
    filter(indic == "BS-ICI") %>%
    dplyr::slice_head(n = 1) %>%
    pull(time)
  debut <- c(year(debut), month(debut))
  IS <- ts(data$PSURVEY$data %>% filter(geo %in% country) %>% filter(indic == "BS-ICI") %>% pull(values), start = debut, frequency = 12)
  debut <- data$PSURVEY$data %>%
    filter(geo %in% country) %>%
    filter(indic == "BS-IPT") %>%
    dplyr::slice_head(n = 1) %>%
    pull(time)
  debut <- c(year(debut), month(debut))
  IPT <- ts(data$PSURVEY$data %>% filter(geo %in% country) %>% filter(indic == "BS-IPT") %>% pull(values), start = debut, frequency = 12)
  IPT <- IPT / 100
  dIPT <- diff(IPT)
  dIPT2 <- stats::lag(dIPT, -1)
  IS <- IS / 100
  dIS <- diff(IS)
  dIS2 <- stats::lag(dIS, -1)
  
  var <- ts.union(IPT / 100, diff(IPT) / 100)
  
  # Traitements particuliers pays sur les exogènes
  if (country %in% c("DE")) {
    var <- ts.union(IPT, dIPT, dIS)
  }
  if (country %in% c("IT")) {
    var <- ts.union(IS, dIPT, dIPT2)
  }
  if (country %in% c("ES")) {
    var <- ts.union(IPT, dIS)
  }
  if (country %in% c("FR")) {
    var <- ts.union(IPT, dIPT, IS, dIS)
  }
  if (country %in% c("PL")) {
    var <- ts.union()
  }
  if (country %in% c("AT", "PT")) {
    var <- ts.union(
      IPT,
      IS, dIS, dIS2
    )
  }
  
  essai <- read.csv2("truck_toll_mileage.csv")
  
  toll <- essai %>% mutate(mois=substr(dates,4,5),an=substr(dates,7,10)) %>% 
    group_by(an,mois) %>% 
    summarize(toll=mean(toll_sa)) %>% 
    ungroup()
  
  toll2 <- essai %>% mutate(mois=substr(dates,4,5),an=substr(dates,7,10),jour=substr(dates,1,2)) %>% 
    filter(jour<=18) %>% 
    group_by(an,mois) %>% 
    summarize(toll=mean(toll_avg)) %>% 
    ungroup()
  
  tolls <- ts(toll$toll, start=c(2015,1),end=c(2023,1),frequency=12)
  dtolls=diff(tolls)
  
  #Commandes
  new_order0 <- read.csv2("auftragseingang_verarbeitendes_gewerbe_insgesamt_x13.csv",skip=2,
                         header=FALSE) %>% 
    select(c(1,4)) %>% 
    rename(dates0=V1,new_order=V4) %>% 
    mutate(dates=ymd(paste0(substr(dates0,7,10),substr(dates0,4,5),substr(dates0,1,2))))

  new_order <- ts(new_order0$new_order,start=c(1993,3),frequency=12)
  dnew_order <- new_order-stats::lag(new_order,-1)
  dnew_order_1 <- stats::lag(dnew_order,-1)
  dnew_order_2 <- stats::lag(new_order,-2)
  dnew_order_3 <- stats::lag(new_order,-2)
  
  var <- ts.union(window(dIS,start=c(2015,2),end=c(2022,12)),
                  window(dtolls,start=c(2015,2),end=c(2022,12))
                  )
  
  
  var <- ts.union(window(dIPT,start=c(2021,1),end=c(2022,12)),
                  window(dtolls,start=c(2021,1),end=c(2022,12))
                  )
  
  pvi_spec <- regarima_spec_tramoseats(
    transform.function = "None",
    estimate.from = "2015-02-01",
    automdl.enabled = TRUE,
    usrdef.outliersEnabled = TRUE,
     usrdef.outliersType = c(
       "AO", "AO", "AO", "AO", "AO",
       "AO", "AO", "AO", "AO", "AO", "AO",
       "AO", "AO", "AO", "AO", "AO"
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
    fcst.horizon = 2
  )
  pvi_regarima <- regarima(dlpvi, pvi_spec)
  
  arima(x=window(dlpvi,start=c(2019,2),end=c(2022,12)),order=c(1,0,0),
        xreg=ts.union(window(dtolls,start=c(2019,2),end=c(2022,12)),
          window(IPT,start=c(2019,2),end=c(2022,12)),
                      window(dIPT,start=c(2019,2),end=c(2022,12)),
                      window(dIS,start=c(2019,2),end=c(2022,12)))
  )
  ls(pvi_regarima)
  dlpvi-pvi_regarima$residuals
  
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
  
  
  
  preds_regarima <- preds_regarima %>%
    add_row(
      Country = country,
      Date = date_to_pred,
      value = round(as.numeric(pred), 1)
    )
  
  resid_regarima <- rbind(
    resid_regarima,
    tsbox::ts_xts(pvi_ts - exp(dlpvi - resid(pvi_regarima)) * (stats::lag(pvi_ts, -1))) %>%
      as_tibble() %>%
      mutate(
        Date = zoo::index(tsbox::ts_xts(resid(pvi_regarima))),
        Country = country
      ) %>%
      select(Country, Date, value)
  )

