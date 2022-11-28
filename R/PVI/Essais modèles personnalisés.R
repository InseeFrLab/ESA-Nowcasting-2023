country <- "CY"
country <- "FR"
country <- "DE"
country <- "IT"
country <- "ES"
country <- "BE"
country <- "PL"
country <- "AT"
country <- "PT"

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
  slice(1:1) %>%
  pull(time)
debut <- c(year(debut), month(debut))
pvi_ts <- ts(data$PVI %>% filter(geo %in% country) %>% pull(values), start = debut, frequency = 12)
dlpvi <- log(pvi_ts) - stats::lag(log(pvi_ts), -1)
# Exogènes
debut <- data$PSURVEY %>%
  filter(geo %in% country) %>%
  filter(indic == "BS-ICI") %>%
  slice(1:1) %>%
  pull(time)
debut <- c(year(debut), month(debut))
IS <- ts(data$PSURVEY %>% filter(geo %in% country) %>% filter(indic == "BS-ICI") %>% pull(values), start = debut, frequency = 12)
debut <- data$PSURVEY %>%
  filter(geo %in% country) %>%
  filter(indic == "BS-IPT") %>%
  slice(1:1) %>%
  pull(time)
debut <- c(year(debut), month(debut))
IPT <- ts(data$PSURVEY %>% filter(geo %in% country) %>% filter(indic == "BS-IPT") %>% pull(values), start = debut, frequency = 12)


var <- ts.union(IPT / 100,
                IS/100,diff(IS)/100,stats::lag(diff(IS),-1)/100)
var <- ts.union(IS / 100,
                diff(IS)/100)
var <- window(var,start=c(2010,1))
var <- ts.union()

pvi_spec <- regarima_spec_tramoseats(
  transform.function = "None",
  estimate.from = "2010-01-01",
  #estimate.to = "2019-12-01",
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
  fcst.horizon = n_forward
)
pvi_regarima <- regarima(dlpvi, pvi_spec)

pvi_regarima
