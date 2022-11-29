country <- "EE"

debut <- data$PPI %>%
  filter(geo %in% country) %>%
  slice(1:1) %>%
  pull(time)
debut <- c(year(debut), month(debut))

fin <- data$PPI %>%
  filter(geo %in% country) %>%
  last() %>%
  pull(time)

n_forward <- lubridate::interval(fin, date_to_pred) %/% months(1)

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

brent <- brent / eur_usd

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

# Liste de variables exogènes minimum
var <- ts.union(dlbrent, dlbrent_1)

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
  ecart_dernier_mois <- lubridate::interval(date_to_pred, data$IPI %>% filter(geo %in% country & cpa2_1 == "CPA_B-D") %>% last() %>% pull(time)) %/% months(1)


  if (ecart_dernier_mois == -1) {
    var <- ts.union(dlbrent, dlipi_1, dlipi_2, dlipi_3, dlipi_4)
  }
  if (ecart_dernier_mois == -2) {
    var <- ts.union(dlbrent, dlbrent_1, dlipi_2, dlipi_3, dlipi_4)
  }
}

# ipi_ing <- ts(data$IPI %>% filter(geo %in% country & cpa2_1 == "CPA_MIG_ING") %>%
#                 pull(values), start = debut, frequency = 12)
# dlipi_ing <- log(ipi_ing) - stats::lag(log(ipi_ing), -1)
# dlipi_ing_1 <- stats::lag(dlipi_ing, -1)
# dlipi_ing_2 <- stats::lag(dlipi_ing, -2)
# dlipi_ing_3 <- stats::lag(dlipi_ing, -3)
# dlipi_ing_4 <- stats::lag(dlipi_ing, -4)
#
# ipi_nrg <- ts(data$IPI %>% filter(geo %in% country & cpa2_1 == "CPA_MIG_NRG_X_E") %>%
#                 pull(values), start = debut, frequency = 12)
# dlipi_nrg <- log(ipi_nrg) - stats::lag(log(ipi_nrg), -1)
# dlipi_nrg_1 <- stats::lag(dlipi_nrg, -1)
# dlipi_nrg_2 <- stats::lag(dlipi_nrg, -2)
# dlipi_nrg_3 <- stats::lag(dlipi_nrg, -3)
# dlipi_nrg_4 <- stats::lag(dlipi_nrg, -4)

var <- ts.union(
  dlbrent, dlbrent_1, dlbrent_2,
  dlipi_2, dlipi_3, dlipi_4
)

ppi_spec <- regarima_spec_tramoseats(
  transform.function = "None",
  estimate.from = "2012-01-01",
  # estimate.to = "2019-12-01",
  automdl.enabled = TRUE,
  outlier.enabled = TRUE,
  outlier.ao = TRUE,
  outlier.tc = FALSE,
  outlier.usedefcv = FALSE,
  outlier.cv = 3.5,
  usrdef.varEnabled = TRUE,
  # usrdef.var = var,
  fcst.horizon = n_forward
)

regarima(dlppi, ppi_spec)$forecast

essai <- function() {
  print("essai")
}

possibleError <- tryCatch(regarima(dlppi, ppi_spec), error = function(e) e)

if (!inherits(possibleError, "error")) {
  print("essai")
}

if (inherits(possibleError, "error")) {
  print("essai")
}
