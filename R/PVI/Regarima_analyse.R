# install.packages("tstools")
# install.packages("RJDemetra")
library(dplyr)
library(astsa)
library(lubridate)
library(tstools)
library(RJDemetra)

data <- getData("PVI")
data$PVI %>% filter(geo %in% "DE")

country <- c("BE")

# Récupération des données
debut <- data$PVI %>%
  filter(geo %in% country) %>%
  slice(1:1) %>%
  pull(time)
debut <- c(year(debut), month(debut))

pvi_ts <- ts(data$PVI %>% filter(geo %in% country) %>% pull(values), start = debut, frequency = 12)
# dpvi <- window(pvi_ts-stats::lag(pvi_ts,-1),start=c(2010,1))
# plot(pvi_ts)
# acf(pvi_ts, na.action = na.pass)
# pacf(pvi_ts, na.action = na.pass)

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

ind2003 <- create_dummy_ts(
  start_basic = c(1990, 1),
  end_basic = c(2023, 1),
  dummy_start = c(2020, 3),
  dummy_end = c(2020, 3),
  sp = T,
  basic_value = 0,
  dummy_value = 1,
  frequency = 12
)
ind2004 <- create_dummy_ts(
  start_basic = c(1990, 1),
  end_basic = c(2023, 1),
  dummy_start = c(2020, 4),
  dummy_end = c(2020, 4),
  sp = T,
  basic_value = 0,
  dummy_value = 1,
  frequency = 12
)
ind2005 <- create_dummy_ts(
  start_basic = c(1990, 1),
  end_basic = c(2023, 1),
  dummy_start = c(2020, 5),
  dummy_end = c(2020, 5),
  sp = T,
  basic_value = 0,
  dummy_value = 1,
  frequency = 12
)
ind2006 <- create_dummy_ts(
  start_basic = c(1990, 1),
  end_basic = c(2023, 1),
  dummy_start = c(2020, 6),
  dummy_end = c(2020, 6),
  sp = T,
  basic_value = 0,
  dummy_value = 1,
  frequency = 12
)
ind2007 <- create_dummy_ts(
  start_basic = c(1990, 1),
  end_basic = c(2023, 1),
  dummy_start = c(2020, 7),
  dummy_end = c(2020, 7),
  sp = T,
  basic_value = 0,
  dummy_value = 1,
  frequency = 12
)

pvi_ts1 <- window(pvi_ts, start = c(2010, 1), end = c(2022, 8))
dpvi <- window(log(pvi_ts) - stats::lag(log(pvi_ts), -1), start = c(2010, 1), end = c(2022, 8))
ind2003 <- window(ind2003, start = c(2010, 1), end = c(2022, 8))
ind2004 <- window(ind2004, start = c(2010, 1), end = c(2022, 8))
ind2005 <- window(ind2005, start = c(2010, 1), end = c(2022, 8))
ind2006 <- window(ind2006, start = c(2010, 1), end = c(2022, 8))
ind2007 <- window(ind2007, start = c(2010, 1), end = c(2022, 8))

IS1 <- window(IS / 100, start = c(2010, 1), end = c(2022, 8))
IS1_1 <- window(stats::lag(IS, -1), start = c(2010, 1), end = c(2022, 8))
dIS1 <- window(diff(IS / 100), start = c(2010, 1), end = c(2022, 8))
IPT1 <- window(IPT / 100, start = c(2010, 1), end = c(2022, 8))
IPT1_1 <- window(stats::lag(IPT / 100, -1), start = c(2010, 1), end = c(2022, 8))
dIPT1 <- window(diff(IPT / 100), start = c(2010, 1), end = c(2022, 8))
dIPT1_1 <- window(diff(stats::lag(IPT, -1) / 100), start = c(2010, 1), end = c(2022, 8))

# sarima(pvi_ts, 0, 1, 1 , details = FALSE)
# sarima(pvi_ts1, 0, 1, 1 , details = FALSE)
# sarima(pvi_ts1, 0, 1, 2 , xreg=cbind(IS1,dIS1,ind2003,ind2004,ind2005,ind2006), details = FALSE)
sarima(dpvi, 0, 0, 1, xreg = cbind(
  IPT1, dIPT1, IS1, dIS1,
  ind2003, ind2004, ind2005, ind2006
), details = FALSE)



pvi_ts19 <- window(pvi_ts, start = c(2010, 1), end = c(2019, 12))
dpvi_19 <- window(dpvi, end = c(2019, 12))
sarima(pvi_ts19, 0, 1, 2, details = FALSE)
sarima(dpvi_19, 0, 1, 2, details = FALSE)
IS19 <- window(IS / 100, start = c(2010, 1), end = c(2019, 12))
IS19_1 <- window(stats::lag(IS, -1), start = c(2010, 1), c(2019, 12))
dIS19 <- window(diff(IS / 100), start = c(2010, 1), c(2019, 12))
IPT19 <- window(IPT / 100, start = c(2010, 1), c(2019, 12))
IPT19_1 <- window(stats::lag(IPT / 100, -1), start = c(2010, 1), c(2019, 12))
dIPT19 <- window(diff(IPT / 100), start = c(2010, 1), c(2019, 12))
dIPT19_1 <- window(diff(stats::lag(IPT, -1) / 100), start = c(2010, 1), end = c(2019, 12))
sarima(dpvi_19, 0, 0, 1, xreg = cbind(dIS19), details = FALSE)

dIPT19 - dIS19

# Essai avec RJDemetra

var <- ts.union(IPT / 100, diff(IPT) / 100, IS / 100, diff(IS) / 100)
# var <- ts.union(IPT/100,diff(IPT)/100)
# var <- ts.union(IPT/100)
dlpvi <- window(log(pvi_ts) - stats::lag(log(pvi_ts), -1), start = c(2010, 1))
dlpvi_19 <- window(dlpvi, end = c(2019, 12))
myspec1 <- regarima_spec_tramoseats(
  spec = "TR3",
  usrdef.varEnabled = TRUE, usrdef.var = var
)
myreg1 <- regarima(pvi_ts, myspec1)

myreg1$forecast

myspec2 <- regarima_spec_tramoseats(
  arima.coefType = "Undefined",
  arima.p = 0,
  arima.q = 1,
  arima.bp = 0,
  arima.bq = 0,
  arima.d = 0,
  arima.bd = 0,
  automdl.enabled = TRUE,
  usrdef.outliersEnabled = TRUE,
  usrdef.outliersType = c(
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO"
  ),
  usrdef.outliersDate = c(
    "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
    "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
    "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01"
  ),
  outlier.enabled = TRUE,
  outlier.ao = TRUE,
  outlier.tc = FALSE,
  outlier.usedefcv = FALSE,
  outlier.cv = 3.5,
  usrdef.varEnabled = TRUE,
  usrdef.var = var,
  fcst.horizon = 10
)
myreg2 <- regarima(dlpvi, myspec2)
myreg2

myreg2$forecast
myreg2$forecast[1]
myreg2$forecast[2]

pvi_ts %>% tail(1) * exp(myreg2$forecast[1]) * exp(myreg2$forecast[2])

myreg3 <- regarima(dlpvi_19, myspec2)
myreg3

# PPI

country <- c("DE")
data$PPI %>% filter(geo %in% country)


# Récupération des données
debut <- data$PPI %>%
  filter(geo %in% country) %>%
  slice(1:1) %>%
  pull(time)
debut <- c(year(debut), month(debut))

ppi <- ts(data$PPI %>% filter(geo %in% country) %>% pull(values), start = debut, frequency = 12)
dlppi <- window(log(ppi) - stats::lag(log(ppi), -1), start = c(2010, 1))

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
brent_1 <- stats::lag(brent, -1)
brent_2 <- stats::lag(brent, -2)

ipi <- data$IPI %>%
  filter(geo %in% country) %>%
  filter(time == ymd("20100101"))
print(ipi, n = 40)

dlbrent <- log(brent) - stats::lag(log(brent), -1)
dlbrent_1 <- stats::lag(dlbrent, -1)
dlbrent_2 <- stats::lag(dlbrent, -2)
var <- ts.union(dlbrent, dlbrent_1, dlbrent_2)

debut <- data$IPI %>%
  filter(geo %in% country) %>%
  slice(1:1) %>%
  pull(time)
debut <- c(year(debut), month(debut))
ipi <- ts(data$IPI %>% filter(geo %in% country & nace_r2 == "B-D") %>%
  pull(values), start = debut, frequency = 12)
dlipi <- log(ipi) - stats::lag(log(ipi), -1)
dlipi_1 <- stats::lag(dlipi, -1)
dlipi_2 <- stats::lag(dlipi, -2)
dlipi_3 <- stats::lag(dlipi, -3)
dlipi_4 <- stats::lag(dlipi, -4)

lcointeg <- stats::lag(log(ppi), -1) - stats::lag(log(ipi), -1)

var <- ts.union(dlbrent, dlipi_1, dlipi_2, dlipi_3, dlipi_4)


ppi_spec <- regarima_spec_tramoseats(
  transform.function = "None",
  estimate.from = "2010-01-01",
  estimate.to = "2019-12-01",
  automdl.enabled = TRUE,
  outlier.enabled = TRUE,
  outlier.ao = TRUE,
  outlier.tc = FALSE,
  outlier.usedefcv = FALSE,
  outlier.cv = 3.5,
  usrdef.varEnabled = TRUE,
  usrdef.var = var,
  fcst.horizon = 2
)
ppi_regarima <- regarima(dlppi, ppi_spec)
ppi_regarima
ls(ppi_regarima)
ppi_regarima$residuals

last(dlipi)

# BS-ISPE

# Modèle en M/M-12
debut <- data$PPI %>%
  filter(geo %in% country) %>%
  slice(1:1) %>%
  pull(time)
debut <- c(year(debut), month(debut))

ppi <- ts(data$PPI %>% filter(geo %in% country) %>% pull(values), start = debut, frequency = 12)
dlppi <- window(log(ppi) - stats::lag(log(ppi), -12), start = c(2010, 1))

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

dlbrent <- log(brent) - stats::lag(log(brent), -12)
dlbrent_1 <- stats::lag(dlbrent, -1)
dlbrent_2 <- stats::lag(dlbrent, -2)

print(data$IPI %>% filter(geo == "DE" & time == ymd("20200101")), n = 36)
debut <- data$IPI %>%
  filter(geo %in% country) %>%
  slice(1:1) %>%
  pull(time)
debut <- c(year(debut), month(debut))
ipi <- ts(data$IPI %>% filter(geo %in% country & nace_r2 == "B-D") %>%
  pull(values), start = debut, frequency = 12)
dlipi <- log(ipi) - stats::lag(log(ipi), -12)
dlipi_1 <- stats::lag(dlipi, -1)
dlipi_2 <- stats::lag(dlipi, -2)
dlipi_3 <- stats::lag(dlipi, -3)
dlipi_4 <- stats::lag(dlipi, -4)

var <- ts.union(dlbrent, dlbrent_1, dlipi_2, dlipi_3, dlipi_4)

ppi_spec <- regarima_spec_tramoseats(
  transform.function = "None",
  estimate.from = "2010-01-01",
  # estimate.to = "2019-12-01",
  automdl.enabled = TRUE,
  outlier.enabled = TRUE,
  outlier.ao = TRUE,
  outlier.tc = FALSE,
  outlier.usedefcv = FALSE,
  outlier.cv = 3.5,
  usrdef.varEnabled = TRUE,
  usrdef.var = var,
  fcst.horizon = 2
)
ppi_regarima <- regarima(dlppi, ppi_spec)
ppi_regarima
ppi_regarima$residuals

# Modèle en niveau
lbrent <- log(brent)
lbrent_1 <- stats::lag(lbrent, -1)
lbrent_2 <- stats::lag(lbrent, -2)
lbrent_3 <- stats::lag(lbrent, -3)
lbrent_4 <- stats::lag(lbrent, -4)
var <- ts.union(lbrent, lbrent_1, lbrent_2, lbrent_3, lbrent_4)
ppi_spec <- regarima_spec_tramoseats(
  estimate.from = "2010-01-01",
  automdl.enabled = TRUE,
  outlier.enabled = TRUE,
  outlier.ao = TRUE,
  outlier.tc = FALSE,
  outlier.usedefcv = FALSE,
  outlier.cv = 3.5,
  usrdef.varEnabled = TRUE,
  usrdef.var = var,
  fcst.horizon = 2
)
ppi_regarima <- regarima(ppi, ppi_spec)
ppi_regarima
