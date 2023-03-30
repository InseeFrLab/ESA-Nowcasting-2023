country <- "AT"
challenge <- "PVI"

dy <- build_data_regarima(challenge, challenges, data, country)$y

IS <- reshape_eurostat_data(data, country) %>%
  select(time, paste(country, "PSURVEY", "BS-ICI", sep = "_")) %>%
  tidyr::drop_na() %>%
  tsbox::ts_ts() / 100

IPT <- reshape_eurostat_data(data, country) %>%
  select(time, paste(country, "PSURVEY", "BS-IPT", sep = "_")) %>%
  tidyr::drop_na() %>%
  tsbox::ts_ts() / 100

dIPT <- diff(IPT)
# dIPT2 <- stats::lag(dIPT, -1)
dIS <- diff(IS)
# dIS2 <- stats::lag(dIS, -1)

ind_wifo <- wifo  %>% 
  tidyr::drop_na() %>% 
  mutate(time=ymd(paste(year(time),month(time),"01"))) %>% 
  group_by(time) %>% 
  summarize(wifo_ind=mean(wifo_ind)) %>% 
  ungroup() %>% 
  tsbox::ts_ts() 

dind_wifo=ind-stats::lag(ind,-1)
var <- ts.union(IPT,dind)
var <- ts.union()

tourism_spec <- regarima_spec_tramoseats(
  spec="TRfull",
  transform.function = "Auto",
  estimate.from = "2021-06-01",
#  estimate.to = "2023-01-01",
  automdl.enabled = TRUE,
  usrdef.outliersEnabled = TRUE,
  tradingdays.option = "TradingDays",
  outlier.enabled = TRUE,
  outlier.ao = TRUE,
  outlier.tc = FALSE,
  outlier.ls = TRUE,
  outlier.usedefcv = FALSE,
  outlier.cv = 4,
  usrdef.varEnabled = TRUE,
  usrdef.var = var,
  fcst.horizon = 2
)
regarima <- regarima(dy, tourism_spec)
regarima
