#LT"

country <- "AT"
# challenge <- "PVI"
# challenges <- challenges_info

#create_regressors(challenge, challenges, data, country)

elements <- build_data_regarima(challenge, challenges, data, country)
y <- elements$Historical
# gtrenda<- elements$X[,"gtrenda"]
# gtrendv<- elements$X[,"gtrendv"]
# gtrendh<- elements$X[,"gtrendh"]
# ICM<- elements$X[,"ICM_"]

# nb_we <- data$NB_WE %>%
#   tsbox::ts_ts()

ICM <- reshape_eurostat_data(data, country) %>%
  select(time, paste(country, "CSURVEY", "BS-CSMCI", sep = "_")) %>%
  tidyr::drop_na() %>%
  tsbox::ts_ts() / 100
gtrendv <- reshape_gtrends_data(data, country) %>%
  select(time, 
         paste(country, "GTRENDS", "VACATIONS", sep = "_")
  ) %>%
  tidyr::drop_na() %>%
  tsbox::ts_ts() / 100
gtrenda <- reshape_gtrends_data(data, country) %>%
  select(time, 
         paste(country, "GTRENDS", "TRAVEL_AGENCIES", sep = "_")
  ) %>%
  tidyr::drop_na() %>%
  tsbox::ts_ts() / 100
gtrendh <- reshape_gtrends_data(data, country) %>%
  select(time, 
         paste(country, "GTRENDS", "HOTELS", sep = "_")
  ) %>%
  tidyr::drop_na() %>%
  tsbox::ts_ts() / 100

#targets::tar_load(challenges)
#targets::tar_load(data_info)

#plot(log(y))

#y <- window(y,start=c(2015,1))

ma_spec <- x13_spec(
  preliminary.check=FALSE,
#  spec = "RSA5c",
 transform.function = "Auto",
  estimate.from = "2012-01-01",
  automdl.enabled = TRUE,
  tradingdays.autoadjust = TRUE,
  tradingdays.option = "TradingDays",
  tradingdays.test = "None",
  easter.enabled=TRUE,
  easter.test="None",
  usrdef.outliersEnabled = TRUE,
  usrdef.outliersType = c(
    "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO"
  ),
  usrdef.outliersDate = c(
    "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
    "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
    "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01",
    "2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01",
    "2022-01-01", "2022-02-01"
  ),
  outlier.enabled = TRUE,
  outlier.ao = TRUE,
  outlier.tc = TRUE,
  outlier.ls = TRUE,
  outlier.cv=4,
  # usrdef.varEnabled = TRUE,
  # usrdef.var = var,
)

result <- x13(window(y,start=c(2012,1)),ma_spec)
# ls(result$final)
# coef <- window(
#   result$final$forecasts[,"sa_f"]/result$final$forecasts[,"y_f"],
#   start=c(year(date_to_pred),month(date_to_pred)),
#   end=c(year(date_to_pred),month(date_to_pred)))

plot(window(result$final$series[,"sa"],start=c(2022,4)))

#y_sa <- x13(window(y,start=c(2015,1)),ma_spec)$final$series[,"sa"]
ICM_sa <- desaiso(ICM)$final$series[,"sa"]
# ICM_sa <- reshape_eurostat_data(data, country) %>%
#   select(time, paste(country, "CSURVEY_SA", "BS-CSMCI", sep = "_")) %>%
#   tidyr::drop_na() %>%
#   tsbox::ts_ts() / 100
#gtrendh_sa <- x13(window(gtrendh,start=c(2015,1)),ma_spec)$final$series[,"sa"]
gtrendh_sa <- desaiso(gtrendh)$final$series[,"sa"]
gtrendv_sa <- x13(window(gtrendv,start=c(2015,1)),ma_spec)$final$series[,"sa"]
gtrenda_sa <- x13(window(gtrenda,start=c(2015,1)),ma_spec)$final$series[,"sa"]
dy_sa=log(y_sa)-log(stats::lag(y_sa,-1))
dICM_sa=ICM_sa-stats::lag(ICM_sa,-1)
dICM_sa_1=stats::lag(dICM_sa,-1)
dICM_sa_2=stats::lag(dICM_sa,-2)
dgtrendh_sa=gtrendh_sa-stats::lag(gtrendh_sa,-1)
dgtrendv_sa=gtrendv_sa-stats::lag(gtrendv_sa,-1)
dgtrendv_sa_1=stats::lag(dgtrendv_sa,-1)
dgtrenda_sa=gtrenda_sa-stats::lag(gtrenda_sa,-1)
dgtrendh_sa_1=stats::lag(dgtrendh_sa,-1)
dgtrendh_sa_2=stats::lag(dgtrendh_sa,-2)
dgtrenda_sa_1=stats::lag(dgtrenda_sa,-1)
dgtrendh_sa2 = gtrendh_sa-stats::lag(gtrendh_sa,-2)
dgtrendh_sa2_1 = stats::lag(dgtrendh_sa2,-1)
dgtrenda_sa2 = gtrenda_sa-stats::lag(gtrenda_sa,-2)
dgtrendv_sa2 = gtrendv_sa-stats::lag(gtrendv_sa,-2)
dgtrenda_sa2_1 = stats::lag(dgtrenda_sa2,-1)

var <- ts.union(dgtrendv_sa2)
var <- ts.union(dICM_sa,dICM_sa_1)
var <- ts.union(dgtrendh_sa)
var <- ts.union(gtrendh)

#ts.plot(dgtrenda_sa2,dgtrendh_sa2)



tourism_spec <- regarima_spec_tramoseats(
  spec="TRfull",
  transform.function = "Auto",
  estimate.from = "2016-01-01",
  estimate.to = "2023-01-01",
  automdl.enabled = TRUE,
  usrdef.outliersEnabled = TRUE,
  tradingdays.option = "TradingDays",
  usrdef.outliersType = c(
    "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO"
  ),
  usrdef.outliersDate = c(
    "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
    "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
    "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01",
    "2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01",
    "2022-01-01", "2022-02-01", "2022-03-01"
  ),
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
tourism_regarima <- regarima(elements$Historical, tourism_spec)
tourism_regarima

#ts.plot(window(gtrendh,start=c(2018,1)),window(log(elements$Historical)/25,start=c(2018,1)))
