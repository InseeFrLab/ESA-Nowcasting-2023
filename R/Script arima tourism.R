library(dplyr)
library(astsa)
library(lubridate)
library(RJDemetra)
library(xts)
library(tsbox)

targets::tar_load(data)
targets::tar_read(models_file)

country <- "DE"
challenge <- "TOURISM"
challenges_info <- challenges

#create_regressors(challenge, challenges, data, country)

elements <- build_data_regarima(challenge, challenges, data, country)
y <- elements$Historical
gtrenda<- elements$X[,"gtrenda"]
gtrendv<- elements$X[,"gtrendv"]
gtrendh<- elements$X[,"gtrendh"]
ICM<- elements$X[,"ICM"]


#targets::tar_load(challenges)
#targets::tar_load(data_info)

#plot(log(y))

#y <- window(y,start=c(2015,1))

ma_spec <- x13_spec(
  transform.function = "Auto",
  estimate.from = "2016-01-01",
  automdl.enabled = TRUE,
  tradingdays.option = "TradingDays",
  tradingdays.autoadjust = TRUE,
  easter.enabled=TRUE,
  easter.test="None",
  usrdef.outliersEnabled = TRUE,
  usrdef.outliersType = c(
    "AO", "AO", "AO", "AO", 
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO"
  ),
  usrdef.outliersDate = c(
    "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
    "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
    "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01",
    "2021-07-01", "2021-08-01", "2021-09-01"
  ),
  outlier.enabled = TRUE,
  outlier.ao = TRUE,
  outlier.tc = FALSE,
  outlier.ls = TRUE,
  # usrdef.varEnabled = TRUE,
  # usrdef.var = var,
)

result <- x13(window(y,start=c(2015,1)),ma_spec)
ls(result$final)
coef <- window(
  result$final$forecasts[,"sa_f"]/result$final$forecasts[,"y_f"],
  start=c(year(date_to_pred),month(date_to_pred)),
  end=c(year(date_to_pred),month(date_to_pred)))

y_sa <- x13(window(y,start=c(2015,1)),ma_spec)$final$series[,"sa"]
ICM_sa <- x13(window(ICM,start=c(2015,1)),ma_spec)$final$series[,"sa"]
gtrendh_sa <- x13(window(gtrendh,start=c(2015,1)),ma_spec)$final$series[,"sa"]
gtrendv_sa <- x13(window(gtrendv,start=c(2015,1)),ma_spec)$final$series[,"sa"]
gtrenda_sa <- x13(window(gtrenda,start=c(2015,1)),ma_spec)$final$series[,"sa"]
dy_sa=log(y_sa)-log(stats::lag(y_sa,-1))
dICM_sa=ICM_sa-stats::lag(ICM_sa,-1)
dICM_sa_1=stats::lag(dICM_sa,-1)
dgtrendh_sa=gtrendh_sa-stats::lag(gtrendh_sa,-1)
dgtrendv_sa=gtrendv_sa-stats::lag(gtrendv_sa,-1)
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
var <- ts.union(dICM_sa,dICM_sa_1,dgtrendv_sa2)

ts.plot(dgtrenda_sa2,dgtrendh_sa2)



tourism_spec <- regarima_spec_tramoseats(
  transform.function = "None",
  estimate.from = "2016-01-01",
  automdl.enabled = TRUE,
  usrdef.outliersEnabled = TRUE,
  usrdef.outliersType = c(
    "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO", "AO", "AO", "AO", "AO",
    "AO", "AO"
  ),
  usrdef.outliersDate = c(
    "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
    "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
    "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01",
    "2021-07-01", "2021-08-01"
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
tourism_regarima <- regarima(dy_sa, tourism_spec)
tourism_regarima

plot(dy_sa)
dy_sa

tourism_regarima$forecast[,1]

lubridate::interval(date_to_pred, last(index(tsbox::ts_xts(ICM)))) %/% months(1)
