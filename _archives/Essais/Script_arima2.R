# rm(models)
# 
# targets::tar_load
# 
# create_regressors("TOURISM", challenges, data, "EL")
# essai <- run_regarima("TOURISM", challenges, data, models)
# 
# essai$preds
# 
# challenge <- "TOURISM"
# country <- "DE"
# 
# challenges_info <- challenges
# 
# regarima <- RJDemetra::regarima(DB$y, specification)
# 
# date_to_pred <- ymd(challenges$DATES$date_to_pred)
# DB <- build_data_regarima("TOURISM", challenges, data, "DE")
# 
# h <- lubridate::interval(last(index(tsbox::ts_xts(DB$y))), date_to_pred) %/% months(1)
# 
# regarima <- estimate_regarima("TOURISM", DB, models, "DE", h)
# 
# if (challenge != "TOURISM") {
#   pred <- last(DB$Historical) * prod(exp(regarima$forecast[, 1]))
# }
# 
# if (challenge == "TOURISM") {
#   pred <- last(DB$Historical_sa) * prod(exp(regarima$forecast[, 1]))
#   pred <- pred / DB$coef_sa
# }
# 
# plot(DB$Historical_sa)
# 
# 
# elements <- build_data_regarima("TOURISM", challenges, data, country)
# dy_sa <- elements$y
# dgtrendv_sa2<- elements$X[,"dgtrendv_sa2"]
# dICM_sa_1<- elements$X[,"dICM_sa_1"]
# 
# 
# var <- ts.union(dICM_sa_1,dgtrendv_sa2)
# 
# tourism_spec <- regarima_spec_tramoseats(
#   transform.function = "None",
#   estimate.from = "2016-01-01",
#   automdl.enabled = TRUE,
#   usrdef.outliersEnabled = TRUE,
#   usrdef.outliersType = c(
#     "AO", "AO", "AO", "AO", "AO",
#     "AO", "AO", "AO", "AO", "AO", "AO",
#     "AO", "AO", "AO", "AO", "AO", "AO",
#     "AO", "AO"
#   ),
#   usrdef.outliersDate = c(
#     "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01",
#     "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01",
#     "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01",
#     "2021-07-01", "2021-08-01"
#   ),
#   outlier.enabled = TRUE,
#   outlier.ao = TRUE,
#   outlier.tc = FALSE,
#   outlier.ls = TRUE,
#   outlier.usedefcv = FALSE,
#   outlier.cv = 3.5,
#   usrdef.varEnabled = TRUE,
#   usrdef.var = var,
#   fcst.horizon = 2
# )
# tourism_regarima <- regarima(dy_sa, tourism_spec)
# tourism_regarima
