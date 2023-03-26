# rm(models)
# 
# targets::tar_load
# 
# create_regressors("TOURISM", challenges, data, "EL")
# essai <- run_regarima("TOURISM", challenges, data, models)
# 
# essai$preds
# 
# country <- "DE"
# 
# challenges_info <- challenges
# 
# date_to_pred <- ymd(challenges$DATES$date_to_pred)
# DB <- build_data_regarima("TOURISM", challenges, data, "DE")
# 
# h <- lubridate::interval(last(index(tsbox::ts_xts(DB$y))), date_to_pred) %/% months(1)
# 
# regarima <- estimate_regarima(challenge, DB, models, "DE", h)
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
