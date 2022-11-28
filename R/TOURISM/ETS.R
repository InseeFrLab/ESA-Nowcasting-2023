library(tsibble)
library(fable)
library(feasts)

to_tsibble <- function(x) {
  x %>% 
    mutate(time = yearmonth(time)) %>% 
    drop_na() %>%
    as_tsibble(key = c(geo), index = time)
}
data_tourism_ts <- data$TOURISM  %>% to_tsibble() %>% 
  fill_gaps() %>% 
  group_by(geo) %>% 
  mutate(values = zoo::na.locf(values)) %>% 
  ungroup() %>% 
  filter((year(time) >= 2003 & !geo %in% c("MT", "FR")) |
           (year(time) >= 2010 & geo %in% c("MT", "FR")))

# Model identified without COVID
# Could be changed for AT and HR
models = data_tourism_ts  %>% 
  filter(year(time) < 2020) %>% 
  model(#naive = NAIVE(values),
    #rw = RW(values ~ drift()),
    # mean3m = MEAN(values~window(3)),
    #arima = ARIMA(log(values)),
    ETS = ETS(values)) %>% 
  refit(data_tourism_ts, reinitialise = FALSE, reestimate = FALSE)

preds_ets = models %>% forecast(h = "12 months") %>% 
  filter(time == date_to_pred) 

# models = data_tourism_ts %>% 
#   to_tsibble() %>% 
#   fill_gaps() %>% 
#   group_by(geo) %>% 
#   mutate(values = zoo::na.locf(values)) %>% 
#   ungroup() %>% 
#   model(#naive = NAIVE(values),
#     #rw = RW(values ~ drift()),
#     # mean3m = MEAN(values~window(3)),
#     #arima = ARIMA(log(values)),
#     ETS = ETS(values))
# models2_noth <- models2 %>% 
#   refit(data_tourism_ts, reinitialise = FALSE, reestimate = FALSE)
# models2_rei <- models2 %>% 
#   refit(data_tourism_ts, reinitialise = TRUE, reestimate = FALSE)
# models2_ree <- models2 %>% 
#   refit(data_tourism_ts, reinitialise = FALSE, reestimate = TRUE)
# models2_all <- models2 %>% 
#   refit(data_tourism_ts, reinitialise = TRUE, reestimate = TRUE)
# country = "AT"
# autoplot(data_tourism_ts  %>% filter(geo == country) %>%
#            filter(year(time) >= 2020)) +
#   autolayer(models %>% forecast(h = "12 month") %>%  filter(geo == country), series = "est_all",
#             level = NULL) +
#   autolayer(models2_noth %>% forecast(h = "12 month") %>%  filter(geo == country), series = "est_all",
#             level = NULL) +
#   autolayer(models2_rei %>% forecast(h = "12 month") %>%  filter(geo == country), series = "est_all",
#             level = NULL) +
#   autolayer(models2_ree %>% forecast(h = "12 month") %>%  filter(geo == country), series = "est_all",
#             level = NULL) +
#   autolayer(models2_all %>% forecast(h = "12 month") %>%  filter(geo == country), series = "est_all",
#             level = NULL)
# # stats_mse = models %>%  glance() %>% 
# #   select(geo, MSE) %>% 
# #   rename(mse_act = MSE) %>% 
# #   left_join(models2_noth %>%  glance() %>% 
# #               select(geo, MSE) %>% 
# #               rename(mse_noth = MSE)) %>% 
# #   left_join(models2_rei %>%  glance() %>% 
# #               select(geo, MSE) %>% 
# #               rename(mse_rei = MSE)) %>% 
# #   left_join(models2_ree %>%  glance() %>% 
# #               select(geo, MSE) %>% 
# #               rename(mse_ree = MSE)) %>% 
# #   left_join(models2_all %>%  glance() %>% 
# #               select(geo, MSE) %>% 
# #               rename(mse_all = MSE))
# stats_mae = models %>%  glance() %>% 
#   select(geo, MAE) %>% 
#   rename(MAE_act = MAE) %>% 
#   left_join(models2_noth %>%  glance() %>% 
#               select(geo, MAE) %>% 
#               rename(MAE_noth = MAE)) %>% 
#   left_join(models2_rei %>%  glance() %>% 
#               select(geo, MAE) %>% 
#               rename(MAE_rei = MAE)) %>% 
#   left_join(models2_ree %>%  glance() %>% 
#               select(geo, MAE) %>% 
#               rename(MAE_ree = MAE)) %>% 
#   left_join(models2_all %>%  glance() %>% 
#               select(geo, MAE) %>% 
#               rename(MAE_all = MAE))
# 
# stats %>% 
#   mutate(., min = .[,-1] %>% apply(1, which.min)) %>% 
#   print(n=24)
# stats_mae  %>% 
#   mutate(., min = .[,-1] %>% apply(1, which.min)) %>% 
#   print(n=24)
#   
# models2 %>% tidy()
# models2 %>% refit(data_tourism_ts,
#                   reinitialise = TRUE, reestimate = FALSE) %>% 
#   tidy()

# preds_ets2 = models %>% forecast(h = "12 months",
#                                  data$TOURISM  %>% 
#                                    filter((year(time) >= 2003 & !geo%in% c("MT", "FR")) |
#                                             (year(time) >= 2010 & geo%in% c("MT", "FR"))) %>% 
#                                    to_tsibble() %>% 
#                                    fill_gaps() %>% 
#                                    group_by(geo) %>% 
#                                    mutate(values = zoo::na.locf(values)) %>% 
#                                    ungroup())
# preds_ets = preds_ets %>% 
#   mutate(Country = geo,
#          Date = time,
#          value = .mean) %>% 
#   as_tibble() %>% 
#   select(Country, Date, value)
# 
# autoplot(data$TOURISM %>%   to_tsibble()  %>% filter(geo == "AT") %>%
#            filter(time >= ymd("2020-01-01"))) +
#   autolayer(models %>% forecast(h = "12 month")%>%  filter(geo == "AT"))
# 
# autoplot(data$TOURISM %>%   to_tsibble()  %>% filter(geo == "FR") %>%
#            filter(time >= ymd("2015-01-01"))) +
#   autolayer(models %>% forecast(h = "12 month")%>%  filter(geo == "FR"))
 