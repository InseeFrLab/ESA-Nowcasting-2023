library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(forecast)

tbats_tourism = function(dates, y) {
  first_date = dates[1]
  y = ts(y, 
         start = c(year(first_date), month(first_date)),
         frequency = 12)
  tbats_mod = forecast::tbats(y)
  # TODO : améliorer ça pour prendre en compte changement d'années
  horizon = month(date_to_pred) - month(yearmon(time(y)[length(y)])) 
  forecast(tbats_mod, h = horizon)$mean[horizon]
}

preds_tbats = data$TOURISM  %>%
  filter((time >= ymd("2003-01-01") & !geo%in% c("MT", "FR")) |
           (time >= ymd("2010-01-01") & geo%in% c("MT", "FR"))) %>%
  to_tsibble() %>%
  fill_gaps() %>%
  group_by(geo) %>%
  mutate(values = zoo::na.locf(values)) %>%
  mutate(values = tbats_tourism(time, values)) %>%
  as_tibble() %>% 
  group_by(geo) %>% 
  summarise(
    values = dplyr::last(values),
    time = date_to_pred
  ) %>% 
  mutate(Country = geo,
         Date = time,
         value = values) %>% 
  select(Country, Date, value)
# ?forecast::tbats()
# data$TOURISM %>% 
#   filter(time >= ymd("2003-01-01")) %>% 
#   filter(geo == "FR") %>% tail()
# 
# dates = data$TOURISM  %>% 
#   filter((time >= ymd("2003-01-01") & !geo%in% c("MT", "FR")) |
#            (time >= ymd("2010-01-01") & geo%in% c("MT", "FR"))) %>% 
#   to_tsibble() %>% 
#   fill_gaps() %>% 
#   group_by(geo) %>% 
#   mutate(values = zoo::na.locf(values)) %>% 
#   filter(geo == "AT") %>% pull((time))
# first_date = ()[1]
# year
# y = data$TOURISM  %>% 
#   filter((time >= ymd("2003-01-01") & !geo%in% c("MT", "FR")) |
#            (time >= ymd("2010-01-01") & geo%in% c("MT", "FR"))) %>% 
#   to_tsibble() %>% 
#   fill_gaps() %>% 
#   group_by(geo) %>% 
#   mutate(values = zoo::na.locf(values)) %>% 
#   filter(geo == "AT") %>% pull(values)
# 
# y = ts(, 
#        start = year(first_date),
#        frequency = 12)
# tbats_mod = forecast::tbats(y)
# horizon = month(date_to_pred) - month(yearmon(time(y)[length(y)]))
# tmp = forecast(tbats_mod, h = horizon)$mean[horizon]
# models = data$TOURISM  %>% 
#   filter(time >= ymd("2003-01-01")) %>% 
#   to_tsibble() %>% 
#   model(#naive = NAIVE(values),
#     #rw = RW(values ~ drift()),
#     # mean3m = MEAN(values~window(3)),
#     #arima = ARIMA(log(values)),
#     TBATS = forecast::TBATS(values))
# preds_ets = models %>% forecast(h = "3 months") %>% 
#   filter(time == date_to_pred) 
# preds_ets = preds_ets %>% 
#   mutate(Country = geo,
#          Date = time,
#          value = .mean) %>% 
#   as_tibble() %>% 
#   select(Country, Date, value)
# 
# # models %>% forecast(h = "1 month")%>%  filter(geo == "AT") %>% 
# #   autoplot(data$PPI %>%   to_tsibble()  %>% filter(geo == "AT") %>% 
# #              filter(time >= ymd("2022-01-01")))

detach("package:forecast", unload = TRUE)
