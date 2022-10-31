library(tsibble)
library(fable)
library(feasts)

to_tsibble <- function(x) {
  x %>% 
    mutate(time = yearmonth(time)) %>% 
    drop_na() %>%
    as_tsibble(key = c(geo), index = time)
}

models = data$TOURISM  %>% 
  filter((time >= ymd("2003-01-01") & !geo%in% c("MT", "FR")) |
           (time >= ymd("2010-01-01") & geo%in% c("MT", "FR"))) %>% 
  to_tsibble() %>% 
  fill_gaps() %>% 
  group_by(geo) %>% 
  mutate(values = zoo::na.locf(values)) %>% 
  ungroup() %>% 
  model(#naive = NAIVE(values),
    #rw = RW(values ~ drift()),
    # mean3m = MEAN(values~window(3)),
    #arima = ARIMA(log(values)),
    ETS = ETS(values))
  
preds_ets = models %>% forecast(h = "12 months") %>% 
  filter(time == date_to_pred) 
preds_ets = preds_ets %>% 
  mutate(Country = geo,
         Date = time,
         value = .mean) %>% 
  as_tibble() %>% 
  select(Country, Date, value)

# models %>% forecast(h = "1 month")%>%  filter(geo == "AT") %>% 
#   autoplot(data$PPI %>%   to_tsibble()  %>% filter(geo == "AT") %>% 
#              filter(time >= ymd("2022-01-01")))
