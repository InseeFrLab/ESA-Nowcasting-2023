library(tsibble)
library(fable)
library(feasts)
to_tsibble <- function(x) {
  x %>% 
    mutate(time = yearmonth(time)) %>% 
    drop_na() %>%
    as_tsibble(key = c(geo, nace_r2), index = time)
}

models = data$PPI  %>% 
  filter(year(time) >= 2003) %>% 
  to_tsibble() %>% 
  model(#naive = NAIVE(values),
        #rw = RW(values ~ drift()),
        # mean3m = MEAN(values~window(3)),
        #arima = ARIMA(log(values)),
        ETS = ETS(values))
preds_ets = models %>% forecast(h = "12 months") %>% 
  filter(as.Date(time) == date_to_pred) %>% 
  mutate(Country = geo,
         Date = as.Date(time),
         value = .mean) %>% 
  as_tibble() %>% 
  select(Country, Date, value)
resid_ets = models %>% residuals() %>% 
  mutate(Country = geo,
         Date = as.Date(time),
         value = .resid) %>% 
  as_tibble() %>% 
  select(Country, Date, value)

# models %>% forecast(h = "1 month")%>%  filter(geo == "BE") %>%
#   autoplot(data$PPI %>%   to_tsibble()  %>% filter(geo == "BE") %>%
#              filter(time >= ymd("2022-01-01")))
