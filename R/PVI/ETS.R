library(tsibble)
library(fable)
library(feasts)

to_tsibble <- function(x) {
  x %>%
    mutate(time = yearmonth(time)) %>%
    drop_na() %>%
    as_tsibble(key = c(geo), index = time)
}

models <- data$PVI %>%
  filter(year(time) >= 2003) %>%
  to_tsibble() %>%
  model( # naive = NAIVE(values),
    # rw = RW(values ~ drift()),
    # mean3m = MEAN(values~window(3)),
    # arima = ARIMA(log(values)),
    ETS = ETS(values)
  )
preds_ets <- models %>%
  forecast(h = "12 months") %>%
  filter(as.Date(time) == date_to_pred) %>%
  mutate(
    Country = geo,
    Date = as.Date(time),
    value = round(.mean, 1)
  ) %>%
  as_tibble() %>%
  select(Country, Date, value)
resid_ets <- models %>%
  residuals() %>%
  mutate(
    Country = geo,
    Date = as.Date(time),
    value = .resid
  ) %>%
  as_tibble() %>%
  select(Country, Date, value)

# models %>% forecast(h = "1 month")%>%  filter(geo == "AT") %>%
#   autoplot(data$PPI %>%   to_tsibble()  %>% filter(geo == "AT") %>%
#              filter(time >= ymd("2022-01-01")))
