library(tsibble)
library(fable)
library(feasts)

data_tourism_ts <- data$TOURISM %>%
  to_tsibble() %>%
  fill_gaps() %>%
  group_by(geo) %>%
  mutate(values = zoo::na.locf(values)) %>%
  ungroup() %>%
  filter((year(time) >= 2003 & !geo %in% c("MT", "FR")) |
    (year(time) >= 2010 & geo %in% c("MT", "FR")))

# Model identified without COVID
# Could be changed for AT and HR
models <- data_tourism_ts %>%
  filter(year(time) < 2020) %>%
  model( # naive = NAIVE(values),
    # rw = RW(values ~ drift()),
    # mean3m = MEAN(values~window(3)),
    # arima = ARIMA(log(values)),
    ETS = ETS(values)
  ) %>%
  refit(data_tourism_ts, reinitialise = FALSE, reestimate = FALSE)

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
