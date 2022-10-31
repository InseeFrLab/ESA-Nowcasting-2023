library(tsibble)
library(fable)
library(feasts)


models = data$TOURISM  %>% 
  filter((time >= ymd("2003-01-01") & !geo%in% c("MT", "FR")) |
           (time >= ymd("2010-01-01") & geo%in% c("MT", "FR"))) %>% 
  to_tsibble() %>% 
  fill_gaps() %>% 
  group_by(geo) %>% 
  mutate(values = zoo::na.locf(values)) %>% 
  ungroup() %>% 
  model(snaive = SNAIVE(values)#,
        #ETS = ETS(values)
        )
# models %>% accuracy() %>%
#   select(geo, .model, RMSE) %>% 
#   pivot_wider(names_from = .model, values_from = RMSE) %>% 
#   print(n=30)

preds_snaive = models %>% forecast(h = "12 months") %>% 
  filter(time == date_to_pred) 
preds_snaive = preds_snaive %>% 
  mutate(Country = geo,
         Date = time,
         value = .mean) %>% 
  as_tibble() %>% 
  select(Country, Date, value)

# models %>% forecast(h = "1 month")%>%  filter(geo == "AT") %>% 
#   autoplot(data$PPI %>%   to_tsibble()  %>% filter(geo == "AT") %>% 
#              filter(time >= ymd("2022-01-01")))
