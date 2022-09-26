library(dplyr)
library(tidyr)
library(eurostat)

EU26 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
          "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SI","SK")

ppi_data <- get_eurostat("sts_inppd_m"
                         , select_time = "M"
                         , filters = list(
                           geo = EU26,
                           indic_bt="PRIN",
                           nace_r2="B-E36", 
                           s_adj="NSA",
                           unit="I15"
                         )
                         , time_format = "date"
)%>%
  select(geo, time, values)%>%
  drop_na()%>%
  pivot_wider(names_from = geo, values_from = values)%>%
  arrange(time)


pvi_data <- get_eurostat("sts_inpr_m"
                         , select_time = "M"
                         , filters = list(
                           geo = EU26,
                           indic_bt="PROD",
                           nace_r2="B-D", 
                           s_adj="SCA",
                           unit="I15"
                         )
                         , time_format = "date"
)%>%
  select(geo, time, values)%>%
  drop_na()%>%
  pivot_wider(names_from = geo, values_from = values)%>%
  arrange(time)

tourism_data <- get_eurostat("tour_occ_nim"
                             , select_time = "M"
                             , filters = list(
                               geo = EU26,
                               c_resid="TOTAL",
                               nace_r2="I551-I553", 
                               unit="NR"
                             )
                             , time_format = "date"
)%>%
  select(geo, time, values)%>%
  drop_na()%>%
  pivot_wider(names_from = geo, values_from = values)%>%
  arrange(time)
