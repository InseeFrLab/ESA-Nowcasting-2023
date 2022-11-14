###############################################################################
#                  Time series models : REGARIMA                              #  
###############################################################################

#########################################
# Required packages
#########################################

#install.packages("RJDemetra")
library(dplyr)
library(astsa)
library(lubridate)
library(RJDemetra)

#########################################
# Estimate a REGARIMA
#########################################

preds_regarima <- tibble(Country=character(),
                       Date=as.POSIXct(NA),
                       value=numeric()
                       )

for (country in countries_tourism){

  #série cible + vérif date de début
  debut <- data$TOURISM%>%filter(geo %in% country)%>%slice(1:1)%>%pull(time)
  debut<-c(year(debut),month(debut))
  
  fin <- data$TOURISM%>%filter(geo %in% country)%>%last%>%pull(time)
  
  n_forward <- interval(fin, date_to_pred) %/% months(1)
  
  tour <- ts(data$TOURISM%>%filter(geo %in% country)%>%pull(values),start=debut,frequency=12)
  dltour <- window(log(tour)-stats::lag(log(tour),-1),start=c(2010,1))
  
  tour_spec <- regarima_spec_tramoseats(transform.function="Auto",
                                        arima.coefType="Undefined",
                                        arima.p=0,
                                        arima.q=1,
                                        arima.bp=0,
                                        arima.bq=1,
                                        arima.d = 1,
                                        arima.bd=1,
                                       estimate.from="2010-01-01",
                                       estimate.to="2019-01-01",
                                       automdl.enabled=FALSE,
                                       usrdef.outliersEnabled=TRUE,
                                       usrdef.outliersType = c("AO","AO", "AO","AO", "AO","AO",
                                                               "AO", "AO","AO", "AO","AO","AO",
                                                               "AO", "AO","AO", "AO","AO", "AO",
                                                               "AO", "AO","AO", "AO","AO", "AO",
                                                               "AO", "AO","AO"),
                                       usrdef.outliersDate = c("2020-01-01","2020-02-01","2020-03-01", "2020-04-01","2020-05-01","2020-06-01",
                                                               "2020-07-01","2020-08-01","2020-09-01", "2020-10-01","2020-11-01","2020-12-01",
                                                               "2021-01-01","2021-02-01","2021-03-01", "2021-04-01","2021-05-01", "2021-06-01",
                                                               "2021-07-01","2021-08-01","2021-09-01", "2021-10-01","2021-11-01", "2021-12-01",
                                                               "2022-01-01","2022-02-01","2022-03-01"),
                                      outlier.enabled=TRUE,
                                      outlier.ao=TRUE,
                                      outlier.tc=FALSE,
                                      outlier.ls = TRUE,
                                      outlier.usedefcv=FALSE,
                                      outlier.cv=3.5,
                                      #usrdef.varEnabled = TRUE,
                                      #usrdef.var = var,
                                      fcst.horizon=2)
  tour_regarima <- regarima(tour,tour_spec)
  
  #Deux cas : pred à 1 ou 2 horizons
  if (n_forward==1) {pred <- tour_regarima$forecast[1]}
  if (n_forward==2) {pred <- tour_regarima$forecast[2]}
  
  preds_regarima <- preds_regarima %>%
    add_row(Country=country,
            Date=date_to_pred,
            value=round(as.numeric(pred),1))
  
}
