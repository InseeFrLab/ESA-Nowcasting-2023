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
# Estimate a SARIMA
#########################################

preds_sarima <- tibble(Country=character(),
                       Date=as.POSIXct(NA),
                       value=numeric()
                       )

for (country in countries_PVI){

  n_forward <- interval(current_date, date_to_pred) %/% months(1)
  
  #série cible + vérif date de début
  debut <- data$PVI%>%filter(geo %in% country)%>%slice(1:1)%>%pull(time)
  debut<-c(year(debut),month(debut))
  pvi_ts <- ts(data$PVI%>%filter(geo %in% country)%>%pull(values),start=debut,frequency=12)
  dlpvi <- log(pvi_ts)-stats::lag(log(pvi_ts),-1)
  #Exogènes
  debut <- data$PSURVEY%>%filter(geo %in% country)%>% filter(indic == "BS-ICI")%>%slice(1:1)%>%pull(time)
  debut<-c(year(debut),month(debut))
  IS <- ts(data$PSURVEY%>%filter(geo %in% country) %>% filter(indic == "BS-ICI") %>%pull(values),start=debut,frequency=12)
  debut <- data$PSURVEY%>%filter(geo %in% country)%>% filter(indic == "BS-IPT")%>%slice(1:1)%>%pull(time)
  debut<-c(year(debut),month(debut))
  IPT <- ts(data$PSURVEY%>%filter(geo %in% country) %>% filter(indic == "BS-IPT") %>%pull(values),start=debut,frequency=12)
  
  var <- ts.union(IPT/100,diff(IPT)/100)
  
  pvi_spec <- regarima_spec_tramoseats(transform.function="None",
                                       estimate.from="2010-01-01",
                                       automdl.enabled=TRUE,
                                       usrdef.outliersEnabled=TRUE,
                                       usrdef.outliersType = c("AO","AO", "AO","AO", "AO","AO",
                                                               "AO", "AO","AO", "AO","AO","AO",
                                                               "AO", "AO","AO", "AO","AO", "AO"),
                                       usrdef.outliersDate = c("2020-01-01","2020-02-01","2020-03-01", "2020-04-01","2020-05-01","2020-06-01",
                                                               "2020-07-01","2020-08-01","2020-09-01", "2020-10-01","2020-11-01","2020-12-01",
                                                               "2021-01-01","2021-02-01","2021-03-01", "2021-04-01","2021-05-01", "2021-06-01"),
                                      outlier.enabled=TRUE,
                                      outlier.ao=TRUE,
                                      outlier.tc=FALSE,
                                      outlier.ls = TRUE,
                                      outlier.usedefcv=FALSE,
                                      outlier.cv=3.5,
                                      usrdef.varEnabled = TRUE,
                                      usrdef.var = var,
                                      fcst.horizon=2)
  pvi_regarima <- regarima(dlpvi,pvi_spec)
  
  #Deux cas : pred à 1 ou 2 horizons
  if (n_forward==1) {pred <- pvi_ts %>% tail(1) * exp(pvi_regarima$forecast[1])}
  if (n_forward==2) {pred <- pvi_ts %>% tail(1) * exp(pvi_regarima$forecast[1]) * exp(pvi_regarima$forecast[2])}
  
  preds_sarima <- preds_sarima %>%
    add_row(Country=country,
            Date=date_to_pred,
            value=round(as.numeric(pred),1))
  
}
