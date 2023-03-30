data_temp <- tempfile()
download.file(
  "https://www.wifo.ac.at/wwadocs/konjunktur/W%C3%B6chentlicherWIFOWirtschaftsindex/WIFO-BusinessCycleAnalysis_WeeklyWIFOEconomicIndex.xlsx",
  data_temp
)

wifo <- readxl::read_excel(
  path = data_temp,
  sheet = "Contributions_production",
  skip = 3
) %>%
  rename(mois = paste0("...1"),
         semaine = paste0("...2"),
         ind = paste0("...4")) %>%
  mutate(wifo_ind=as.numeric(ind)) %>% 
  select(mois, semaine, wifo_ind) %>% 
  mutate(annee=substr(mois,nchar(mois)-3,nchar(mois)))

an <- "2020"
for (i in 1:nrow(wifo)) {
  if (is.na(wifo[i,4])) {
    wifo[i,4] <- an
  }
  else {an <- wifo[i,4]}
}

#wifo <- wifo[-1,]
wifo <- (subset(wifo,!is.na(wifo$semaine) & !is.na(wifo$wifo_ind)))

wifo <- wifo %>% 
  mutate(time=ymd(paste0(annee,"0101"))+weeks(substr(semaine,3,4)),
         geo = "AT") %>% 
  select(time,wifo_ind,geo)


