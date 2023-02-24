# Categories and topics gtrends:
#   https://github.com/pat310/google-trends-api/wiki/Google-Trends-Categories

# Paper with implementation of the method:
#   https://www.oecd-ilibrary.org/docserver/6b9c7518-en.pdf?expires=1677169973&id=id&accname=guest&checksum=9E6B60934CB624C8C42FDDEB85F1CE90
#   Annex A page 40

country = 'FR'

# Imports of the data

### 'Hotels & Accommodations' topic = 179
hotels_gtrends_data <- gtrendsR::gtrends(geo = country,
                                         time = "all",
                                         gprop = "web",
                                         category = c(179),
                                         onlyInterest = TRUE)

### All searches
# We should look into "All categories" (code 0), but does not work...
# So we manually add the searches for all of the main categories

get_gtrends <- function(country = country, category = 179){
  gtrends_data <- gtrendsR::gtrends(geo = country,
                                    time = "all",
                                    gprop = "web",
                                    category = category,
                                    onlyInterest = TRUE)
  return(tsbox::ts_xts(gtrends_data$interest_over_time[, c("date", "geo", "category", "hits")]))
}

all_gtrends_data <- get_gtrends(category = 3) # Initialisation to the first category

# Boucle pour évaluer la fonction sur tous les x de l'intervalle et sommer les résultats
for(category in c(47, 44, 22, 12, 5, 7, 71, 8, 45, 65, 11, 13, 958, 19, 16, 299, 14, 66, 29, 533, 174, 18, 20, 67)){
  # List of all the other categories
  tryCatch({
    all_gtrends_data <- all_gtrends_data + get_gtrends(category = category)
  }, error = function(e){})
}

# ### 'Travel' category = 67
# # We should look into "All categories" (code 0), but does not work...
# travel_gtrends_data <- gtrendsR::gtrends(geo = "FR",
#                                          time = "all",
#                                          gprop = "web",
#                                          category = c(67),
#                                          onlyInterest = TRUE)

# Creation of SVI and svi

SV_ct <- tsbox::ts_xts(hotels_gtrends_data$interest_over_time[, c("date", "geo", "category", "hits")])
# SVT_t <- tsbox::ts_xts(travel_gtrends_data$interest_over_time[, c("date", "geo", "category", "hits")])
SVT_t <- all_gtrends_data

ratio <- SV_ct / SVT_t
C_c <- max(ratio)
SVI_ct <- ratio * 100 / C_c
plot(SVI_ct)

svi_ct <- log(SVI_ct)

# Extracting the common component

hp_filtered_svi_ct <- xts::xts(mFilter::hpfilter(svi_ct,freq=12,"frequency")[["trend"]][,"FR_179"], zoo::index(svi_ct))

# Compute the PCA
pca <- prcomp(hp_filtered_svi_ct, retx = TRUE, center = TRUE, scale = TRUE)
# See the principal components
first_component <- xts::xts(pca$x[,"PC1"], zoo::index(hp_filtered_svi_ct))
# Rescale the result
standardised_first_component <- (first_component - mean(first_component)) / sd(first_component)
rescaled_first_component <- standardised_first_component * sd(svi_ct) + mean(svi_ct)

corrected_svi_ct <- svi_ct - rescaled_first_component

corrected_SVI_ct <- exp(corrected_svi_ct)

new_C = 100/max(corrected_SVI_ct)
final_SVI_ct <- corrected_SVI_ct * new_C

plot(final_SVI_ct)

