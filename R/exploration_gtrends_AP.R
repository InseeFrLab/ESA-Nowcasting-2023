# # Categories and topics gtrends:
# #   https://github.com/pat310/google-trends-api/wiki/Google-Trends-Categories
# 
# # Paper with implementation of the method:
# #   https://www.oecd-ilibrary.org/docserver/6b9c7518-en.pdf?expires=1677169973&id=id&accname=guest&checksum=9E6B60934CB624C8C42FDDEB85F1CE90
# #   Annex A page 40
# 
# country = 'FR'
# 
# # Imports of the data
# 
# get_gtrends <- function(country = country, category = 179){
#   gtrends_data <- gtrendsR::gtrends(geo = country,
#                                     time = "all",
#                                     gprop = "web",
#                                     category = category,
#                                     onlyInterest = TRUE)
#   return(tsbox::ts_xts(gtrends_data$interest_over_time[, c("date", "geo", "category", "hits")]))
# }
# 
# ### 'Hotels & Accommodations' topic = 179
# hotels_gtrends_data <- get_gtrends(country = country,
#                                    category = c(179))
# plot(hotels_gtrends_data)
# 
# vacation_gtrends_data <- get_gtrends(country = country,
#                                      category = c(1119))
# plot(vacation_gtrends_data)
# 
# ### All searches
# # We should look into "All categories" (code 0), but does not work...
# # So we manually add the searches for all of the main categories
# 
# # all_gtrends_data <- get_gtrends(category = 3) # Initialisation to the first category
# # 
# # # Boucle pour évaluer la fonction sur tous les x de l'intervalle et sommer les résultats
# # for(category in c(47, 44, 22, 12, 5, 7, 71, 8, 45, 65, 11, 13, 958, 19, 16, 299, 14, 66, 29, 533, 174, 18, 20, 67)){
# #   # List of all the other categories
# #   tryCatch({
# #     all_gtrends_data <- all_gtrends_data + get_gtrends(category = category)
# #   }, error = function(e){})
# # }
# 
# # SVT_t <- all_gtrends_data
# 
# 
# # Creation of SVI and svi
# 
# SVI_1t <- hotels_gtrends_data
# SVI_2t <- vacation_gtrends_data
# 
# # SV_ct <- hotels_gtrends_data
# # ratio <- SV_ct / SVT_t
# # C_c <- max(ratio)
# # SVI_ct <- ratio * 100 / C_c
# # plot(SVI_ct)
# # svi_ct <- log(SVI_ct)
# 
# svi_1t <- log(SVI_1t)
# svi_2t <- log(SVI_2t)
# 
# # Extracting the common component
# 
# hp_filtered_svi_1t <- xts::xts(mFilter::hpfilter(svi_1t,freq=12,"frequency")[["trend"]][,"FR_179"], zoo::index(svi_1t))
# hp_filtered_svi_2t <- xts::xts(mFilter::hpfilter(svi_2t,freq=12,"frequency")[["trend"]][,"FR_1119"], zoo::index(svi_2t))
# 
# # Combine the time series into one file
# 
# stacked_series <- merge(hp_filtered_svi_1t, hp_filtered_svi_2t)
# plot(stacked_series)
# 
# # Compute the PCA
# pca <- prcomp(stacked_series, retx = TRUE, center = TRUE, scale = TRUE)
# # See the principal components
# first_component <- xts::xts(pca$x[,"PC1"], zoo::index(hp_filtered_svi_1t))
# 
# # Rescale the result
# # mean_svi = xts::xts(rowMeans(stacked_series), zoo::index(hp_filtered_svi_1t))
# mean_svi = (hp_filtered_svi_1t + hp_filtered_svi_2t)/2
# standardised_first_component <- (first_component - mean(first_component)) / sd(first_component)
# rescaled_first_component <- standardised_first_component * sd(mean_svi) + mean(mean_svi)
# 
# # Check the direction of the PCA
# trend <- lm(rescaled_first_component ~ time(rescaled_first_component))
# slope <- trend$coefficients[2]
# if (slope > 0){
#   rescaled_first_component = - rescaled_first_component
# }
# 
# # Correct the time series
# 
# corrected_svi_1t <- svi_1t - rescaled_first_component
# corrected_svi_2t <- svi_2t - rescaled_first_component
# 
# corrected_SVI_1t <- exp(corrected_svi_1t)
# corrected_SVI_2t <- exp(corrected_svi_2t)
# 
# new_C_1 = 100/max(corrected_SVI_1t)
# final_SVI_1t <- corrected_SVI_1t * new_C_1
# plot(final_SVI_1t)
# 
# new_C_2 = 100/max(corrected_SVI_2t)
# final_SVI_2t <- corrected_SVI_2t * new_C_2
# plot(final_SVI_2t)
