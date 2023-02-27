# Crisis/Recession: topic - Economic crisis, topic - Crisis, topic - Recession
# 
# Bankruptcy: topic - Bankruptcy, topic - foreclosure
# 
# Credit, Loans & Personal Finance: topic - Investment, topic - Mortgage, topic - Interest rate, Credit & lending, Investing
# 
# Consumption Items & Services: Food & drink, Vehicle brands, Home & garden, Sports, Autos & vehicles, Grocery & food retailers, Vehicle licensing & registration, Hotels & accommodations
# 
# News: Business news, Economy news
# 
# Housing: topic - Affordable housing, topic - House price index
# 
# Business & Industrial Activity: Construction, consulting & contracting, Business services, Transportation & logistics, manufacturing
# 
# Health: Health
# 
# 
# 
# Hotels & Accommodations 179 GO
# Travel Agencies & Services 1010
# Travel Guides & Travelogues 1011
# Vacation Offers 1019 GO
# Mountain & Ski Resorts 1119
# 
# Business & Industrial 12
# Business News 784
# Small Business 551
# Manufacturing 49 GO
# Financial Markets 1163
# Fiscal Policy News 1165
# Housing & Development 1166
# Company News 1179
# Industrial Materials & Equipment 287 GO
# Electricity 658
# Oil & Gas 659 GO
# Fuel Economy & Gas Prices 1268 GO
# Economy News 1164
# Economics 520
# Bankruptcy 423
# 
# gtrends_data1 <- gtrendsR::gtrends(gprop = "web",
#                                   geo = "FR",
#                                   category = c(179),
#                                   time = "all")
# 
# 
# gtrends_data2 <- gtrendsR::gtrends(gprop = "web",
#                                    geo = "FR",
#                                    category = c(1010),
#                                    time = "all")
# 
# y1 <- tsbox::ts_xts(gtrends_data1$interest_over_time[, c("date", "geo", "category", "hits")])
# y2 <- tsbox::ts_xts(gtrends_data2$interest_over_time[, c("date", "geo", "category", "hits")])
# 
# log_y1 <- log(y1)
# log_y2 <- log(y2)
# 
# log_y1_filtered <- xts::xts(mFilter::hpfilter(log_y1,freq=12,"frequency")[["trend"]][,"FR_179"], zoo::index(log_y1))
# log_y2_filtered <- xts::xts(mFilter::hpfilter(log_y2,freq=12,"frequency")[["trend"]][,"FR_1010"], zoo::index(log_y2))
# plot(log_y1_filtered)
# plot(log_y2_filtered)
# stacked_series <- merge(log_y1_filtered, log_y2_filtered)
# plot(stacked_series)
# 
# my_pca <- prcomp(stacked_series, scale = TRUE,
#                  center = TRUE, retx = TRUE)
# # See the principal components
# PC1 <- xts::xts(my_pca$x[,"PC1"], zoo::index(stacked_series))
# 
# ############## 
# 
# log_y1_filtered <- mFilter::hpfilter(log_y1,freq=12,"frequency")[["trend"]]
# log_y2_filtered <- mFilter::hpfilter(log_y2,freq=12,"frequency")[["trend"]]
# stacked_series <- cbind(log_y1_filtered, log_y2_filtered)
# 
# my_pca <- prcomp(stacked_series, scale = TRUE,
#                  center = TRUE, retx = TRUE)
# # See the principal components
# PC1 <- my_pca$x[,"PC1"]
# 
# log_SVIs_average <- rowMeans(stacked_series)
# 
# PC1_rescaled <- mean(log_SVIs_average) + (PC1 - mean(PC1)) * sd(log_SVIs_average)/sd(PC1)
# 
# 
# z<- log_y1 - PC1_rescaled
# log_y2 - PC1_rescaled
# 
# 100/max(z) * z
# 
