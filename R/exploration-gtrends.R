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
# Hotels & Accommodations 179 : DONE
# Travel Agencies & Services 1010 : DONE
# Travel Guides & Travelogues 1011 : TO DO
# Vacation Offers 1019 : DONE
# Mountain & Ski Resorts 1119 : DONE
#
# Business & Industrial 12 : TO DO
# Business News 784 : TO DO
# Small Business 551 : TO DO
# Manufacturing 49 GO : DONE
# Financial Markets 1163 : TO DO
# Fiscal Policy News 1165 : TO DO
# Housing & Development 1166 : TO DO
# Company News 1179 : TO DO
# Industrial Materials & Equipment 287 : DONE
# Electricity 658 : TO DO
# Oil & Gas 659 : TO DO
# Fuel Economy & Gas Prices 1268 : DONE
# Economy News 1164 : TO DO
# Economics 520 : TO DO
# Bankruptcy 423 : TO DO
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
