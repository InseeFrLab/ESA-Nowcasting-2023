#' Data Retrieval functions
#'
#' This module provides a collection of functions for retrieving data from 
#' different data sources. It includes functions specifically designed for 
#' fetching data from various sources such as databases, APIs, files... 
#' These functions facilitate the retrieval of data for further analysis.


# Eurostat

get_data_from_eurostat <- function(data_info) {
  subset_lists <- Filter(function(x) x$source == "Eurostat", data_info)

  data <- lapply(subset_lists, function(x) {
    get_eurostat_data(x$id,
      filters = x$filters
    ) |>
      dplyr::select(geo, names(x$filters)[3], time, values) |>
      tidyr::drop_na(values)
  })

  return(data)
}

get_eurostat_data <- function(id, filters = NULL,
                              stringsAsFactors = FALSE,
                              ...) {
  # get url
  url <- prepare_url(id = id, filters = filters)

  # get response
  resp <- httr::RETRY("GET", url, terminate_on = c(404), times = 20)

  if (httr::http_error(resp)) {
    stop(paste("The requested url cannot be found:", url))
  }

  status <- httr::status_code(resp)

  # check status and get json

  msg <- ". Some datasets are not accessible via the eurostat
          interface."

  if (status == 200) {
    jdat <- jsonlite::fromJSON(url)
  } else if (status == 400) {
    stop(
      "Failure to get data. Probably invalid dataset id. Status code: ",
      status, msg
    )
  } else if ((status == 500) | (status == 413)) {
    stop("Failure to get data. Probably filters did not return any data
         or data exceeded query size limitation. Status code: ", status, msg)
  } else if (status == 416) {
    stop(
      "Too many categories have been requested. Maximum is 50.",
      status, msg
    )
  } else {
    stop("Failure to get data. Status code: ", status, msg)
  }

  # get json data
  dims <- jdat$dimension
  ids <- jdat$id

  dims_list <- lapply(dims[rev(ids)], function(x) {
    y <- x$category$label
    y <- names(unlist(y))
  })

  variables <- expand.grid(dims_list,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = stringsAsFactors
  )

  dat <- as.data.frame(variables[rev(names(variables))])
  vals <- unlist(jdat$value, use.names = FALSE)
  dat$values <- rep(NA, nrow(dat))
  inds <- 1 + as.numeric(names(jdat$value)) # 0-indexed
  if (!length(vals) == length(inds)) {
    stop("Complex indexing not implemented.")
  }
  dat$values[inds] <- vals

  data <- tibble::as_tibble(dat) |>
    dplyr::mutate(time = as.Date(paste(time, "01", sep = "-")))
  return(data)
}

prepare_url <- function(id, filters) {
  # prepare filters for query
  filters2 <- as.list(unlist(filters))
  names(filters2) <- rep(names(filters), lapply(filters, length))
  
  # prepare url
  url_list <- list(
    scheme = "https",
    hostname = "ec.europa.eu",
    path = file.path(
      "eurostat/api/dissemination/statistics/1.0/data",
      id
    ),
    query = filters2
  )
  
  class(url_list) <- "url"
  return(httr::build_url(url_list))
}


# Yahoo Finance

get_data_from_yahoo <- function(data_info) {
  subset_lists <- Filter(function(x) x$source == "Yahoo", data_info)

  data <- mapply(function(x, name) {
    id_var <- gsub("^", "", x$id, fixed = TRUE)
    df <- quantmod::getSymbols(x$id, src = "yahoo", auto.assign = FALSE)

    df <- tryCatch(
      {
        df <- df |>
          tsbox::ts_tbl()
      },
      error = function(e) {
        cat("Bug with the Yahoo API, removing the last value...\n")
        df <- df[-nrow(df), ] |>
          tsbox::ts_tbl()
      }
    )
    df |>
      subset(id %in% paste(id_var, c("Volume", "Adjusted"), sep = ".")) |>
      tidyr::spread(id, value) |>
      dplyr::rename_at(dplyr::vars(dplyr::starts_with(id_var)), list(~ sub(id_var, name, .)))
  }, subset_lists, names(subset_lists), SIMPLIFY = FALSE)
  return(data)
}


# Google Trends
# See https://www.oecd-ilibrary.org/docserver/6b9c7518-en.pdf?expires=1681837213&id=id&accname=guest&checksum=31B9D9D14F31F4C9DFC38934AD9A4D96

get_gtrends <- function(country = "FR", category = 179) {
  gtrends_data <- gtrendsR::gtrends(
    geo = country,
    time = "all",
    gprop = "web",
    category = category,
    onlyInterest = TRUE
  )
  Sys.sleep(1)
  return(tsbox::ts_xts(gtrends_data$interest_over_time[, c("date", "geo", "hits")]))
}

get_rescaling_gtrends <- function(subset_lists) {
  rescaled_first_component_by_country <- list()
  for (country in subset_lists[[1]]$filters$geo) {
    print(country)
    Sys.sleep(1)
    list_hp_filtered_svi_ct <- list()

    for (x in subset_lists) {
      success <- FALSE
      while (!success) {
        Sys.sleep(1)
        tryCatch(
          {
            x_gtrends_data <- get_gtrends(
              country = country,
              category = x$category
            )
            success <- TRUE
          },
          error = function(e) {
            cat("Got a 429 error, pausing and retrying...\n")
            Sys.sleep(30)
          }
        )
      }

      # Creation of SVI and svi
      SVI_ct <- x_gtrends_data

      svi_ct <- log(10**(-10) + SVI_ct)
      hp_filtered_svi_ct <- xts::xts(mFilter::hpfilter(svi_ct, freq = 12, "frequency")[["trend"]], zoo::index(svi_ct))

      list_hp_filtered_svi_ct[[x$short_name]] <- hp_filtered_svi_ct
    }

    # Extracting the common component
    stacked_series <- do.call(merge, list_hp_filtered_svi_ct)
    pca <- prcomp(stacked_series, retx = TRUE, center = TRUE, scale = TRUE)
    first_component <- xts::xts(pca$x[, "PC1"], zoo::index(list_hp_filtered_svi_ct[[1]]))

    mean_svi <- xts::xts(
      apply(Reduce(merge, list_hp_filtered_svi_ct), 1, mean),
      zoo::index(list_hp_filtered_svi_ct[[1]])
    )
    standardised_first_component <- (first_component - mean(first_component)) / sd(first_component)
    rescaled_first_component <- standardised_first_component * sd(mean_svi) + mean(mean_svi)

    # Check the direction of the PCA
    trend_first_component <- lm(rescaled_first_component ~ time(rescaled_first_component))
    slope_first_component <- trend_first_component$coefficients[2]
    if (slope_first_component > 0) {
      rescaled_first_component <- -rescaled_first_component
    }

    rescaled_first_component_by_country[[country]] <- rescaled_first_component
  }
  return(rescaled_first_component_by_country)
}

get_data_from_google_trends <- function(data_info) {
  subset_lists <- Filter(function(x) x$source == "gtrends", data_info)
  if (length(subset_lists) > 0) {
    rescaled_first_component_by_country <- get_rescaling_gtrends(subset_lists)
  }

  data <- lapply(subset_lists, function(x) {
    gtrends_countries <- list()
    for (country in x$filters$geo) {
      print(country)
      success <- FALSE
      while (!success) {
        Sys.sleep(1)
        tryCatch(
          {
            x_gtrends_data <- get_gtrends(
              country = country,
              category = x$category
            )
            success <- TRUE
          },
          error = function(e) {
            cat("Got a 429 error, pausing and retrying...\n")
            Sys.sleep(30)
          }
        )
      }
      # Creation of SVI and svi

      SVI_ct <- x_gtrends_data
      svi_ct <- log(10**(-10) + SVI_ct)

      rescaled_first_component <- rescaled_first_component_by_country[[country]]

      # Correct the time series

      corrected_svi_ct <- svi_ct - rescaled_first_component
      corrected_SVI_ct <- exp(corrected_svi_ct)

      new_C <- 100 / max(corrected_SVI_ct)
      final_SVI_ct <- corrected_SVI_ct * new_C

      gtrends_df <- data.frame(final_SVI_ct)
      colnames(gtrends_df) <- c(x$short_name)
      gtrends_df <- cbind(time = rownames(gtrends_df), gtrends_df)
      gtrends_df["geo"] <- country
      gtrends_countries[[country]] <- gtrends_df
    }
    data_x <- bind_rows(gtrends_countries)
    rownames(data_x) <- NULL
    return(data_x)
  })

  return(data)
}


# Smaller sources of data

get_data_from_ember <- function(data_info) {
  subset_lists <- Filter(function(x) x$source == "ember-climate", data_info)

  data <- lapply(subset_lists, function(x) {
    countries_codes <- readr::read_csv(x[["url-geo-code"]]) |>
      dplyr::rename(
        geo = `Alpha-2 code`,
        geo_code_3 = `Alpha-3 code`
      ) |>
      dplyr::select(Country, geo, geo_code_3)

    data <- readr::read_csv(x$url) |>
      dplyr::rename(
        time = Date,
        ELEC_PRICES = `Price (EUR/MWhe)`
      ) |>
      dplyr::inner_join(countries_codes |>
        dplyr::select(-geo_code_3)) |>
      dplyr::select(geo, time, ELEC_PRICES)
  })
  return(data)
}


# Data for specific countries

# Retrieval of Destatis data on tolls mileage for road transport in Germany
get_data_from_destatis <- function(data_info) {
  subset_lists <- Filter(function(x) x$source == "Destatis", data_info)

  data <- lapply(subset_lists, function(x) {
    data_temp <- tempfile()
    download.file(
      subset_lists$TOLL_DE$url,
      data_temp
    )
    data <- readxl::read_excel(
      path = data_temp,
      sheet = "Daten",
      skip = 5
    ) |>
      rename(toll = paste0("Kalender- und saisonbereinigt (KSB)")) |>
      mutate(time = ymd(
        paste0(substr(Datum, 1, 4), substr(Datum, 6, 7), substr(Datum, 9, 10))
      )) |>
      mutate(geo = "DE") |>
      select(time, toll, geo)
  })
  return(data)
}

# Retrieval of data from the Austrian statistical institute
# Early indicator on industry form Wifo
get_data_from_wifo <- function(data_info) {
  subset_lists <- Filter(function(x) x$source == "Wifo", data_info)

  data <- lapply(subset_lists, function(x) {
    data_temp <- tempfile()
    download.file(
      subset_lists$WEEKLY_INDEX_AT$url,
      data_temp
    )
    data <- readxl::read_excel(
      path = data_temp,
      sheet = "Contributions_production",
      skip = 3
    ) |>
      rename(
        mois = paste0("...1"),
        semaine = paste0("...2"),
        wifo_ind = paste0("...4")
      ) |>
      select(mois, semaine, wifo_ind) |>
      mutate(annee = substr(mois, nchar(mois) - 3, nchar(mois)))

    an <- "2020"
    for (i in 1:nrow(data)) {
      if (is.na(data[i, 4])) {
        data[i, 4] <- an
      } else {
        an <- data[i, 4]
      }
    }

    data <- data[-1, ]
    data <- (subset(data, !is.na(data$semaine)))

    data <- data |>
      mutate(
        time = ymd(paste0(annee, "0101")) + weeks(substr(semaine, 3, 4)),
        geo = "AT"
      ) |>
      select(time, wifo_ind, geo)
  })
  return(data)
}


# Utils

get_weekend_days <- function(data_info, challenges_info) {
  date_to_pred <- lubridate::ymd(challenges_info$DATES$date_to_pred)
  subset_lists <- Filter(function(x) x$source == "Week-end", data_info)

  data <- lapply(subset_lists, function(x) {
    dates <- seq(as.Date(x[["init_date"]]), date_to_pred + months(1), by = "month")
    nb_weekend_days <- dplyr::tibble(
      month = lubridate::month(dates), year = lubridate::year(dates),
      weekends = numeric(length(dates))
    )
    for (i in 1:length(dates)) {
      month_start <- as.Date(paste(
        nb_weekend_days$year[i], nb_weekend_days$month[i], 1,
        sep = "-"
      ))
      month_end <- as.Date(paste(nb_weekend_days$year[i],
        nb_weekend_days$month[i],
        lubridate::days_in_month(month_start),
        sep = "-"
      ))
      nb_weekend_days$weekends[i] <- sum(
        lubridate::wday(seq(month_start, month_end, by = "day")) %in% c(7, 1)
      )
    }

    return(nb_weekend_days)
  })
  return(data)
}

get_data <- function(data_info, list_db) {
  list_data <- lapply(
    list_db,
    function(x) list(data = x)
  )

  return(Map(c, list_data, data_info))
}

read_data_from_s3 <- function(challenges_info, data_info) {
  month <- challenges_info$DATES$month_to_pred

  if (!dir.exists(paste0("data/", month))) {
    dir.create(paste0("data/", month))
  }

  data <- mapply(function(x, source) {
    url <- paste0("https://minio.lab.sspcloud.fr/projet-esa-nowcasting/data/", month, "/", source, ".parquet")
    destfile <- paste0("data/", month, "/", source, ".parquet")
    if (!file.exists(destfile)) {
      download.file(url, destfile)
    }
    return(arrow::read_parquet(destfile))
  }, data_info, names(data_info), SIMPLIFY = FALSE)

  return(
    get_data(
      data_info[order(names(data_info))],
      data[order(names(data))]
    )
  )
}
