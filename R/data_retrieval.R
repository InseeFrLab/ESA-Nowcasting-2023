get_data_from_eurostat <- function(data_info) {
  subset_lists <- Filter(function(x) x$source == "Eurostat", data_info)

  data <- lapply(subset_lists, function(x) {
    get_eurostat_json_thomas(x$id,
      filters = x$filters
    ) |>
      dplyr::select(geo, names(x$filters)[3], time, values) |>
      tidyr::drop_na(values)
  })

  return(data)
}

get_data_from_yahoo <- function(data_info) {
  subset_lists <- Filter(function(x) x$source == "Yahoo", data_info)

  data <- mapply(function(x, name) {
    id_var <- gsub("^", "", x$id, fixed = TRUE)
    quantmod::getSymbols(x$id, src = "yahoo", auto.assign = FALSE) |>
      tsbox::ts_tbl() |>
      subset(id %in% paste(id_var, c("Volume", "Adjusted"), sep = ".")) |>
      tidyr::spread(id, value) |>
      dplyr::rename_at(dplyr::vars(dplyr::starts_with(id_var)), list(~ sub(id_var, name, .)))
  }, subset_lists, names(subset_lists), SIMPLIFY = FALSE)
  return(data)
}

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

get_weekend_days <- function(data_info, challenges_info) {
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)
  subset_lists <- Filter(function(x) x$source == "Week-end", data_info)

  data <- lapply(subset_lists, function(x) {
    dates <- seq(as.Date(x[["init_date"]]), date_to_pred + months(1), by = "month")
    nb_weekend_days <- dplyr::tibble(
      month = month(dates), year = year(dates),
      weekends = numeric(length(dates))
    )
    for (i in 1:length(dates)) {
      month_start <- as.Date(paste(
        nb_weekend_days$year[i], nb_weekend_days$month[i], 1,
        sep = "-"
      ))
      month_end <- as.Date(paste(nb_weekend_days$year[i],
        nb_weekend_days$month[i],
        days_in_month(month_start),
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


get_data <- function(data_info = yaml::read_yaml("data.yaml"),
                     challenges_info = yaml::read_yaml("challenges.yaml")) {
  eurostat <- get_data_from_eurostat(data_info)
  yahoo <- get_data_from_yahoo(data_info)
  ember <- get_data_from_ember(data_info)
  week_ends <- get_weekend_days(data_info, challenges_info)

  list_data <- lapply(
    c(eurostat, yahoo, ember, week_ends),
    function(x) list(data = x)
  )

  return(Map(c, list_data, data_info))
}

get_eurostat_json_thomas <- function(id, filters = NULL,
                                     type = c("code", "label", "both"),
                                     lang = c("en", "fr", "de"),
                                     stringsAsFactors = FALSE,
                                     ...) {
  # get response
  # url <- try(eurostat_json_url(id = id, filters = filters, lang = lang))
  # if (class(url) == "try-error") { stop(paste("The requested data set cannot be found with the following specifications to get_eurostat_json function: ", "id: ", id, "/ filters: ", filters, "/ lang: ", lang))  }
  url <- eurostat_json_url_thomas(id = id, filters = filters, lang = lang)

  # resp <- try(httr::GET(url, ...))
  # if (class(resp) == "try-error") { stop(paste("The requested url cannot be found within the get_eurostat_json function:", url))  }
  resp <- httr::RETRY("GET", url, terminate_on = c(404))
  if (httr::http_error(resp)) {
    stop(paste("The requested url cannot be found within the get_eurostat_json function:", url))
  }

  status <- httr::status_code(resp)

  # check status and get json

  msg <- ". Some datasets are not accessible via the eurostat
          interface. You can try to search the data manually from the comext
  	  database at http://epp.eurostat.ec.europa.eu/newxtweb/ or bulk
  	  download facility at
  	  http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing
  	  or annual Excel files
  	  http://ec.europa.eu/eurostat/web/prodcom/data/excel-files-nace-rev.2"

  if (status == 200) {
    jdat <- jsonlite::fromJSON(url)
  } else if (status == 400) {
    stop(
      "Failure to get data. Probably invalid dataset id. Status code: ",
      status, msg
    )
  } else if (status == 500) {
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
  # dims <- jdat[[1]]$dimension # Was called like this in API v1.1
  dims <- jdat$dimension # Switched to this with API v2.1
  # ids <- dims$id # v1.1
  ids <- jdat$id # v2.1

  dims_list <- lapply(dims[rev(ids)], function(x) {
    y <- x$category$label
    if (type[1] == "label") {
      y <- unlist(y, use.names = FALSE)
    } else if (type[1] == "code") {
      y <- names(unlist(y))
    } else if (type[1] == "both") {
      y <- unlist(y)
    } else {
      stop("Invalid type ", type)
    }
  })

  variables <- expand.grid(dims_list,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = stringsAsFactors
  )

  # dat <- data.frame(variables[rev(names(variables))], values = jdat[[1]]$value) # v1.1
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

eurostat_json_url_thomas <- function(id, filters, lang) {
  # prepare filters for query
  filters2 <- as.list(unlist(filters))
  names(filters2) <- rep(names(filters), lapply(filters, length))

  # prepare url
  url_list <- list(
    scheme = "http",
    hostname = "ec.europa.eu",
    path = file.path(
      "eurostat/api/dissemination/statistics/1.0/data",
      id
    ),
    query = filters2
  )

  class(url_list) <- "url"
  url <- httr::build_url(url_list)
  url
}
