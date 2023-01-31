get_latest_dates <- function(data, var) {
  # Returns a list with last available value for each variable of the xts dataset
  return(as.character(last(zoo::index(data)[!is.na(data[, var])])))
}

theme_custom <- function() {
  dark_grey <- rgb(83, 83, 83, maxColorValue = 255)
  light_grey <- rgb(217, 217, 217, maxColorValue = 255)
  dark_blue <- "#003299"
  theme_minimal() %+replace%
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      axis.ticks = element_line(color = dark_grey),
      axis.ticks.length = unit(5, "pt"),
      legend.position = "bottom",
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 10),
      legend.text = element_text(size = 10),
      plot.caption = element_text(hjust = 0, size = 8, colour = dark_blue),
      plot.subtitle = element_text(size = 10, colour = dark_blue),
      plot.title = element_text(size = 12, face = "bold", colour = dark_blue),
      panel.background = element_rect(colour = light_grey),
      axis.line = element_line(color = dark_grey),
      legend.margin = margin(t = -0.1, b = -0.1, unit = "cm"),
      legend.key.size = unit(0.2, "cm"),
      legend.key.width = unit(0.4, "cm")
    )
}

subplot_pred <- function(sample, country, xlim, predictions, legend = F) {
  plot <- ggplot() +
    ggtitle(country) +
    geom_line(data = subset(sample, geo %in% country), aes(x = as.POSIXct(time, format = "%Y-%m-%d"), y = values)) +
    geom_point(data = subset(predictions, Country %in% country), aes(x = as.POSIXct(Date, format = "%Y-%m-%d"), y = value, color = Entries)) +
    scale_x_datetime(limits = as.POSIXct(c(xlim, NA), format = "%Y-%m-%d")) +
    scale_color_manual("", values = Palette_col) +
    theme_custom() +
    {
      if (!legend) {
        theme(legend.position = "none")
      }
    }
  
  return(plot)
}

plot_preds <- function(challenge, challenges_info, data_info, predictions, Countries, xlim = "2020-01-01", ncol = 2) {
  code_variable_interest <- challenges_info[[challenge]]$principal_nace
  
  sample <- data_info[[challenge]]$data %>%
    dplyr::filter((nace_r2 %in% code_variable_interest) & (geo %in% Countries))
  
  ListPlots <- sapply(Countries, subplot_pred, sample = sample, xlim = xlim, predictions = predictions, simplify = FALSE)
  
  legend <- get_legend(
    subplot_pred(sample, Countries[1], xlim, predictions, legend = T)
    + theme(legend.box.margin = margin(0, 0, 0, 0))
  )
  
  prow <- plot_grid(plotlist = ListPlots, align = "h", ncol = ncol, vjust = -0.8)
  
  plot <- plot_grid(prow + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                    legend,
                    ncol = 1, rel_heights = c(1, 0.1)
  )
  return(plot)
}

get_metrics <- function(sample, Countries, up_date, low_date) {
  setDT(sample)
  sample <- sample[
    (Country %in% Countries) &
      (Date %between% c(low_date, up_date)),
    .(
      ME = mean(value, na.rm = TRUE),
      N = as.double(sum(!is.na(value))),
      MAE = mean(abs(value), na.rm = TRUE),
      RMSE = sqrt(mean(value^2, na.rm = TRUE))
    ),
    by = .(Entries)
  ]

  sample <- melt(sample, id.vars = c("Entries"), value.name = "value", variable.name = "Statistic")

  return(sample)
}

subplot_statistic <- function(sample, statistic, legend = TRUE, y_labs = TRUE) {
  sample <- sample[, c("Entries", "Statistic") := list(
    factor(Entries, levels = c("Naive", "SARIMA", "REG-ARIMA", "XGBoost", "XGBoost_diff", "DFM", "ETS")),
    factor(Statistic, levels = c("N", "ME", "MIN", "MAX", "MAE", "RMSE"))
  )]

  temp <- sample[(Statistic == "N")][order(Entries)]

  NewTitle <- paste0(temp$Entries, "\n(", temp$value, ")")
  names(NewTitle) <- temp$Entries

  sample <- sample[(Statistic != "N")][, Entries := factor(dplyr::recode(Entries, !!!NewTitle), levels = NewTitle)]

  plot <- ggplot(data = subset(sample, Statistic == statistic)) +
    ggtitle(statistic) +

    # makes the bar and format
    geom_bar(aes(x = Entries, y = value, fill = Entries), stat = "identity", position = "dodge", width = 0.8) +
    geom_hline(yintercept = 0) +
    # Add labels
    geom_text(aes(x = Entries, y = value, vjust = ifelse(value > 0, -0.2, 1.15), label = round(value, 2)), size = 3) +
    # set general theme
    theme_custom() +
    theme(plot.title = element_text(size = 9, face = "plain", colour = "black")) +
    {
      if (!legend) {
        theme(legend.position = "none")
      }
    } +
    {
      if (!y_labs) {
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      }
    } +
    # set colors and name of data
    scale_fill_manual("", values = Palette_col)

  return(plot)
}

plot_statistics <- function(sample) {
  Plot1 <- subplot_statistic(sample, "ME", FALSE, TRUE)
  Plot2 <- subplot_statistic(sample, "MAE", FALSE, FALSE)
  Plot3 <- subplot_statistic(sample, "RMSE", FALSE, FALSE)

  legend <- cowplot::get_legend(
    subplot_statistic(sample, "ME", TRUE, FALSE)
    + theme(legend.box.margin = margin(-15, 0, 0, 0))
  )

  prow <- plot_grid(Plot1,
    Plot2,
    Plot3,
    align = "h", ncol = 3, vjust = -0.8
  )

  plot <- plot_grid(legend,
    prow,
    ncol = 1, rel_heights = c(0.1, 1)
  )

  return(plot)
}

save_entries <- function(challenge, entries, challenges_info) {
  
  entries <- lapply(entries, function(x){
    lapply(split(x$preds %>% pull(value, Country), 
                 names(x$preds %>% pull(value, Country))), 
           unname)
  })
  month <- challenges_info$DATES$month_to_pred
  filename <- paste0("Submissions/", challenge, "/results_", month, ".json")
  
  if (file.exists(filename)) {
    current_file <- rjson::fromJSON(file = filename)
    current_file[names(entries)] <- entries
    file <- rjson::toJSON(current_file)
    write(jsonlite::prettify(file), filename)
  } else {
    file <- rjson::toJSON(entries)
    write(jsonlite::prettify(file), filename)
  }
  
  ## Save in S3 the json
  system(
    paste(
      paste0("mc cp Submissions/", challenge,"/results_", month, ".json"),
      paste0("s3/projet-esa-nowcasting/submissions/", challenge, "/results_", month, ".json")
    )
  )
  
  #### Save the data in S3 ####
  save(data, file = paste0("data_", challenge, "_", month, ".RData"))
  system(
    paste(
      paste0("mc cp data_", challenge, "_", month, ".RData"),
      paste0("s3/projet-esa-nowcasting/data/", challenge, "/data_", month, ".RData")
    )
  )
}

## Customize palette
pal_col <- rbind(
  c(0, 50, 153),
  c(255, 180, 0),
  c(255, 75, 0),
  c(101, 184, 0),
  c(0, 177, 234),
  c(0, 120, 22),
  c(129, 57, 198),
  c(92, 92, 92),
  c(152, 161, 208),
  c(253, 221, 167),
  c(246, 177, 131),
  c(206, 225, 175),
  c(215, 238, 248),
  c(141, 184, 141),
  c(174, 151, 199),
  c(169, 169, 169),
  c(217, 217, 217)
)

Palette_col <- rgb(pal_col[, 1], pal_col[, 2], pal_col[, 3], maxColorValue = 255)

add_entries <- function(entries, filename) {
  current_file <- rjson::fromJSON(paste0(readLines(filename), collapse = ""))
  file <- rjson::toJSON(c(current_file, entries))
  write(jsonlite::prettify(file), filename)
}

reorder_entries <- function(entries, filename) {
  current_file <- rjson::fromJSON(paste0(readLines(filename), collapse = ""))
  current_file <- current_file[entries]
  names(current_file) <- sprintf("entry_%i", 1:length(current_file))
  file <- rjson::toJSON(current_file)
  write(jsonlite::prettify(file), filename)
}

to_tsibble <- function(x) {
  x %>%
    mutate(time = tsibble::yearmonth(time)) %>%
    tidyr::drop_na() %>%
    tsibble::as_tsibble(key = c(geo), index = time)
}
