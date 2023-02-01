save_entries <- function(challenge, entries, challenges_info) {
  entries <- lapply(entries, function(x) {
    lapply(
      split(
        x$preds %>% pull(value, Country),
        names(x$preds %>% pull(value, Country))
      ),
      unname
    )
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
      paste0("mc cp Submissions/", challenge, "/results_", month, ".json"),
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

save_data <- function(data, challenges_info) {
  month <- challenges_info$DATES$month_to_pred

  if (!dir.exists(paste0("data/", month))) {
    dir.create(paste0("data/", month))
  }

  mapply(function(x, name) {
    filename <- paste0("data/", month, "/", name, ".parquet")
    arrow::write_parquet(
      x$data,
      filename
    )
    system(
      paste(
        paste("mc cp", filename),
        paste0("s3/projet-esa-nowcasting/", filename)
      )
    )
  }, data, names(data), SIMPLIFY = FALSE)

  paste0("data/", month)
}
