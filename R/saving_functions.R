#' Data Saving functions
#'
#' This module provides a collection of functions for saving data and 
#' submissions to different destinations. It includes functions specifically 
#' designed for saving data to various file formats, databases, cloud storage. 
#' These functions facilitate the saving of data after processing or analysis, 
#' ensuring data persistence and accessibility for future use.

save_entries <- function(challenge, entries, challenges_info, save_s3) {
  entries <- lapply(entries, function(x) {
    lapply(
      split(
        x$preds |> pull(value, Country),
        names(x$preds |> pull(value, Country))
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
  if (save_s3) {
    aws.s3::put_object(
      file = paste0("Submissions/", challenge, "/results_", month, ".json"),
      bucket = "projet-esa-nowcasting", object = paste0("submissions/", challenge, "/results_", month, ".json"),
      region = ""
    )

    #### Save the data in S3 ####
    save(data, file = paste0("data_", challenge, "_", month, ".RData"))
    aws.s3::put_object(
      file = paste0("data_", challenge, "_", month, ".RData"),
      bucket = "projet-esa-nowcasting", object = paste0("data/", challenge, "/data_", month, ".RData"),
      region = ""
    )
  }
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

save_data <- function(data, challenges_info, save_s3) {
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

    if (save_s3) {
      aws.s3::put_object(
        file = filename,
        bucket = "projet-esa-nowcasting", object = filename,
        region = ""
      )
    }
  }, data, names(data), SIMPLIFY = FALSE)

  paste0("data/", month)
}


put_dir_s3 <- function(local_dir, s3_dir, bucket) {
  # Get a list of files in the local folder
  files_to_copy <- list.files(local_dir, recursive = TRUE)
  # Copy the files to your Minio bucket
  for (file in files_to_copy) {
    # Determine the s3_dir key and file path
    file_path <- paste0(local_dir, file)
    s3_dir_key <- paste0(s3_dir, file)
    # Copy the file to your Minio bucket
    aws.s3::put_object(file = file_path, object = s3_dir_key, bucket = bucket, region = "")
  }
}
