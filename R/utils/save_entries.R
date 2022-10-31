library(rjson)
library(jsonlite)

save_entries <- function(entries, filename){
  file = rjson::toJSON(entries)
  write(jsonlite::prettify(file), filename)
}

add_entries <- function(entries, filename){
  current_file = rjson::fromJSON(paste0(readLines(filename), collapse = ""))
  file = rjson::toJSON(c(current_file, entries))
  write(jsonlite::prettify(file), filename)
}
reorder_entries <- function(entries, filename){
  current_file = rjson::fromJSON(paste0(readLines(filename), collapse = ""))
  current_file = current_file[entries]
  names(current_file) = sprintf("entry_%i", 1:length(current_file))
  file = rjson::toJSON(current_file)
  write(jsonlite::prettify(file), filename)
}
