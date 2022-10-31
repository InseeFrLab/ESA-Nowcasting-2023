library(rjson)
library(jsonlite)

save_entries <- function(entries, filename){
  file = rjson::toJSON(entries)
  write(jsonlite::prettify(file), filename)
}

add_entries <- function(entries, filename){
  current_file = rjson::fromJSON(paste0(readLines(filename), collapse = ""))
  c(current_file, entries)
  file = rjson::toJSON(c(current_file, entries))
  write(jsonlite::prettify(file), filename)
}
