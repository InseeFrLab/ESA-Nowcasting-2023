library(rjson)
library(jsonlite)

save_entries <- function(entries, filename) {
  file <- rjson::toJSON(entries)
  write(jsonlite::prettify(file), filename)
}
