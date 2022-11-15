get_latest_dates <- function(data, var){
  # Returns a list with last available value for each variable of the xts dataset
  return(as.character(last(index(data)[!is.na(data[, var])])))
}
