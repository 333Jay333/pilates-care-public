format_swiss_date <- function(x) {
  format(as.Date(x), "%d.%m.%Y")
}

format_swiss_date_with_origin <- function(x) {
  format(as.Date(x, origin = "1970-01-01"), "%d.%m.%Y")
}
