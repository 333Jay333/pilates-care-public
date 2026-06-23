format_swiss_date <- function(x) {
  format(as.Date(x), "%d.%m.%Y")
}

format_swiss_date_with_origin <- function(x) {
  ifelse(x != 0, format(as.Date(x, origin = "1970-01-01"), "%d.%m.%Y"), "")
}

# takes a year and a weekday coded as integer (Monday = 1, ..., Sunday = 7) and returns the date of the first weekday of that years
first_weekday_of_year <- function(year, weekday) {
  jan1 <- as.Date(paste0(year, "-01-01"))
  jan1_wday <- as.integer(format(jan1, "%u")) 
  offset <- (weekday - jan1_wday) %% 7
  jan1 + offset
}
