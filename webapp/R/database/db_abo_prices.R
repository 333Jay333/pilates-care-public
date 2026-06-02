insert_abo_price <- function(con, course_id, abo_type, abo_price) {
  dbExecute(
    con,
    "INSERT OR IGNORE INTO abo_prices (course_id, abo_type, abo_price) VALUES (?,?,?)",
    params = list(
      course_id, abo_type, abo_price
    )
  )
}

update_abo_price <- function(con, course_id, abo_type, abo_price) {
  dbExecute(
    con,
    "
    UPDATE abo_prices
    SET abo_price = ?
    WHERE (course_id, abo_type) = (?,?)
    ",
    params = list(
      abo_price, course_id, abo_type
    )
  )
}

get_abo_price <- function(con, course_id, abo_type) {
  dbGetQuery(
    con,
    "SELECT abo_price FROM abo_prices WHERE (course_id, abo_type) = (?,?)",
    params = list(
      course_id, abo_type
    )
  )
}
