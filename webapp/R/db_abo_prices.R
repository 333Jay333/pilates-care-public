insert_abo_price <- function(con, course_id, abo_id, abo_price) {
  dbExecute(
    con,
    "INSERT INTO abo_prices (course_id, abo_id, abo_price) VALUES (?,?,?)",
    params = list(
      course_id, abo_id, abo_price
    )
  )
}

update_abo_price <- function(con, course_id, abo_id, abo_price) {
  dbExecute(
    con,
    "
    UPDATE abo_prices
    SET abo_price = ?
    WHERE (course_id, abo_id) = (?,?)
    ",
    params = list(
      abo_price, course_id, abo_id
    )
  )
}

get_abo_price <- function(con, course_id, abo_id) {
  dbGetQuery(
    con,
    "SELECT abo_price FROM abo_prices WHERE (course_id, abo_id) = (?,?)",
    params = list(
      course_id, abo_id
    )
  )
}
