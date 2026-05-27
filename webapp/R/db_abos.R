insert_abo <- function(con, user_id, abo_type, abo_start) {
  dbExecute(
    con,
    "INSERT OR IGNORE INTO abos (user_id, abo_type, abo_start) VALUES (?,?,?)",
    params = list(
      user_id, abo_type, abo_start
    )
  )
}

insert_abo_end <- function(con, user_id, abo_type, abo_start, abo_end) {
  dbExecute(
    con,
    "INSERT INTO abos (user_id, abo_type, abo_start, abo_end) VALUES (?,?,?,?)",
    params = list(
      user_id, abo_type, abo_start, abo_end
    )
  )
}

update_abo_end <- function(con, user_id, abo_type, abo_start, abo_end) {
  dbExecute(
    con,
    "
    UPDATE abos
    SET abo_end = ?
    WHERE (user_id, abo_type, abo_start) = (?,?,?)
    ",
    params = list(
      abo_end,
      user_id, abo_type, abo_start
    )
  )
}

get_abos <- function(con) {
  dbGetQuery(
    con,
    "SELECT * FROM abos"
  )
}

get_abo_user_id <- function(con, user_id) {
  dbGetQuery(
    con,
    "SELECT * FROM abos WHERE user_id = ?",
    params = list(
      user_id
    )
  )
}

# query to get how many courses were attended already for members with 10er abo
get_attended_courses_abo_10 <- function(con) {
  dbGetQuery(
    con,
    "
    SELECT 
      m.user_id,
      m.vorname,
      m.name,
      COUNT(a.course_date_id) AS n_attended,
      (10 - COUNT(a.course_date_id)) AS still_left
    FROM members m
    JOIN abos ab 
      ON m.user_id = ab.user_id
    LEFT JOIN attendance a 
      ON m.user_id = a.user_id
    WHERE ab.abo_type = 10
    GROUP BY m.user_id, m.vorname, m.name
    "
  )
}

