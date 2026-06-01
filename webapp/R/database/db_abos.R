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

update_abo_end <- function(con, abo_id, abo_end) {
  dbExecute(
    con,
    "
    UPDATE abos
    SET abo_end = ?
    WHERE abo_id = ?
    ",
    params = list(
      abo_end,
      abo_id
    )
  )
}

archive_abo <- function(con, abo_id) {
  dbExecute(
    con,
    "
    UPDATE abos
    SET abo_status = 'archived'
    WHERE abo_id = ?
    ",
    params = list(
      abo_id
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

get_active_abos <- function(con) {
  dbGetQuery(
    con,
    "
    SELECT 
      ab.abo_id,
      ab.user_id,
      ab.abo_type,
      ab.abo_start,
      ab.abo_end,
      m.vorname,
      m.name
    FROM abos ab
    JOIN members m
      ON ab.user_id = m.user_id
    WHERE ab.abo_status = 'active'
    "
  )
}

get_active_abo_user_id <- function(con, user_id) {
  dbGetQuery(
    con,
    "
    SELECT * FROM abos 
    WHERE user_id = ?
      AND abo_status = 'active'
    ",
    params = list(
      user_id
    )
  )
}

# query to get how many courses were attended already for members with 10er abo
# only selects courses if the match the abo_id of the currently active abo
get_attended_courses_abo_10 <- function(con) {
  dbGetQuery(
    con,
    "
    SELECT 
      m.user_id,
      m.vorname,
      m.name,
      ab.abo_id,
      COUNT(a.course_date_id) AS n_attended,
      (10 - COUNT(a.course_date_id)) AS still_left
    FROM members m
    JOIN abos ab 
      ON m.user_id = ab.user_id
    LEFT JOIN attendance a 
      ON m.user_id = a.user_id
    WHERE ab.abo_type = 10
      AND ab.abo_status = 'active'
      AND a.abo_id = ab.abo_id
    GROUP BY m.user_id, m.vorname, m.name
    "
  )
}

# query to get how many courses were attended already for members with 10er abo
# only selects courses if the match the abo_id of the currently active abo
get_attended_courses_abo_10_user_id <- function(con, user_id) {
  dbGetQuery(
    con,
    "
    SELECT 
      m.user_id,
      m.vorname,
      m.name,
      ab.abo_id,
      COUNT(a.course_date_id) AS n_attended,
      (10 - COUNT(a.course_date_id)) AS still_left
    FROM members m
    JOIN abos ab 
      ON m.user_id = ab.user_id
    LEFT JOIN attendance a 
      ON m.user_id = a.user_id
    WHERE ab.abo_type = 10
      AND ab.abo_status = 'active'
      AND a.abo_id = ab.abo_id
      AND m.user_id = ?
    GROUP BY m.user_id, m.vorname, m.name
    ",
    params = list(
      user_id
    )
  )
}



