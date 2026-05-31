insert_attendance <- function(con, course_date_id, user_id, abo_id) {
  dbExecute(
    con,
    "INSERT OR IGNORE INTO attendance 
      (course_date_id, user_id, abo_id) 
      VALUES (?,?,?)",
    params = list(
      course_date_id, 
      user_id,
      abo_id
    )
  )
} # ignore is important because a member might already have the attendance added

delete_attendance <- function(con, course_date_id, user_id) {
  dbExecute(
    con,
    "DELETE FROM attendance WHERE (course_date_id, user_id) = (?,?)",
    params = list(
      course_date_id,
      user_id
    )
  )
}

get_attendance_user_id <- function(con, user_id) {
  dbGetQuery(
    con,
    "SELECT * FROM attendance WHERE user_id = ?",
    params = list(
      user_id
    )
  )
}

get_attendance_course_id <- function(con, course_id) {
  dbGetQuery(
    con,
    "
    SELECT 
      cd.course_date,
      m.vorname,
      m.name,
      cd.course_date_id,
      m.user_id
    FROM attendance a
    JOIN members m 
      ON a.user_id = m.user_id
    JOIN course_dates cd 
      ON a.course_date_id = cd.course_date_id
    WHERE cd.course_id = ?
    ",
    params = list(
      course_id
    )
  )
}
