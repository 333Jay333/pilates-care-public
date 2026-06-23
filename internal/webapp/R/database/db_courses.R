insert_course <- function(con, kursname, location) {
  dbExecute(
    con,
    "INSERT OR IGNORE INTO courses (kursname, location) VALUES (?,?)",
    params = list(
      kursname, location
    )
  )
}

get_courses <- function(con) {
  dbGetQuery(
    con,
    "SELECT * FROM courses"
  )
}

get_course_id <- function(con, course_id) {
  dbGetQuery(
    con,
    "SELECT * FROM courses WHERE course_id = ?",
    params = list(course_id)
  )
}

get_course_id_kursname <- function(con, kursname) {
  dbGetQuery(
    con,
    "SELECT course_id FROM courses WHERE kursname = ?",
    params = list(kursname)
  )
}

delete_course_id <- function(con, course_id) {
  dbExecute(
    con,
    "DELETE FROM courses WHERE course_id = ?",
    params = list(course_id)
  )
}