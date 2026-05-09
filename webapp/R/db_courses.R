insert_course <- function(con, kursname, location) {
  dbExecute(
    con,
    "INSERT INTO courses (kursname, location) VALUES (?,?)",
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

delete_course_id <- function(con, course_id) {
  dbExecute(
    con,
    "DELETE FROM courses WHERE course_id = ?",
    params = list(course_id)
  )
}