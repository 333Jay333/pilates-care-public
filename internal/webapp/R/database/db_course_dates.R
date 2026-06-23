# has to have an adding function
# this will get triggered X amount of times -> based on course_dates add

# if 12 months -> 52 times
# otherwise month*4 times

insert_course_date <- function(con, course_id, course_date) {
  dbExecute(
    con,
    "INSERT OR IGNORE INTO course_dates (course_id, course_date) VALUES (?,?)",
    params = list(
      course_id, 
      course_date # better would be a format here: format(course_date, "%Y-%m-%d"). Now, everything gets stored as integer (days since 01.01.1970)
    )
  )
}

delete_course_date_id <- function(con, course_date_id) {
  dbExecute(
    con,
    "DELETE FROM course_dates WHERE course_date_id = ?",
    params = list(course_date_id)
  )
}

get_course_dates <- function(con) {
  dbGetQuery(
    con,
    "SELECT * FROM course_dates"
  )
}

get_course_dates_course_id <- function(con, course_id) {
  dbGetQuery(
    con,
    "SELECT * FROM course_dates WHERE course_id = ?",
    params = list(course_id)
  )
}

get_last_course_date_course_id <- function(con, course_id) {
  dbGetQuery(
    con,
    "SELECT course_date FROM course_dates WHERE course_id = ?
      ORDER BY course_date DESC LIMIT 1",
    params = list(course_id)
  )
}

get_course_dates_course_id_after_date <- function(con, course_id, after_date) {
  dbGetQuery(
    con,
    "
    SELECT * FROM course_dates
    WHERE course_id = ?
      AND course_date BETWEEN ? AND ?
    ORDER BY course_date
    ",
    params = list(
      course_id, 
      after_date,
      as.integer(today()) # because I made the error of saving dates an integers in my db
    )
  )
}

get_course_date_id_course_id_course_date <- function(con, course_id, course_date) {
  dbGetQuery(
    con,
    "
    SELECT course_date_id FROM course_dates
    WHERE course_id = ?
      AND course_date = ?
    ",
    params = list(
      course_id,
      course_date
    )
  )
}

