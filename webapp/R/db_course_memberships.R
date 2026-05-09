insert_course_member <- function(con, user_id, course_id) {
  dbExecute(
    con,
    "INSERT INTO course_memberships (user_id, course_id) VALUES (?,?)",
    params = list(
      user_id, course_id
    )
  )
}

get_course_members_course_id <- function(con, course_id) {
  dbGetQuery(
    con, 
    "
    SELECT cm.user_id FROM course_memberships cm
    JOIN members m ON cm.user_id = m.user_id
    WHERE cm.course_id = ?
    ",
    params = list(
      course_id
    )
  )
}
