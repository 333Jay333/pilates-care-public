insert_course_member <- function(con, user_id, course_id) {
  dbExecute(
    con,
    "INSERT OR IGNORE INTO course_memberships (user_id, course_id) VALUES (?,?)",
    params = list(
      user_id, course_id
    )
  )
}

get_course_members_course_id <- function(con, course_id) {
  dbGetQuery(
    con, 
    "
    SELECT 
      cm.user_id 
    FROM course_memberships cm
    JOIN members m 
      ON cm.user_id = m.user_id
    JOIN abos ab
      ON cm.user_id = ab.user_id
    WHERE cm.course_id = ?
      AND ab.abo_status = 'active'
    ",
    params = list(
      course_id
    )
  )
}

get_course_membership_user_id <- function(con, user_id) {
  dbGetQuery(
    con,
    "SELECT * FROM course_memberships WHERE user_id = ?",
    params = list(
      user_id
    )
  )
}
