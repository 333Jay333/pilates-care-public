insert_member <- function(con, kk, zv, vnr, vorname, name, adresse, plz, mail) {
  dbExecute(
    con,
    "INSERT OR IGNORE INTO members (kk, zv, vnr, vorname, name, adresse, plz, mail) VALUES (?,?,?,?,?,?,?,?)",
    params = list(
      kk, zv, vnr, vorname, name, adresse, plz, mail
    )
  )
}

delete_member_user_id <- function(con, user_id) {
  dbExecute(
    con, 
    "DELETE FROM members WHERE user_id = ?",
    params = list(user_id)
  )
}

set_member_status_user_id <- function(con, user_id, status) {
  dbExecute(
    con,
    "
    UPDATE members
    SET status = ?
    WHERE user_id = ?
    ",
    params = list(
      status,
      user_id
    )
  )
}

get_members <- function(con) {
  dbGetQuery(
    con,
    "SELECT * FROM members"
  )
}

get_active_members <- function(con) {
  dbGetQuery(
    con,
    "SELECT * FROM members WHERE status = ?",
    params = list("active")
  )
}

get_members_abo_10 <- function(con) {
  dbGetQuery(
    con,
    "
    SELECT
      m.user_id,
      m.vorname,
      m.name
    FROM attendance a
    JOIN members m
      ON a.user_id = m.user_id
    JOIN abos ab
      ON a.user_id = ab.user_id
    WHERE ab.abo_type = ?
    ",
    params = list(
      10 # 10er Abo has abo_type 10
    )
  )
}

get_members_abo_month <- function(con) {
  dbGetQuery(
    con,
    "
    SELECT
      m.user_id,
      m.vorname,
      m.name,
      ab.abo_end
    FROM abos ab
    JOIN members m
      ON ab.user_id = m.user_id
    WHERE ab.abo_type = ? 
      OR ab.abo_type = ?
    ",
    params = list(
      3, # 3 month abo has abo_type 3
      6 # 6 month abo has abo_type 6
    )
  )
}

get_member_user_id <- function(con, user_id) {
  dbGetQuery(
    con,
    "SELECT * FROM members WHERE user_id = ?",
    params = list(user_id)
  )
}

get_member_name_vnr <- function(con, name, vnr) {
  dbGetQuery(
    con,
    "SELECT * FROM members WHERE (name, vnr) = (?,?)",
    params = list(
      name, vnr
    )
  )
}

get_member_vorname_name <- function(con, vorname, name) {
  dbGetQuery(
    con,
    "SELECT * FROM members WHERE (vorname, name) = (?,?)",
    params = list(
      vorname, name
    )
  )
}


