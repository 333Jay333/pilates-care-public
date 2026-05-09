insert_therapist <- function(con, vorname, name, praxis, adresse, plz, tel, mail, zsr, knr, emfit, pilat_nr) {
  dbExecute(
    con,
    "INSERT INTO therapists (vorname, name, praxis, adresse, plz, tel, mail, zsr, knr, emfit, pilat_nr) VALUES (?,?,?,?,?,?,?,?,?,?,?)",
    params = list(
      vorname, name, praxis, adresse, plz, tel, mail, zsr, knr, emfit, 
      as.integer(pilat_nr)
    )
  )
}

get_therapists <- function(con) {
  dbGetQuery(
    con,
    "SELECT * FROM therapists"
  )
}

delete_therapist_user_id <- function(con, user_id) {
  dbExecute(
    con, 
    "DELETE FROM therapists WHERE user_id = ?",
    params = list(user_id)
  )
}

get_therapist_user_id <- function(con, user_id) {
  dbGetQuery(
    con,
    "SELECT * FROM therapists WHERE user_id = ?",
    params = list(user_id)
  )
}

get_therapist_vorname <- function(con, vorname) {
  dbGetQuery(
    con,
    "SELECT * FROM therapists WHERE vorname = ?",
    params = list(vorname)
  )
}