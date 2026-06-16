# R/services/services_abo.R
# This script contains functions related to the abos which are used across different modules

# function to add abo for a user
add_abo <- function(con, abo_type, user_id, abo_start) {
  if (abo_type == 10) {
    insert_abo(con, user_id, abo_type, abo_start) # call insert function for abo db
  } else { # calculate abo end automatically for 3 and 6 month abo
    if (abo_type == 3) {
      abo_end <- abo_start + 91
    } else if (abo_type == 6) {
      abo_end <- abo_start + 183
    } else {
      abo_end <- NULL
    }
    insert_abo_end(con, user_id, abo_type, abo_start, abo_end) # call insert function for abo db
  }
}

# function to prepare a badge column for usage in datatables
abo_badge <- function(abo_type) {
  styles <- list(
    "10er Abo"      = "background:#cfe2ff; color:#084298;",  # blue
    "3-Monats Abo"  = "background:#d1e7dd; color:#0a3622;",  # green
    "6-Monats Abo"  = "background:#e9d8fd; color:#44267a;"   # purple
  )
  
  # handle missing abo
  if (is.na(abo_type) || is.null(abo_type)) {
    return(sprintf(
      '<span style="background:#e9ecef; color:#6c757d; padding:2px 8px; border-radius:4px; font-size:11px;">kein Abo</span>'
    ))
  }
  
  style <- styles[[abo_type]]
  
  sprintf(
    '<span style="%s padding:2px 8px; border-radius:4px; font-size:11px;">%s</span>',
    style, abo_type
  )
}
