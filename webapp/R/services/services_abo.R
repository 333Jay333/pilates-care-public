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
