library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(pool)
library(here)
library(tidyverse)

source("R/init_db.R") # sets up the tables in my db
source("R/db_therapists.R")
source("R/db_members.R")
source("R/db_courses.R")
source("R/db_course_memberships.R")
source("R/db_abos.R")
source("R/db_course_dates.R")
source("R/db_attendance.R")
source("modules/mod_therapists.R") # module for therapists
source("modules/mod_members.R")
source("modules/mod_certificate.R")
source("modules/mod_courses.R")
source("modules/mod_attendance.R")
source("modules/mod_abo_dashboard.R")
source("modules/mod_abos.R")

# Set up db folder locally
if (!dir.exists(here("db"))) {
  dir.create(here("db"), recursive = TRUE)
}

# Set up connection to db
db <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = paste0(here("db","db.sqlite"))
)

# initialise db
init_db(db)

# ---- Close the connection to the db when the app stops ----
onStop(function() {
  poolClose(db)
})