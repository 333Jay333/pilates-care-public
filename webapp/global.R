library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(pool)
library(here)
library(tidyverse)

# Source scripts
list.files("R", recursive = TRUE, full.names = TRUE) |>
  walk(source)

# Source modules for webapp
list.files("modules", full.names = TRUE) |>
  walk(source)

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

# enable foreign keys for the db -> this is needed in SQLite such that DELETE CASCADES work
dbExecute(db, "PRAGMA foreign_keys = ON")

# ---- Close the connection to the db when the app stops ----
onStop(function() {
  poolClose(db)
})