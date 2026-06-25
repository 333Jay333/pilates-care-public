library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(pool)
library(here)
library(tidyverse)

# auto-restore renv on first launch
if (!requireNamespace("renv", quietly = TRUE) || !renv::status()$synchronized) {
  renv::restore(prompt = FALSE)
}

# Source scripts
list.files("R", recursive = TRUE, full.names = TRUE) |>
  walk(source)

# Source modules for webapp
list.files("modules", full.names = TRUE) |>
  walk(source)

# Set up db folder locally
if (!dir.exists(here("internal","db"))) {
  dir.create(here("internal","db"), recursive = TRUE)
}

# Set up connection to db
db <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = paste0(here("internal","db","db.sqlite"))
)

# initialise db
init_db(db)

# enable foreign keys for the db -> this is needed in SQLite such that DELETE CASCADES work
dbExecute(db, "PRAGMA foreign_keys = ON")

# ---- Close the connection to the db when the app stops ----
onStop(function() {
  poolClose(db)
})