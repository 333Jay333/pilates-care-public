#!/bin/bash

cd "$(dirname "$0")"

# renv::restore() needs to run from the root where renv.lock lives
Rscript -e "renv::restore(prompt = FALSE); setwd('internal/webapp'); shiny::runApp('.', launch.browser = TRUE)"