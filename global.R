## global.R
library(pacman)
p_load(shiny)
p_load(tidyverse)
p_load(login)
p_load(shinyjs)
p_load(DBI)
p_load(RSQLite)
p_load(bslib)

# The `login` package builds its login box on the server and wraps it in
# `enclosing_panel` (its default is shiny::wellPanel()). We hand it a bslib
# card instead - see login_server() in server.R.
login_card <- function(...) {
  bslib::card(
    bslib::card_header("Sign in"),
    bslib::card_body(...)
  )
}

# Initialize database
db_conn <- dbConnect(RSQLite::SQLite(), "users.sqlite")

# Source our business logic
source("R/functions.R")
