## global.R
library(pacman)
p_load(shiny)
p_load(tidyverse)
p_load(login)
p_load(shinyjs)
p_load(DBI)
p_load(RSQLite)

# Initialize database
db_conn <- dbConnect(RSQLite::SQLite(), "users.sqlite")

# Source our business logic
source("R/functions.R")
