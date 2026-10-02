## global.R
library(pacman)
p_load(shiny)
p_load(tidyverse)
p_load(login)
p_load(shinyjs)
p_load(DBI)
p_load(RSQLite)
p_load(bslib)

# The `login` package builds its boxes on the server and wraps them in
# `enclosing_panel` (its default is shiny::wellPanel()). We hand it a bslib
# card instead - see login_server() in server.R. The same function wraps the
# sign-in, sign-up and password-reset panels, so pick a heading from what the
# panel actually contains.
login_card <- function(...) {
  dots <- list(...)
  html <- paste(vapply(dots, function(x) paste(as.character(x), collapse = ""),
                       character(1)),
                collapse = "")

  # Matched case-insensitively because these labels come from login_server()'s
  # create_account_label / the package's "Send reset code" button.
  title <- if (grepl("create account", html, ignore.case = TRUE)) {
    "Create account"
  } else if (grepl("send reset code", html, ignore.case = TRUE)) {
    "Reset password"
  } else {
    "Sign in"
  }

  bslib::card(
    bslib::card_header(title),
    do.call(bslib::card_body, dots)
  )
}

# ── Email (Gmail SMTP) ───────────────────────────────────────
# Credentials come from the Windows *user* environment variables GMAIL_USER and
# GMAIL_PASS (a Gmail App Password, not the account password) plus the optional
# GMAIL_HOST / GMAIL_PORT overrides. They are deliberately read from the
# environment so nothing secret ever lands in this repository.
#
# Returns NULL when either variable is missing, which switches login_server()
# into its no-email mode: sign-ups are accepted immediately and the
# password-reset panel reports that no email server is configured.
make_app_emailer <- function(username = Sys.getenv("GMAIL_USER"),
                             password = Sys.getenv("GMAIL_PASS"),
                             host     = Sys.getenv("GMAIL_HOST", "smtp.gmail.com"),
                             port     = Sys.getenv("GMAIL_PORT", "465")) {
  if (!nzchar(username) || !nzchar(password)) {
    return(NULL)
  }

  login::emayili_emailer(
    email_host     = host,
    email_port     = as.integer(port),
    email_username = username,
    email_password = password,
    from_email     = username
  )
}

app_emailer <- make_app_emailer()

if (is.null(app_emailer)) {
  message("GMAIL_USER / GMAIL_PASS not set: email verification and ",
          "password reset are disabled.")
}

# Initialize database
db_conn <- dbConnect(RSQLite::SQLite(), "users.sqlite")

# Source our business logic
source("R/functions.R")
