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
# card instead - see login_server() in server.R.
#
# One function serves the sign-in, sign-up and password-reset panels, and each
# of those passes through several states (form -> "enter the code from the
# email" -> new password), so the heading must come from an identity that
# outlives the state. It is taken from the output currently being rendered,
# which never changes across a flow; when there is no render context to read
# (a console call, a test) it falls back to the panel's *inputs*, which unlike
# the buttons are present in every state.
login_card <- function(...) {
  bslib::card(
    bslib::card_header(panel_title(list(...))),
    do.call(bslib::card_body, list(...))
  )
}

panel_title <- function(dots) {
  info  <- tryCatch(getCurrentOutputInfo(), error = function(e) NULL)
  name  <- if (is.list(info) && !is.null(info$name)) info$name else ""

  if (grepl("new_user_ui$", name))       return("Create account")
  if (grepl("reset_password_ui$", name)) return("Reset password")
  if (grepl("login_ui$", name))          return("Sign in")

  html <- paste(vapply(dots, function(x) paste(as.character(x), collapse = ""),
                       character(1)),
                collapse = "")

  # Input ids are namespaced as <id>-... and are stable within a panel:
  # new_username / new_user_code / new_password* vs forgot_password_email /
  # reset_password* vs the sign-in panel's plain username/password.
  if (grepl("login-new_", html, fixed = TRUE)) return("Create account")
  if (grepl("login-reset_password", html, fixed = TRUE) ||
      grepl("login-forgot_password", html, fixed = TRUE)) return("Reset password")

  "Sign in"
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

# Sign-up may only be offered when it can be verified by email. Without an
# emailer login_server() would take new accounts immediately (its default is
# verify_email = !is.null(emailer)), so ui.R shows the create-account card only
# when this is TRUE, and server.R forces verify_email on as a backstop.
signup_enabled <- function(emailer = app_emailer) !is.null(emailer)

if (is.null(app_emailer)) {
  message("GMAIL_USER / GMAIL_PASS not set: email verification and ",
          "password reset are disabled.")
}

# Initialize database
db_conn <- dbConnect(RSQLite::SQLite(), "users.sqlite")

# Source our business logic
source("R/functions.R")
