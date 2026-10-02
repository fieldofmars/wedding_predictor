## Server-level tests: the validation wiring in server.R
#
# tests/testthat/test_functions.R covers the helpers in R/functions.R
# (quantile_problem(), fit_lognormal_from_quantiles()). What it cannot cover is
# the Shiny layer: that server.R actually feeds the helper's message through
# validate()/renderUI so the user sees it. These tests drive the real server
# function through shiny::testServer() to guard that wiring.

library(testthat)
library(shiny)

# The app files (global.R, server.R, R/) live at the project root, which is not
# necessarily the working directory testthat is running from.
app_root <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "global.R")) && dirname(d) != d) {
    d <- dirname(d)
  }
  d
})
stopifnot(file.exists(file.path(app_root, "global.R")))

old_wd <- setwd(app_root)

# Load the app the way shiny::runApp() does: global.R brings in the packages,
# the SQLite connection and the business logic; server.R evaluates to the
# server function (shinyServer() returns its argument).
source("global.R", local = FALSE)
server_fn <- source("server.R", local = FALSE)$value
setwd(old_wd)

# A server function built with a chosen emailer, so tests of the email-dependent
# paths do not depend on whether this machine has GMAIL_USER / GMAIL_PASS set.
# server.R reads app_emailer when the function runs, so sourcing it into an
# environment that defines app_emailer overrides the value global.R produced.
server_with_emailer <- function(emailer) {
  env <- new.env(parent = globalenv())
  env$app_emailer <- emailer
  source(file.path(app_root, "server.R"), local = env)$value
}

# Every input the server reads, with the app's defaults.
base_inputs <- list(
  # Dates must be Date objects: that is what Shiny hands the server for a
  # dateInput, and server.R feeds them straight into seq(by = "1 month").
  lodgement_date     = as.Date("2026-10-02"),
  ceremony_date_pick = as.Date("2028-02-02"),
  impatience         = 5,
  search_range       = c(6, 30),
  q50                = 12,
  q90                = 23,
  booking_cost       = 400,
  resched_cost       = 200,
  mc_n_sims          = 1000,
  mc_seed            = 42
)

# An output can deliver its value or, when validate() rejects the inputs, the
# silent error carrying the message that would be rendered. Accept either, so
# the assertions describe the user-visible text rather than Shiny internals.
output_text <- function(expr) {
  tryCatch(
    paste(as.character(expr), collapse = " "),
    error = function(e) conditionMessage(e)
  )
}

# login_server() polls cookies::get_cookie() on every flush. That helper insists
# on a real ShinySession (it cannot resolve one under shiny::testServer's mock
# session) and its error takes neighbouring outputs down with it. Stub the call
# for the duration of a test - cookies play no part in input validation.
local_stub_cookies <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    get_cookie = function(...) NULL,
    .package = "cookies",
    .env = .env
  )
}

# Overwrite a base input rather than appending a duplicate name to setInputs().
with_inputs <- function(...) {
  modifyList(base_inputs, list(...))
}

test_that("dist_params() rejects q90 <= q50 with the friendly message", {
  local_stub_cookies()
  shiny::testServer(server_fn, {
    session$setInputs(q50 = 12, q90 = 8)

    # validate() signals a silent error - the same mechanism every output uses
    # to display a friendly message instead of a red Shiny error.
    expect_error(dist_params(), class = "shiny.silent.error")
    expect_error(dist_params(),
                 regexp = "90% figure must be larger than the 50% figure",
                 class = "shiny.silent.error")
  })
})

test_that("dist_params() still fits a valid pair", {
  local_stub_cookies()
  shiny::testServer(server_fn, {
    do.call(session$setInputs, base_inputs)

    params <- dist_params()
    expect_true(is.list(params))
    expect_equal(params$mu, log(12))
    expect_true(params$sigma > 0)
  })
})

test_that("invalid quantiles render the message the user sees", {
  local_stub_cookies()
  shiny::testServer(server_fn, {
    do.call(session$setInputs, base_inputs)

    # Valid pair: no banner, and the Results panel is not an error message.
    expect_equal(output_text(output$mc_input_problem), "")
    expect_false(grepl("must be larger", output_text(output$risk_assessment)))

    # q90 < q50: the Results panel and the Monte Carlo tab both show the message.
    do.call(session$setInputs, with_inputs(q90 = 8))
    expect_match(output_text(output$risk_assessment),
                 "90% figure must be larger than the 50% figure")
    expect_match(output_text(output$mc_input_problem),
                 "Cannot run a simulation")
  })
})

test_that("Monte Carlo results are dropped when inputs go invalid", {
  local_stub_cookies()
  shiny::testServer(server_fn, {
    do.call(session$setInputs, base_inputs)

    # A successful run: the observer caches the simulation while the inputs
    # are valid - the two halves the mc_has_results gate reads.
    session$setInputs(mc_run = 1)
    expect_null(input_problem())
    expect_false(is.null(mc_results()))
    expect_false(is.null(mc_summary()))

    # Inputs go bad: the cached simulation is dropped and the tab says why,
    # instead of silently keeping the previous run on screen.
    session$setInputs(q90 = 8)
    expect_false(is.null(input_problem()))
    expect_true(is.null(mc_results()))
    expect_true(is.null(mc_summary()))
    expect_match(output_text(output$mc_input_problem),
                 "Cannot run a simulation")
    expect_match(output_text(output$risk_assessment),
                 "90% figure must be larger than the 50% figure")

    # Fixing the inputs must not resurrect results computed under old ones.
    session$setInputs(q90 = 23)
    expect_null(input_problem())
    expect_true(is.null(mc_results()))
    expect_equal(output_text(output$mc_input_problem), "")
  })
})

test_that("make_app_emailer() builds an emailer only when credentials exist", {
  # Missing either half -> NULL, which is what switches login_server() back to
  # its no-email mode (immediate sign-up, reset panel says not configured).
  expect_null(make_app_emailer(username = "", password = ""))
  expect_null(make_app_emailer(username = "a@example.com", password = ""))
  expect_null(make_app_emailer(username = "", password = "secret"))

  emailer <- make_app_emailer(username = "a@example.com", password = "secret")
  expect_true(is.function(emailer))
  # Signature login_server() calls: emailer(to_email =, subject =, message =)
  expect_named(formals(emailer), c("to_email", "subject", "message"))

  # The settings reach the underlying emayili server object (read from the
  # closure's environment rather than by sending a real email).
  env <- environment(emailer)
  expect_equal(env$email_host, "smtp.gmail.com")
  expect_equal(env$email_port, 465L)
  expect_equal(env$email_username, "a@example.com")
  expect_equal(env$from_email, "a@example.com")

  overridden <- make_app_emailer(username = "a@example.com", password = "secret",
                                 host = "mail.example.com", port = 587)
  expect_equal(environment(overridden)$email_host, "mail.example.com")
  expect_equal(environment(overridden)$email_port, 587L)
})

test_that("the app's emailer tracks GMAIL_USER / GMAIL_PASS", {
  configured <- nzchar(Sys.getenv("GMAIL_USER")) && nzchar(Sys.getenv("GMAIL_PASS"))
  expect_identical(!is.null(app_emailer), configured)
})

test_that("sign-up is gated on email verification being available", {
  expect_false(signup_enabled(NULL))
  expect_true(signup_enabled(function(...) invisible(NULL)))
  expect_identical(signup_enabled(app_emailer), !is.null(app_emailer))
})

test_that("ui.R only shows the create-account card when there is an emailer", {
  # ui.R decides at start-up, so render it twice with app_emailer overridden.
  render_ui <- function(emailer) {
    env <- new.env(parent = globalenv())
    env$app_emailer <- emailer
    source(file.path(app_root, "ui.R"), local = env)$value
  }

  with_email <- as.character(render_ui(function(...) invisible(NULL)))
  expect_match(with_email, "login-new_user_ui", fixed = TRUE)
  expect_false(grepl("Sign-up is disabled", with_email, fixed = TRUE))

  without_email <- as.character(render_ui(NULL))
  expect_false(grepl("login-new_user_ui", without_email, fixed = TRUE))
  expect_match(without_email, "Sign-up is disabled", fixed = TRUE)
  # Sign-in and password reset stay available either way.
  expect_match(without_email, "login-login_ui", fixed = TRUE)
  expect_match(without_email, "login-reset_password_ui", fixed = TRUE)
})

test_that("a sign-up submitted without an emailer creates no user row", {
  # Hiding the card in ui.R only stops honest clients: a crafted session can
  # post login-new_* inputs anyway, so the server-side backstop (verify_email =
  # TRUE in server.R) is what has to hold. Submit the form and check the
  # database afterwards.
  local_stub_cookies()
  no_email_server <- server_with_emailer(NULL)

  shiny::testServer(no_email_server, {
    probe <- "no-email-signup-test@example.com"
    do.call(session$setInputs, base_inputs)
    session$setInputs(
      `login-new_username` = probe,
      `login-new_password1` = "hunter2hunter2",
      `login-new_password2` = "hunter2hunter2"
    )

    # The handler runs and dies at the send step, printing a note. Capturing it
    # means the database assertions below cannot pass without the handler having
    # run at all. It is a warning rather than a message (the package calls
    # message(e) on the caught error), and testthat swallows that unless it is
    # handled explicitly here.
    sent <- character(0)
    withCallingHandlers(
      session$setInputs(`login-new_user` = 1),
      message = function(m) {
        sent <<- c(sent, conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        sent <<- c(sent, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_true(any(grepl("email", sent, ignore.case = TRUE)),
                info = paste(utils::head(sent, 3), collapse = " | "))
    expect_equal(output_text(output[["login-new_user_message"]]), "")

    rows <- DBI::dbGetQuery(
      db_conn,
      "SELECT username FROM users WHERE lower(username) = lower(?)",
      params = list(probe)
    )
    activity <- DBI::dbGetQuery(
      db_conn,
      paste("SELECT username FROM users_activity WHERE lower(username) = lower(?)",
            "AND action = 'create_account'"),
      params = list(probe)
    )

    # Leave the database exactly as found, whatever the assertions below say.
    DBI::dbExecute(db_conn,
                   "DELETE FROM users WHERE lower(username) = lower(?)",
                   params = list(probe))
    DBI::dbExecute(db_conn,
                   "DELETE FROM users_activity WHERE lower(username) = lower(?)",
                   params = list(probe))

    expect_equal(nrow(rows), 0)
    expect_equal(nrow(activity), 0)
  })
})

test_that("card headings follow the panel, not the current step", {
  # The package swaps a panel's contents as a flow advances: the sign-up form
  # loses its "Create Account" button once a code has been requested, and both
  # flow panels then show only Resend/Submit. Sniffing button text gave those
  # states the "Sign in" heading, so the inputs are what identifies a panel.
  expect_equal(panel_title(list(
    div(textOutput("login-login_message")),
    textInput("login-username", "Email:", ""),
    passwdInput("login-password", "Password:", ""),
    checkboxInput("login-remember_me", "Remember me?", TRUE),
    actionButton("login-Login", "Login")
  )), "Sign in")

  expect_equal(panel_title(list(
    textInput("login-new_username", "Email:", ""),
    passwdInput("login-new_password1", "Password:", ""),
    passwdInput("login-new_password2", "Confirm Password:", ""),
    actionButton("login-new_user", "Create Account")
  )), "Create account")

  # Sign-up, code-entry state: no "Create Account" button left on screen.
  expect_equal(panel_title(list(
    textInput("login-new_user_code", "Enter the code from the email:", ""),
    actionButton("login-send_new_user_code", "Resend Code"),
    actionButton("login-submit_new_user_code", "Submit")
  )), "Create account")

  expect_equal(panel_title(list(
    textInput("login-forgot_password_email", "Email address: ", ""),
    actionButton("login-send_reset_password_code", "Send reset code")
  )), "Reset password")

  # Reset, code-entry state: "Send reset code" has become "Resend Code".
  expect_equal(panel_title(list(
    textInput("login-reset_password_code", "Enter the code from the email:", ""),
    actionButton("login-send_reset_password_code", "Resend Code"),
    actionButton("login-submit_reset_password_code", "Submit")
  )), "Reset password")

  # Reset, new-password state: only password fields and a "Reset Password" button.
  expect_equal(panel_title(list(
    passwdInput("login-reset_password1", "Enter new password:", ""),
    passwdInput("login-reset_password2", "Confirm new password:", ""),
    actionButton("login-reset_new_password", "Reset Password")
  )), "Reset password")
})

test_that("each panel keeps its heading while it renders", {
  local_stub_cookies()
  # A stub emailer makes this independent of the machine's GMAIL_* credentials:
  # without one login_server() never builds the reset card, so its
  # output-name heading would otherwise go untested on a credential-less machine.
  stub_server <- server_with_emailer(function(...) invisible(NULL))

  shiny::testServer(stub_server, {
    do.call(session$setInputs, base_inputs)

    # Here the heading comes from the output being rendered, not the inputs.
    # The login module namespaces its outputs, so they are addressed by their
    # session-level ids.
    expect_match(output_text(output[["login-login_ui"]]), "Sign in", fixed = TRUE)
    expect_match(output_text(output[["login-new_user_ui"]]),
                 "Create account", fixed = TRUE)
    expect_match(output_text(output[["login-reset_password_ui"]]),
                 "Reset password", fixed = TRUE)
  })
})

test_that("without an emailer the reset panel is a message, not a card", {
  local_stub_cookies()
  no_email_server <- server_with_emailer(NULL)

  shiny::testServer(no_email_server, {
    do.call(session$setInputs, base_inputs)

    reset <- output_text(output[["login-reset_password_ui"]])
    expect_match(reset, "Email server has not been configured", fixed = TRUE)
    expect_false(grepl("Reset password", reset, fixed = TRUE))

    # Sign-in is unaffected by the missing emailer.
    expect_match(output_text(output[["login-login_ui"]]), "Sign in", fixed = TRUE)
  })
})

# global.R opened a connection for this test process only; the running app has
# its own.
if (exists("db_conn", inherits = FALSE) && DBI::dbIsValid(db_conn)) {
  DBI::dbDisconnect(db_conn)
}
