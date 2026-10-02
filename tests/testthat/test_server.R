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

# global.R opened a connection for this test process only; the running app has
# its own.
if (exists("db_conn", inherits = FALSE) && DBI::dbIsValid(db_conn)) {
  DBI::dbDisconnect(db_conn)
}
