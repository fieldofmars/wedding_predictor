# Wedding Predictor — Visa Processing & Wedding Scheduling Planner

A small [R Shiny](https://shiny.posit.co/) web application that helps a couple choose a
wedding ceremony date when one partner's visa processing time is uncertain.

You tell the app:

* when the visa application was lodged,
* the published processing-time statistics (how long 50% / 90% of visas take),
* what a wedding booking costs and what a reschedule fee costs,
* how impatient you are (1 = "marry ASAP, accept the risk" → 10 = "happy to wait, be safe").

It answers:

* **how risky** a given ceremony date is (LOW → VERY HIGH risk band),
* the **probability of each outcome** (visa in time / reschedule / window expired),
* the **expected cost** of each choice,
* and shows all of the underlying maths, plus an optional Monte Carlo simulation that
  reproduces the same answers empirically.

Everything runs locally in R; there is no external API and no backend other than a small
SQLite file used for login.

---

## Table of contents

1. [The model in plain English](#1-the-model-in-plain-english)
2. [Repository layout](#2-repository-layout)
3. [How the app is wired (architecture)](#3-how-the-app-is-wired-architecture)
4. [Getting started](#4-getting-started)
5. [Logging in (authentication)](#5-logging-in-authentication)
6. [Running the tests](#6-running-the-tests)
7. [Deploying](#7-deploying)
8. [Handover notes: known issues and gotchas](#8-handover-notes-known-issues-and-gotchas)
9. [Where to change things next](#9-where-to-change-things-next)

---

## 1. The model in plain English

### 1.1 The problem

Visa processing time is a random variable: you lodge the application, then you wait an
unknown number of months for the visa to be granted. You would like to book a wedding
ceremony, but if the visa has not arrived by the ceremony date the booking has to be
rescheduled — and eventually, if the visa still has not arrived, the original booking is
lost entirely and you must pay for a new one.

So there is a trade-off:

* **earlier ceremony** = you get married sooner, but a higher chance of a costly
  reschedule (or a rebooking),
* **later ceremony** = you are very likely to be fine, but you wait longer.

### 1.2 The processing-time distribution

The app models visa processing time `T` (in **months after lodgement**) as a
**log-normal** random variable:

```
T ~ LogNormal(mu, sigma)        ln(T) ~ Normal(mu, sigma²)
```

A log-normal is the standard choice for "duration until an event" data: it is strictly
positive, right-skewed, and has no inconvenient upper bound.

Two published quantiles pin down the two parameters (see
`fit_lognormal_from_quantiles()` in [R/functions.R](R/functions.R)):

```
q50 = number of months by which 50% of visas are processed  (the median)
q90 = number of months by which 90% of visas are processed

mu    = ln(q50)
sigma = (ln(q90) - mu) / Phi⁻¹(0.9)          Phi⁻¹(0.9) = 1.281552
```

Why this works: for a log-normal, the median is exactly `exp(mu)`, so `mu = ln(q50)`
falls out directly; the 90th percentile is `exp(mu + sigma·Phi⁻¹(0.9))`, so one
rearrangement gives `sigma`.

Defaults in the UI are `q50 = 12` and `q90 = 23` months, which gives
`mu ≈ 2.4849` and `sigma ≈ 0.5077`.

The CDF (probability the visa is granted by month `t`) and PDF are thin wrappers over
R's built-ins:

```
F(t) = P(T <= t) = plnorm(t, mu, sigma)      # visa_cdf()
f(t) = dlnorm(t, mu, sigma)                  # visa_pdf()
Q(p) = exp(mu + sigma * qnorm(p))            # quantile / find_optimal_d1()
```

### 1.3 The three scenarios

The ceremony is booked for `d1` months after lodgement. Once you know `d1`, exactly one
of three mutually exclusive and exhaustive outcomes occurs (this is `scenario_analysis()`):

| # | Condition on grant time `T` | Meaning | Total cost (cumulative) |
|---|------------------------------|---------|--------------------------|
| 1 | `T <= d1` | Visa granted in time | `B` |
| 2 | `d1 < T <= d1 + 12` | Visa arrives inside the 12-month reschedule window | `B + R` |
| 3 | `T > d1 + 12` | Window expired; the original booking is lost and you rebook | `B + R + B` |

where `B` = booking cost (default `$400`) and `R` = reschedule fee (default `$200`).

Probabilities are straight CDF differences:

```
P1 = F(d1)
P2 = F(d1 + 12) - F(d1)
P3 = 1 - F(d1 + 12)
P1 + P2 + P3 = 1
```

Costs are **cumulative** — money already spent is never refunded — so the expected cost is

```
E[cost] = P1·B + P2·(B + R) + P3·(B + R + B)
```

The 12-month window is a hard-coded business rule (see `d1 + 12` in
[R/functions.R](R/functions.R) and in the plots in [server.R](server.R)).

### 1.4 Impatience ↔ date, and the risk bands

The "impatience" slider (1–10) is a proxy for the confidence you want in the visa
arriving before the ceremony:

```
target_prob = 0.45 + (impatience / 10) × 0.50
```

so impatience `1 → 50%` and impatience `10 → 95%` (step: 5 percentage points per unit).

* **Slider → date:** take the quantile of the log-normal at `target_prob`,
  `d1 = qlnorm(target_prob, mu, sigma)` (this is `find_optimal_d1()`), round to whole
  months and add to the lodgement date.
* **Date → slider (the reverse):** compute the confidence implied by the chosen date,
  `conf = F(d1)`, then `impatience = round((conf - 0.45) / 0.05)` clamped to `[1, 10]`.

The confidence `F(d1)` is also what drives the coloured risk banner:

| Confidence `F(d1)` | Risk band |
|--------------------|-----------|
| ≥ 90% | LOW RISK (green) |
| ≥ 75% | MODERATE RISK (blue) |
| ≥ 60% | ELEVATED RISK (amber) |
| ≥ 45% | HIGH RISK (orange) |
| < 45% | VERY HIGH RISK (red) |

### 1.5 Month arithmetic

There are two different month conversions in the code — be aware of both:

* **date → months** (used for computing `d1` from the two date pickers):
  `days / 30.4375` (the average Gregorian month),
* **months → date** (used when the slider writes a new ceremony date):
  `seq(from = lodgement, by = "1 month", length.out = n)`.

They are close but not identical, and the slider path rounds `d1` to a whole month.

### 1.6 The Monte Carlo tab

The Results tab uses exact closed-form probabilities. The Monte Carlo tab does the same
thing by brute force: `run_monte_carlo()` draws `n` samples with `rlnorm()`, classifies
each into one of the three scenarios, assigns the corresponding cumulative cost, and
`summarise_monte_carlo()` produces proportions, means, standard deviations, standard
error and empirical 95% intervals. The tab then shows the simulated results next to the
closed-form values so you can watch the estimates converge as `n` grows (Law of Large
Numbers). An optional seed makes runs reproducible (`0` in the UI means "random").

---

## 2. Repository layout

This is **not** an R package — there is no `DESCRIPTION`, no `NAMESPACE` and no `renv.lock`.
It is a conventional multi-file Shiny app: Shiny looks for `global.R`, `ui.R` and
`server.R` in the app directory and loads them automatically.

| Path | Purpose |
|------|---------|
| [global.R](global.R) | Runs **once per R process**, before any user connects. Loads all packages (`pacman::p_load` auto-installs anything missing), opens the SQLite connection `db_conn`, and `source()`s the business logic. |
| [ui.R](ui.R) | The static page layout: the sign-in / sign-up / reset-password cards, the sidebar of inputs, and the three tabs (Results / Monte Carlo / Show Working). Pure HTML-generating R code; no calculations. |
| [server.R](server.R) | All the reactive logic and rendering: slider↔date synchronisation, the risk panel, the two Results plots, the whole Monte Carlo tab, and the "Show Working" text. This is the biggest file (~1,000 lines). |
| [R/functions.R](R/functions.R) | **All pure business logic**, with roxygen-style documentation: `fit_lognormal_from_quantiles()`, `visa_cdf()`, `visa_pdf()`, `scenario_analysis()`, `find_optimal_d1()`, `metrics_grid()`, `run_monte_carlo()`, `summarise_monte_carlo()`. No Shiny code here — which is what makes it unit-testable. |
| [tests/testthat/test_functions.R](tests/testthat/test_functions.R) | 26 `test_that()` blocks covering the pure functions above (82 expectations). |
| [tests/testthat/test_server.R](tests/testthat/test_server.R) | Server-level tests: drives the real `server.R` through `shiny::testServer()` and checks that invalid quantiles surface as the friendly message (20 expectations). |
| [seed_user.R](seed_user.R) | Standalone script that creates (or refreshes) one test account — `test@example.com` / `test` — leaving all other users untouched. |
| [shiny_run.ps1](shiny_run.ps1) | Windows PowerShell launcher: sets the library path and starts the app on port 8100. |
| [users.sqlite](users.sqlite) | The login database (`users` + `users_activity` tables). **Git-ignored** (`*.sqlite` in [.gitignore](.gitignore)) — it will not be present on a fresh clone. |
| [rsconnect/shinyapps.io/grandprixlegends/Wedding_Predictor.dcf](rsconnect/shinyapps.io/grandprixlegends/Wedding_Predictor.dcf) | Deployment record: which shinyapps.io account, app name and app id this directory was last deployed to. |
| [wedding_predictor.Rproj](wedding_predictor.Rproj) | RStudio project file (UTF-8, 2-space tabs). Opening this file opens the project in RStudio with the working directory already at the app root. |
| [.gitignore](.gitignore) | Ignores R session litter (`.Rhistory`, `.RData`, …), `.freebuff/` and `*.sqlite`. |
| `.freebuff/` | Local tooling metadata for the Freebuff editor. Not part of the app; ignored by git. |

**Source files use Windows (CRLF) line endings** and there is no `.gitattributes`. If you
work across operating systems you may want to add one so line endings stay consistent.

---

## 3. How the app is wired (architecture)

### 3.1 Shiny lifecycle (for readers new to Shiny)

Shiny apps are **reactive**: instead of a script that runs top to bottom, you register
functions that Shiny re-runs automatically whenever their inputs change.

1. `global.R` is evaluated once when the app process starts.
2. `ui.R` is evaluated for every page load and describes what the page looks like.
3. `server.R`'s `shinyServer(function(input, output, session) { ... })` runs **once per
   connected user**, giving that user their own private copy of all the reactives.

Key vocabulary used below:

* **`input$...`** — a value from a UI control (e.g. `input$q50`).
* **`reactive({...})`** — a lazily recomputed value; it only re-runs when something it
  reads changes, and everyone reading it gets the same cached result.
* **`observeEvent(x, {...})`** — run code when `x` changes.
* **`output$... <- renderX({...})`** — produce a value for a UI placeholder
  (`uiOutput`, `renderUI`, `plotOutput`/`renderPlot`, `verbatimTextOutput`/`renderPrint`).
* **`reactiveVal(x)`** — a manually settable value you can store state in.

### 3.2 The reactive data flow

```
inputs: q50, q90
    └─> dist_params()        fit_lognormal_from_quantiles()  → list(mu, sigma)

inputs: lodgement_date, ceremony_date_pick
    └─> ceremony_months()    days / 30.4375                  → d1 (months)

dist_params + ceremony_months + booking_cost + resched_cost
    └─> summary_data()       scenario_analysis()             → list(mu, sigma, d1_opt,
                                                   target_prob, scenarios, ceremony_date,
                                                   costs...)

summary_data ──> output$risk_assessment   (risk banner + scenario table + E[cost])
             ──> output$tradeoff_plot      (stacked area of P1/P2/P3 vs. ceremony date)
             ──> output$dist_plot          (CDF with ceremony + window-end markers)
             ──> observeEvent(input$mc_run)  → run_monte_carlo() → mc_results/mc_summary
             ──> output$working_*          (the five "Show Working" text panels)
```

`summary_data()` is the single source of truth for "the current answer". If you add an
input, the safest pattern is to fold it into `summary_data()` and let everything else
read from there.

### 3.3 The bidirectional slider ↔ date sync (trickiest code in the repo)

The UI has two controls that mean the same thing: the **impatience slider** and the
**ceremony date picker**. Changing one must update the other — but naive two-way
observers would trigger each other forever.

The guard is a `reactiveVal` called `sync_source` in [server.R](server.R) with three
values (`"slider"`, `"date"`, `"none"`):

* when the slider changes, if `sync_source()` is already `"date"` we know *this* change
  was caused by our own date update → reset to `"none"` and return without acting;
* otherwise compute the new date, set `sync_source("slider")`, and push the value with
  `updateDateInput()`;
* the date observer does the mirror image.

**If you add a third control that updates either of these two, you must participate in
this protocol or you will create an infinite loop.** (`session$freezeReactiveValue()` is
the more idiomatic modern alternative if you ever want to refactor.)

Note that `summary_data()` treats `input$ceremony_date_pick` as the truth; the slider is
essentially a convenience control.

### 3.4 State that survives re-renders

Monte Carlo results are expensive to compute, so they are cached in three `reactiveVal`s
(`mc_results`, `mc_summary`, `mc_closed_form`) and only overwritten when **Run
Simulation** is pressed. `output$mc_has_results` is registered with
`outputOptions(..., suspendWhenHidden = FALSE)` so the `conditionalPanel` in the UI can
see it before the tab is opened.

The simulation itself runs **synchronously inside the Shiny session**, so a very large
`n_sims` will block that one user's session (and the R process) until it finishes. The
default 10,000 and the 100,000 maximum are both fine; anything heavier should be moved to
`future`/promises.

### 3.5 Presentation vs. logic

`ui.R` and `server.R` contain a lot of hand-written inline HTML/CSS (the risk banner, the
comparison table, the formula reference box). That is deliberate but means **styling
changes are made by editing string literals in `server.R`**, not CSS files.

---

## 4. Getting started

### 4.1 Prerequisites

* **R ≥ 4.6** (the project was developed and is run against R 4.6.1 on Windows; nothing
  in the code is version-specific).
* **RStudio** (optional but recommended — open [wedding_predictor.Rproj](wedding_predictor.Rproj)).
* Network access **on the first run**: [global.R](global.R) uses `pacman::p_load()`,
  which silently installs any package that is missing.
* Packages used: `pacman`, `shiny`, `tidyverse` (dplyr/tidyr/ggplot2/tibble), `login`,
  `shinyjs`, `DBI`, `RSQLite`; plus `testthat` and `digest` for tests/seeding.

> There is no dependency lockfile, so different machines may get different package
> versions. See [known issues](#8-handover-notes-known-issues-and-gotchas).

### 4.2 Run it (any OS)

From the project root:

```r
install.packages("pacman")          # the one package you must install by hand
shiny::runApp(".", port = 8100, launch.browser = TRUE)
```

or in RStudio: open the project and click **Run App**.

### 4.3 Run it (Windows PowerShell helper)

```powershell
.\shiny_run.ps1
```

The script locates `Rscript.exe` itself, in this order:

1. `C:\Program Files (x86)\R\R-4.6.1\bin\Rscript.exe`
2. `C:\Program Files\R\R-4.6.1\bin\Rscript.exe`
3. whatever `Rscript.exe` is on your `PATH`

and exits with a clear error if none is found. It then starts the app at
<http://127.0.0.1:8100> **without** opening a browser, using R's own default user
library (it deliberately does **not** set `R_LIBS_USER` — see
[known issues](#8-handover-notes-known-issues-and-gotchas) for why that breaks things).

If you install a different R version, update the candidate list at the top of
[shiny_run.ps1](shiny_run.ps1) — or just make sure `Rscript` is on `PATH`, since that is
the fallback.

### 4.4 First-run tour

1. You land on the sign-in screen: a **Sign in** card, a **Create account** card beside
   it and a **Reset password** card underneath (the main app is `display: none` until you
   authenticate).
2. Log in (see below) — the whole `#login_screen` block disappears and the planner
   appears.
3. **Results tab**: risk banner + scenario table + expected cost, then the stacked-area
   "scenario probabilities by ceremony date" chart, then the log-normal CDF chart with
   your ceremony date (red) and the end of the 12-month window (orange).
4. **Monte Carlo tab**: choose runs/seed, press **Run Simulation**, compare simulated vs.
   closed-form numbers.
5. **Show Working tab**: the full derivation, five steps, printed as text — useful
   when checking a change you made to the maths.

---

## 5. Logging in (authentication)

Authentication is provided by the CRAN package
[`login`](https://cran.r-project.org/package=login) (source:
[jbryer/login](https://github.com/jbryer/login)). It is used in the standard module way —
**the same module id `"login"` must be used everywhere**:

```r
# ui.R
login_ui(id = "login")
new_user_ui(id = "login")
reset_password_ui(id = "login")
...
logout_button(id = "login")

# server.R
USER <- login_server(
  id = "login",
  db_conn = db_conn,
  emailer = app_emailer,      # built in global.R; NULL => no-email fallback
  enclosing_panel = login_card
)
```

`USER` is a `reactiveValues()` with `logged_in` and `username`. A small observer in
[server.R](server.R) shows/hides `#main_app` and `#login_screen` using `shinyjs` — the
wrapper [ui.R](ui.R) puts around all three panels, so they disappear together on login.

**The panels are bslib cards.** The package renders its inputs on the server and wraps
them in `enclosing_panel` (default: `shiny::wellPanel()`); we pass `login_card` from
[global.R](global.R). One function serves all three panels, so it derives its heading
from the panel's contents — "Sign in", "Create account" or "Reset password" — rather
than hard-coding one title. Because this app runs **Bootstrap 3.4.1** (Shiny's classic
`fluidPage` default) while cards are a Bootstrap 5 component, [ui.R](ui.R) also injects a
small CSS block that styles `.bslib-card`. The panels sit in a `max-width: 900px`
wrapper: sign-in and sign-up side by side at ≥768px, the reset card full width below.

### 5.1 The database

`global.R` opens one connection for the whole process:

```r
db_conn <- dbConnect(RSQLite::SQLite(), "users.sqlite")
```

`login_server()` creates the tables on first run if they do not exist:

* `users(username, password, created_date)`
* `users_activity(username, action, timestamp)` — an audit log of logins/logouts/cookies

Because `users.sqlite` is git-ignored, **a fresh clone has no database until you start
the app once** (the tables are then created empty).

### 5.2 How passwords are stored

* The browser hashes the typed password with **MD5** in JavaScript before it leaves the
  browser, so the Shiny server never sees the plaintext.
* `login_server()` is called without a `salt`, so `get_password()` returns that value
  unchanged and **the stored password is simply `md5(password)`** (32 hex characters).
* To seed a user manually you must store the same digest:

  ```r
  digest::digest("test", algo = "md5", serialize = FALSE)
  # "098f6bcd4621d373cade4e832627b4f6"
  ```

> ✅ [seed_user.R](seed_user.R) follows exactly this rule (`algo = "md5"`) and is
> non-destructive: it creates the `users` table if missing and replaces only the demo
> account's own row. Run `Rscript seed_user.R` from the project root to (re)create
> `test@example.com` / `test`. **Do not "upgrade" the hash to a stronger algorithm** —
> it has to match what the browser sends.

### 5.3 Email, sign-up and password reset

Sign-up and password reset are wired up, and both depend on Gmail SMTP.

* **Sign-up form.** [ui.R](ui.R) renders `new_user_ui(id = "login")`, so next to the
  sign-in card there is a **Create account** card (email, password, confirm). The account
  row is only inserted into `users` after the emailed code is typed in.
* **Email** is built in [global.R](global.R) by `make_app_emailer()`, which reads the
  Windows *user* environment variables:

  | Variable | Meaning | Default |
  |----------|---------|---------|
  | `GMAIL_USER` | Gmail address; also used as the From address | — (required) |
  | `GMAIL_PASS` | 16-character Gmail **App Password**, not your account password | — (required) |
  | `GMAIL_HOST` | SMTP host | `smtp.gmail.com` |
  | `GMAIL_PORT` | SMTP port | `465` |

  Set them once per machine:

  ```powershell
  setx GMAIL_USER "you@gmail.com"
  setx GMAIL_PASS "abcdefghijklmnop"   # App Password: Google Account -> 2-Step Verification -> App passwords
  ```

  **Restart the app and any open terminal afterwards** — `setx` only affects processes
  started after it returns.

  Passing an `emailer` is what switches verification on: `login_server()` defaults to
  `verify_email = !is.null(emailer)`, so the flow becomes *request account → email with a
  6-digit code → enter the code → row inserted*, and the **Reset password** card sends a
  code the same way. The email body is the package's own default text (we no longer pass
  `create_account_message`).
* **No credentials in the repo.** If either variable is missing, `make_app_emailer()`
  returns `NULL`, [server.R](server.R) passes that straight through, and the app falls
  back to the old no-email mode: sign-ups accepted immediately, the reset card printing
  *"Email server has not been configured."*, and a `message()` printed at startup. The
  function is covered by `tests/testthat/test_server.R` without ever sending a mail.
* **Cookies: still plaintext.** "Remember me" writes a `loginusername` cookie for 30
  days holding the **plain username**, readable by JavaScript, cleared on logout. The
  installed versions of `cookies`/`login` expose no password or encryption option at all
  (`cookies::set_cookie()` has no such argument — older docs mentioning a
  `cookie_password` do not apply here), so there is nothing to configure. To shorten the
  window, pass `cookie_expiration = <days>` to `login_server()`.
* **No HTTPS locally.** shinyapps.io serves over HTTPS; anything self-hosted should be
  put behind a TLS-terminating reverse proxy.

---

## 6. Running the tests

The suite lives in two files under `tests/testthat/`:

**[test_functions.R](tests/testthat/test_functions.R)** (26 tests, 82 expectations)
checks the pure business logic in `R/functions.R`:

* `fit_lognormal_from_quantiles()` — valid, positive `sigma`;
* `visa_cdf()` — bounded in `[0,1]`, equals 0.5 at `q50` and 0.9 at `q90`;
* `scenario_analysis()` — probabilities sum to 1, cumulative costs are
  `B / B+R / B+R+B` for default and custom values, monotonicity, and the
  very-early / very-late edge cases;
* `find_optimal_d1()` — round-trips the quantile, monotone in `target_prob`;
* `metrics_grid()` — expected columns, row probabilities sum to 1, fixed costs;
* `run_monte_carlo()` — output structure, correct scenario classification, costs match
  scenarios, identical results for an identical seed, and convergence to the closed form
  at 50,000 runs;
* `summarise_monte_carlo()` — structure, counts sum to `n_sims`, CIs are ordered;
* `quantile_problem()` — every rejected pair (`q90 < q50`, `q90 == q50`,
  non-positive, missing, `Inf`) and the accepted ones.

**[test_server.R](tests/testthat/test_server.R)** (20 expectations) covers the Shiny
wiring those helper tests cannot: it sources `global.R` and `server.R`, then drives the
real server function with `shiny::testServer()` and asserts that

* `dist_params()` refuses `q90 <= q50` with a `shiny.silent.error` whose message is the
  friendly text the outputs render, and still fits a valid pair;
* the Results panel (`output$risk_assessment`) and the Monte Carlo banner
  (`output$mc_input_problem`) actually display that message, and stay quiet for a valid
  pair;
* a cached Monte Carlo run is dropped (not left on screen) the moment the inputs go
  invalid, and is not resurrected when they are fixed.

Two setup details are baked into that file, both worth knowing if you copy the pattern:
`cookies::get_cookie()` is stubbed because `login_server()` polls it on every flush and it
does not work under a mock session, and the date inputs are passed as `Date` objects
because that is what Shiny hands `server.R`, which feeds them into `seq(by = "1 month")`.

**There is no `DESCRIPTION` file and no `tests/testthat.R` runner**, so
`devtools::test()` / `R CMD check` will not work as-is. `test_functions.R` also assumes the
functions are already in the search path (it calls `fit_lognormal_from_quantiles()`
directly, and the functions use `%>%`, `tibble()` and `case_when()`); `test_server.R`
loads the app itself.

Run everything from the project root with:

```r
library(tidyverse)      # provides %>%, tibble, dplyr used inside R/functions.R
library(testthat)
source("R/functions.R")

test_dir("tests/testthat")    # both files: 102 expectations
# or a single file:
# test_file("tests/testthat/test_functions.R")
# test_file("tests/testthat/test_server.R")
```

The logic file still uses the legacy `context("...")` helper; it works, but it is
deprecated in testthat 3rd edition and would be the first thing to remove if you modernise
the tests.

---

## 7. Deploying

The app has been deployed to shinyapps.io. The deployment record is
[rsconnect/.../Wedding_Predictor.dcf](rsconnect/shinyapps.io/grandprixlegends/Wedding_Predictor.dcf):

| Field | Value |
|-------|-------|
| account / username | `grandprixlegends` |
| server | `shinyapps.io` |
| app name | `Wedding_Predictor` |
| appId | `16657556` |
| URL | <https://grandprixlegends.shinyapps.io/Wedding_Predictor/> |

To redeploy from the project root:

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(
  name   = "grandprixlegends",
  token  = "<token from shinyapps.io dashboard>",
  secret = "<secret from shinyapps.io dashboard>"
)
rsconnect::deployApp(appName = "Wedding_Predictor")
```

Things to check before deploying:

* **`users.sqlite` is git-ignored.** Review the file list `deployApp()` prints before it
  uploads; if the database is not included, the deployed instance starts with empty
  `users`/`users_activity` tables and nobody can log in until you seed it.
* Dependencies are declared only through `pacman::p_load(shiny)`-style calls in
  [global.R](global.R) with **unquoted** package names. `rsconnect` detects dependencies
  by static code analysis, so check the package list it prints before uploading and
  confirm everything you need is in it — install anything missing explicitly first
  (`install.packages("...")`) so it is present in your local library.
* The deploy is synchronous from your point of view but takes a few minutes; logs are in
  the shinyapps.io dashboard.

---

## 8. Handover notes: known issues and gotchas

Ordered roughly by how likely they are to bite you.

1. **Password hashes must be `md5`, not something "stronger".**
   Because no `salt` is configured, the `login` package stores the browser-supplied MD5
   digest verbatim. [seed_user.R](seed_user.R) originally stored `sha512("test")` (so the
   seeded login failed with *"Incorrect password"*) and started with a bare
   `DELETE FROM users` (destroying every account) — both are fixed: it now writes
   `digest("test", algo = "md5", ...)`, creates the table if missing, and deletes only
   the demo user's row. If you add your own user-creation code, keep the md5 rule.

2. **Do not force `R_LIBS_USER` / `.libPaths()` — there are two R user libraries.**
   R 4.6's default user library is `%LOCALAPPDATA%\R\win-library\4.6` (where `pacman`,
   `testthat`, `shiny`, … are actually installed). A legacy
   `%USERPROFILE%\Documents\R\win-library\4.6` also exists from older runs but is
   **incomplete** (no `pacman`), and setting `R_LIBS_USER` to it *replaces* the default
   rather than adding to it. That is exactly what this script and `seed_user.R` used to
   do, and the app then died at startup with `there is no package called 'pacman'`.
   Both overrides have been removed — if a new script needs the user library, just use
   R's defaults.

3. **`q90` must be greater than `q50` (now validated).**
   The UI constrains each input to `1..60` but not against each other, and a pair with
   `q90 <= q50` has no usable log-normal fit (sigma would be zero or negative).
   `quantile_problem()` in [R/functions.R](R/functions.R) checks the pair and
   `dist_params()` runs it through `validate()`/`need()`, so outputs show a plain-English
   message instead of a red Shiny error, and dependants (slider sync, Monte Carlo) halt
   silently the way they do with `req()`. `fit_lognormal_from_quantiles()` keeps its
   `stopifnot()` as a backstop for non-Shiny callers. **Follow the same pattern for any
   new input** — add a `*_problem()` helper, a `validate()` in the reactive that first
   reads it, and tests.
   The Monte Carlo tab is wired to the same check rather than only to the last successful
   run: `input_problem()` is a shared reactive, `output$mc_input_problem` renders a
   "Cannot run a simulation" banner the moment the pair goes bad (even before any run has
   ever happened), `output$mc_has_results` is gated on `is.null(input_problem())` so the
   `conditionalPanel` never shows a previous simulation against inputs the app now
   rejects, and an `observe()` clears the three cached MC `reactiveVal`s so fixing the
   inputs cannot resurrect stale results — the tab goes back to its empty "Run Simulation"
   state instead.

4. **Two different definitions of "a month".**
   Date → months uses `days / 30.4375`; months → date uses `seq(..., by = "1 month")` and
   rounds to whole months. Round-tripping slider → date → slider therefore drifts
   slightly and always lands on an integer month. Mostly harmless, but don't be surprised
   if the slider moves when you re-pick the "same" date.

5. **The 12-month reschedule window is hard-coded** in at least four places
   (`scenario_analysis()`, `metrics_grid()`, `run_monte_carlo()`, and the `d1 + 12`
   markers in [server.R](server.R)). If the business rule changes, grep for `+ 12`.

6. **One shared, never-closed database connection.** `db_conn` is opened in `global.R`
   and reused by every session for the life of the process; nothing ever calls
   `dbDisconnect()`. Fine for a Shiny process (it dies with the process), but don't
   copy this pattern into scripts — and be aware that two R processes in the same
   directory will both write to `users.sqlite` (SQLite allows it, with locking).

7. **No dependency pinning.** No `renv.lock`, no `DESCRIPTION`, and `pacman::p_load()`
   installs *whatever version is current* on first run. If a future package release breaks
   the app, you have no record of what worked. Adding `renv::init()` is a cheap win.

8. **Monte Carlo runs synchronously** inside the session's observer. 100,000 runs is
   still fast, but there is no progress indicator and no `future`/`promises`
   offloading — a heavier simulation would freeze the UI.

9. **Tests are not wired into any runner or CI.** No `tests/testthat.R`, no GitHub
   Actions, legacy `context()` API. Nobody is automatically told when a change breaks the
   business logic.

10. **Auth is still demo-grade.** Email verification and password reset now work (see
    §5.3), but passwords are unsalted MD5 hashed in the browser, the remember-me cookie
    holds a plain username, sign-up is open to anyone who can reach the app, and the user
    table is a single flat SQLite file. Treat it as a demo layer: if the app ever holds
    real personal data, put it behind HTTPS + a real identity provider.

11. **`output$mc_has_results` must stay registered**
    with `outputOptions(..., suspendWhenHidden = FALSE)`; if you remove that line the
    Monte Carlo results panel silently never appears.

12. **CRLF line endings, no `.gitattributes`, and a casual commit history** (8 commits
    so far). Keep the existing style — 2-space indent, `##` header comments in
    `global.R`/`ui.R`/`server.R`, roxygen comments for everything in `R/functions.R`.

---

## 9. Where to change things next

| You want to… | Change… |
|--------------|---------|
| Adjust default costs / quantiles | The `value =` arguments in [ui.R](ui.R) (e.g. `booking_cost = 400`, `q50 = 12`); the defaults in `scenario_analysis()` / `metrics_grid()` / `run_monte_carlo()` in [R/functions.R](R/functions.R) should be kept in sync. |
| Move the risk thresholds | The `if/else if` ladder over `conf` in `output$risk_assessment` ([server.R](server.R)). |
| Change the impatience → confidence mapping | `impatience_to_confidence()` and `confidence_to_impatience()` in [server.R](server.R) **together** (they must stay inverses), plus the formula text in `output$working_formulas`. |
| Change the 12-month window | Grep for `+ 12` across `R/functions.R` and `server.R`. |
| Add a new input | Add the control in [ui.R](ui.R), fold it into `summary_data()` in [server.R](server.R), and let existing outputs read it from there. |
| Add a new output/tab | A `tabPanel(...)` in [ui.R](ui.R) plus a matching `output$...` in [server.R](server.R). |
| Change or extend the maths | [R/functions.R](R/functions.R) first (keep it Shiny-free), then add tests in [tests/testthat/test_functions.R](tests/testthat/test_functions.R). |
| Change input validation or its wiring | `quantile_problem()` in [R/functions.R](R/functions.R) plus the `validate()` call in `dist_params()` in [server.R](server.R), then extend [tests/testthat/test_server.R](tests/testthat/test_server.R) so the rendered message is covered too. |
| Rebrand the app | `titlePanel(...)` in [ui.R](ui.R) and the `subtitle`/`title` strings in the ggplot `labs()` calls. |
| Turn email verification on/off | Set or unset `GMAIL_USER` / `GMAIL_PASS` (§5.3); `make_app_emailer()` in [global.R](global.R) decides whether `login_server()` receives an emailer. |
| Change the sign-up / reset labels | `username_label`, `password_label`, `create_account_label` arguments of `login_server()` — they also drive the card headings `login_card()` picks. |

### Useful commands

```r
shiny::runApp(".", port = 8100)                       # run the app
testthat::test_dir("tests/testthat")          # both test files (102 expectations)
?login::login_server                                   # auth package reference
sessionInfo()                                          # report versions when filing a bug
```
