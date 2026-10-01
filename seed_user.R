# seed_user.R
# Creates (or refreshes) the demo account used for local testing.
#
# Password hashing: the `login` package hashes the password in the BROWSER with
# MD5 before it reaches the server, and this app configures no salt, so the value
# stored in the `users` table MUST be:
#
#   digest(password, algo = "md5", serialize = FALSE)
#
# Anything else (e.g. sha512) will not match and login fails with
# "Incorrect password."
#
# This script is non-destructive: it only touches the demo account's own row.

# Uses R's default .libPaths() — no hard-coded library directories.
library(digest)
library(DBI)
library(RSQLite)

username <- "test@example.com"
password <- "test"
hash     <- digest(password, algo = "md5", serialize = FALSE)

db_conn <- dbConnect(SQLite(), "users.sqlite")
on.exit(dbDisconnect(db_conn), add = TRUE)

# login_server() creates the table on first run; create it here as well so the
# script also works on a fresh clone before the app has ever been started.
if (!dbExistsTable(db_conn, "users")) {
  dbWriteTable(db_conn, "users", data.frame(
    username      = character(),
    password      = character(),
    created_date  = numeric(),
    stringsAsFactors = FALSE
  ))
}

# Remove only the demo account (never other users), then insert it fresh.
dbExecute(db_conn, sprintf(
  "DELETE FROM users WHERE username = '%s'", username
))
dbExecute(db_conn, sprintf(
  "INSERT INTO users (username, password, created_date) VALUES ('%s', '%s', %f)",
  username, hash, as.numeric(Sys.time())
))

cat(sprintf("Seeded %s with password '%s' (stored md5: %s)\n",
            username, password, hash))
cat(sprintf("Rows in users table: %d\n", dbGetQuery(db_conn, "SELECT COUNT(*) AS n FROM users")$n))
