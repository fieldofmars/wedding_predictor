# seed_user.R
.libPaths(c(paste0(Sys.getenv("USERPROFILE"), "/Documents/R/win-library/4.6"), .libPaths()))
library(digest)
library(DBI)
library(RSQLite)

h <- digest("test", algo = "sha512", serialize = FALSE)
db_conn <- dbConnect(SQLite(), "users.sqlite")

# Clear existing users
dbExecute(db_conn, "DELETE FROM users")

# Insert test user
dbExecute(db_conn, sprintf(
  "INSERT INTO users (username, password, created_date) VALUES ('test@example.com', '%s', %f)",
  h, as.numeric(Sys.time())
))

dbDisconnect(db_conn)
print("User test@example.com seeded with password 'test'")
