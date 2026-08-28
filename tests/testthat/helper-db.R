sqlite_con <- function() {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  con
}
