test_that("the dialect's own spellings reflect in any case and spacing", {
  con <- sqlite_con()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(
    con,
    "CREATE TABLE t (a tinyint, b Numeric( 10 ,2 ), c varchar(20))"
  )

  desired <- sqlr_table(
    "t",
    sqlr_column("a", sqlr_integer_type(bytes = 1L)),
    sqlr_column("b", sqlr_numeric(10, 2)),
    sqlr_column("c", sqlr_varchar(20))
  )
  expect_equal(sqlr_diff(desired, sqlr_reflect(con)@tables[[1L]]), character())
})

test_that("any other declared type comes back verbatim as sqlr_other()", {
  con <- sqlite_con()
  on.exit(DBI::dbDisconnect(con))

  declared <- c(
    "INT", "DOUBLE PRECISION", "VARCHAR", "INTEGER(10)", "MediumInt",
    "VARCHAR(99999999999)", ""
  )
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE t (",
      paste0("x", seq_along(declared), " ", declared, collapse = ", "),
      ")"
    )
  )

  expect_equal(
    lapply(sqlr_reflect(con)@tables[[1L]]@columns, function(col) col@type),
    lapply(declared, function(x) sqlr_other(x, raw = x))
  )
})
