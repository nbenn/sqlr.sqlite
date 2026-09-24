test_that("a rendered schema executes and reflects back identically", {
  con <- sqlite_con()
  on.exit(DBI::dbDisconnect(con))

  desired <- sqlr_schema(
    "main",
    sqlr_table(
      "users",
      sqlr_column("id", sqlr_bigint(), null = FALSE),
      sqlr_column("email", sqlr_varchar(255), null = FALSE),
      sqlr_column("nickname", sqlr_text(), default = "anon"),
      sqlr_column("active", sqlr_boolean(), default = TRUE),
      sqlr_column("score", sqlr_numeric(10, 2), default = 0L),
      sqlr_primary_key("id"),
      sqlr_unique("email")
    ),
    sqlr_table(
      "orders",
      sqlr_column("id", sqlr_bigint(), null = FALSE),
      sqlr_column("user_id", sqlr_bigint(), null = FALSE),
      sqlr_column("total", sqlr_numeric(10, 2)),
      sqlr_column("placed", sqlr_date()),
      sqlr_primary_key("id"),
      sqlr_foreign_key("user_id", "users", "id", on_delete = "cascade"),
      sqlr_index("user_id", name = "orders_user_idx")
    )
  )

  for (stmt in sqlr_render(desired, sqlite())) {
    expect_error(DBI::dbExecute(con, stmt), NA)
  }

  expect_equal(sqlr_diff(desired, sqlr_reflect(con)), character())
})

test_that("a self-referencing foreign key survives the round trip", {
  con <- sqlite_con()
  on.exit(DBI::dbDisconnect(con))

  desired <- sqlr_schema(
    "main",
    sqlr_table(
      "employees",
      sqlr_column("id", sqlr_int(), null = FALSE),
      sqlr_column("manager_id", sqlr_int()),
      sqlr_primary_key("id"),
      sqlr_foreign_key("manager_id", "employees", "id")
    )
  )

  for (stmt in sqlr_render(desired, sqlite())) {
    expect_error(DBI::dbExecute(con, stmt), NA)
  }

  expect_equal(sqlr_diff(desired, sqlr_reflect(con)), character())
})

test_that("every type spelling the dialect renders reflects back as its type", {
  con <- sqlite_con()
  on.exit(DBI::dbDisconnect(con))

  types <- list(
    sqlr_integer_type(bytes = 1L), sqlr_smallint(), sqlr_int(), sqlr_bigint(),
    sqlr_real(), sqlr_double(),
    sqlr_numeric(), sqlr_numeric(10), sqlr_numeric(10, 2),
    sqlr_text(), sqlr_char(3), sqlr_varchar(255),
    sqlr_blob(), sqlr_boolean(),
    sqlr_date(), sqlr_time(), sqlr_timestamp(),
    sqlr_json(), sqlr_json(binary = TRUE), sqlr_uuid(),
    sqlr_other("GEOMETRY")
  )
  columns <- lapply(seq_along(types), function(i) {
    sqlr_column(paste0("x", i), types[[i]])
  })
  desired <- sqlr_schema("main", do.call(sqlr_table, c(list("t"), columns)))

  for (stmt in sqlr_render(desired, sqlite())) {
    expect_error(DBI::dbExecute(con, stmt), NA)
  }

  expect_equal(sqlr_diff(desired, sqlr_reflect(con)), character())
})
