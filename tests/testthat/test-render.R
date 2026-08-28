test_that("integer widths keep their names so they round trip", {
  lite <- sqlite()

  expect_equal(sqlr_render_type(sqlr_smallint(), lite), "SMALLINT")
  expect_equal(sqlr_render_type(sqlr_int(), lite), "INTEGER")
  expect_equal(sqlr_render_type(sqlr_bigint(), lite), "BIGINT")
})

test_that("foreign key targets are never schema-qualified", {
  out <- sqlr_render(
    sqlr_schema(
      "main",
      sqlr_table("users", sqlr_column("id", sqlr_int()), sqlr_primary_key("id")),
      sqlr_table(
        "orders",
        sqlr_column("id", sqlr_int()),
        sqlr_column("user_id", sqlr_int()),
        sqlr_foreign_key("user_id", "users", "id")
      )
    ),
    sqlite()
  )

  expect_match(paste(out, collapse = "\n"), "REFERENCES \"users\"")
  expect_no_match(paste(out, collapse = "\n"), "REFERENCES \"main\"")
})

test_that("the schema qualifies the index name, not the table", {
  out <- sqlr_render(
    sqlr_schema(
      "main",
      sqlr_table(
        "t",
        sqlr_column("id", sqlr_int()),
        sqlr_index("id", name = "t_idx")
      )
    ),
    sqlite()
  )

  expect_match(out[[2L]], "CREATE INDEX \"main\".\"t_idx\" ON \"t\"")
})

test_that("identity is refused rather than silently mis-rendered", {
  expect_error(
    sqlr_render(
      sqlr_table("t", sqlr_column("id", sqlr_bigint(), identity = sqlr_identity())),
      sqlite()
    ),
    "no identity columns"
  )
})
