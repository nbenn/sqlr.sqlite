type_from_declared <- function(declared) {
  args <- suppressWarnings(
    as.integer(regmatches(declared, gregexpr("-?[0-9]+", declared))[[1L]])
  )
  shape <- toupper(
    gsub("\\s*([(),])\\s*", "\\1", gsub("-?[0-9]+", "#", declared))
  )

  type <- if (!anyNA(args)) {
    switch(
      shape,
      "TINYINT" = sqlr_integer_type(bytes = 1L),
      "SMALLINT" = sqlr_smallint(),
      "INTEGER" = sqlr_int(),
      "BIGINT" = sqlr_bigint(),
      "REAL" = sqlr_real(),
      "DOUBLE" = sqlr_double(),
      "NUMERIC" = sqlr_numeric(),
      "NUMERIC(#)" = sqlr_numeric(args[[1L]]),
      "NUMERIC(#,#)" = sqlr_numeric(args[[1L]], args[[2L]]),
      "TEXT" = sqlr_text(),
      "CHAR(#)" = sqlr_char(args[[1L]]),
      "VARCHAR(#)" = sqlr_varchar(args[[1L]]),
      "BLOB" = sqlr_blob(),
      "BOOLEAN" = sqlr_boolean(),
      "DATE" = sqlr_date(),
      "TIME" = sqlr_time(),
      "TIMESTAMP" = sqlr_timestamp(),
      "JSON" = sqlr_json(),
      "JSONB" = sqlr_json(binary = TRUE),
      "UUID" = sqlr_uuid()
    )
  }

  if (is.null(type)) {
    return(sqlr_other(declared, raw = declared))
  }

  type@raw <- declared
  type
}
